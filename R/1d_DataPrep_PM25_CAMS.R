# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Process the pm25 cams data
#'
#' This function implements a data preparation step to process the pm25 cams
#' data. It is useful only for its side effects, i.e. for saving the processed
#' data.
#'
#' @inheritParams get_config
#' @inheritParams run_workflow
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' run_data_prep_pm25_cams()
#' }
#'
run_data_prep_pm25_cams <- function(config = NULL, config_overrides = NULL) {

  config <- get_config(config, config_overrides)

  # Loading shapefiles
  ew_msoa <- readRDS("Data_act/Processed/Shapefiles/ew_msoa.rds")
  uk_full <- readRDS("Data_act/Processed/Shapefiles/uk_full.rds")

  #########################################
  ### Initial processing from NetCDF UK ###
  ### wide to tif files for Manchester  ###
  #########################################
  # Empty raster
  r <- raster::raster(xmn = -10,
                      xmx = 3,
                      ymn = 49,
                      ymx = 62,
                      res = 0.1)

  # Adding unique  ID
  r[] <- 1:(dim(r)[1]*dim(r)[2])

  # Empty dataset to append to
  weights <- NULL

  # Extracting values
  a1 <- raster::extract(r, # Grid unique IDs
                        uk_full, # Shapefiles
                        weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
                        small = TRUE) # Small areas in comparison to the raster

  # Setting to missing if not in UK
  r[!(r[] %in% a1[[1]])] <- NA

  # file names
  files <- c('Data_ref/Raw/PM25/CAMS-Europe/CAMSEurope_20201201-20210531.nc',
             'Data_ref/Raw/PM25/CAMS-Europe/CAMSEurope_20210601-20211130.nc',
             'Data_ref/Raw/PM25/CAMS-Europe/CAMSEurope_20211201-20220430.nc')

  # files start dates
  start_date <- c(as.Date('2020-12-01'),
                  as.Date('2021-06-01'),
                  as.Date('2021-12-01'))

  # Loop for each file
  for (i in 1:length(files)){
    # Opening raster
    ncin <- raster::raster(files[i],
                           band = 1,
                           verbose = FALSE,
                           stopIfNotEqualSpaced = FALSE)
    # Getting the number of  days
    N_days <- floor(raster::nbands(ncin)/24)
    # Dates
    Dates <- start_date[i] + (1:N_days) - 1
    # Loop for each day in the year
    for (j in 1:N_days){
      # Getting date
      date <- Dates[j]
      # Looping for each hour in the day
      for (k in (24*(j-1)+1):(24*j)){
        # Opening raster
        ncin <- raster::raster(files[i],
                               band = k,
                               verbose = FALSE,
                               stopIfNotEqualSpaced = FALSE)
        # # Cropping for the UK
        # ncin <- raster::crop(ncin, raster::extent(-10, 3, 49, 62))
        raster::extent(ncin) <- c(-10, 3, 49, 62)
        # Setting to missing if not in  UK
        ncin[is.na(r[])] <- NA
        # Saving raster
        raster::writeRaster(ncin,
                            filename = paste('Data_act/Processed/PM25/CAMS-Europe/PM25_', date, '-', sprintf("%02d", k %% 24), "00.tif", sep = ''),
                            overwrite = TRUE)
        # else {keep <- keep + ncin}
        print(paste(date, '-', sprintf("%02d", (k - 1) %% 24), "00", sep = ''))
      }
    }
  }

  #################################################
  ### Aggregating to MSOA and bringing together ###
  #################################################
  # Empty raster
  r0 <- raster::raster(xmn = -2.8,
                       xmx = -1.8,
                       ymn = 53.2,
                       ymx = 53.7,
                       res = 0.1)

  # Adding unique  ID
  r0[] <- 1:(dim(r0)[1]*dim(r0)[2])

  # Subsetting Greater Manchester shapefiles
  mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                      'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

  # Converting to long lat
  mcr_msoa <- mcr_msoa %>%
    sf::st_transform(sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

  # Extracting values
  a1 <- raster::extract(r0, # Grid unique IDs
                        mcr_msoa, # Shapefiles
                        weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
                        small = TRUE) # Small areas in comparison to the raster

  # Empty dataset to append to
  Weights_msoa <- NULL

  # Loop for each area
  for (i in 1:length(a1)){
    # Converting Weights_msoa to dataframes
    tmp <- as.data.frame(a1[[i]])
    # Removing NAs
    tmp <- subset(tmp, !is.na(value))
    # Reweighting the Weights_msoa after zeroes removed
    tmp$weight <- tmp$weight/sum(tmp$weight)
    # New Region Name
    tmp$area_id <- mcr_msoa$area_id[i]
    # Creating new dataset
    if (i == 1) {Weights_msoa <- tmp}
    else {Weights_msoa <- rbind(Weights_msoa, tmp)}
    # Removing unecessary data
    rm(tmp)
  }

  # Altering column names
  names(Weights_msoa)[1] <- c('IDGRID')

  # Merging to skeleton dataset
  pm25_cams <- NULL

  # Loop for each date
  for (i in as.character(seq(as.Date('2020-12-01'), as.Date('2021-07-31'), by = 1))){
    # Loop for each time
    for (j in 0:23){
      # Reading in PM25 from CAMS
      r <- raster::raster(paste('Data_act/Processed/PM25/CAMS-Europe/PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
      # Renaming raster
      names(r) <- 'pm25'
      # Creating aggregated estimates of PM25 by MSOA
      tmp1 <- r %>%
        # Converting raster to dataframe
        raster::crop(r0) %>%
        raster::stack(r0) %>%
        raster::rasterToPoints() %>%
        as.data.frame()%>%
        # Renaming columns
        dplyr::select(IDGRID = layer, pm25) %>%
        # Merging on weights to aggregate
        dplyr::right_join(Weights_msoa,
                          by = 'IDGRID') %>%
        # Aggregating grid to
        # Note: Using the .(area_id) syntax causes a name conflict with dplyr -
        # so use the formula syntax for the 'group by' var instead, i.e.
        # '~ area_id' rather than '.(area_id)'
        plyr::ddply(~ area_id,
                    plyr::summarize,
                    pm25_cams_agg = weighted.mean(pm25, weight))
      # Extracting PM2.5 values at centroids
      tmp1$pm25_cams_cent <- raster::extract(r, mcr_msoa[,c('cent_long', 'cent_lat')] %>% sf::st_drop_geometry())
      # Adding on date and hour
      tmp1 <- tmp1 %>%
        dplyr::mutate(hour = j,
                      date = i) %>%
        # Outputting datasets
        dplyr::select(area_id, date, hour, pm25_cams_cent, pm25_cams_agg)
      # Appending together
      pm25_cams <- rbind(pm25_cams, tmp1)
      # Removing uncessary datasets
      rm(tmp1)
      # Printing index
      print(paste('PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
    }
  }

  ######################
  ### Saving outputs ###
  ######################
  # Save cams
  saveRDS(pm25_cams, "Data_act/Processed/PM25/pm25_cams.rds")

  invisible()
}
