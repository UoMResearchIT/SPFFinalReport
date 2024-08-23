#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory
setwd('C:/Users/mcassag/Data/personal_exposure_model')

# Loading source code
source('C:/Users/mcassag/Code/Personal_Exposure_Model/SPFFinalReport/Code/CaseStudy2/0_Source.R')

# Loading shapefiles 
load("original/Processed/Shapefiles/shapefiles.RData")

#########################################
### Initial processing from NetCDF UK ###
### wide to tif files for Manchester  ###
#########################################
# Empty raster
r_uk <- raster(xmn = -10,
            xmx = 3,
            ymn = 49,
            ymx = 62,
            res = 0.1)

# Adding unique  ID
r_uk[] <- 1:(dim(r_uk)[1]*dim(r_uk)[2])

# Extracting values
a1_uk <- raster::extract(
  r_uk, # Grid unique IDs
  uk_full, # Shapefiles
  weight = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
  small = TRUE) # Small areas in comparison to the raster

# Setting to missing if not in UK
r_uk[!(r_uk[] %in% a1_uk[[1]])] <- NA

# file names
files <- c('original/Raw/PM25/CAMS-Europe/CAMSEurope_20201201-20210531.nc')
           #'original/Raw/PM25/CAMS-Europe/CAMSEurope_20210601-20211130.nc',
           #'original/Raw/PM25/CAMS-Europe/CAMSEurope_20211201-20220430.nc')

# files start dates
start_date <- c(as.Date('2020-12-01'),
                as.Date('2021-06-01'),
                as.Date('2021-12-01'))

# Loop for each file
for (i in 1:length(files)){
  # Opening raster
  ncin <- raster(files[i],
                 band = 1,
                 verbose = FALSE,
                 stopIfNotEqualSpaced = FALSE)
  # Getting the number of  days
  N_days <- floor(nbands(ncin)/24)
  # Dates
  Dates <- start_date[i] + (1:N_days) - 1
  # Loop for each day in the year
  for (j in 1:N_days){
    # Getting date
    date <- Dates[j]
    # Looping for each hour in the day
    for (k in (24*(j-1)+1):(24*j)){
      # Opening raster
      ncin <- raster(files[i],
                     band = k,
                     verbose = FALSE,
                     stopIfNotEqualSpaced = FALSE)
      # # Cropping for the UK
      # ncin <- crop(ncin, extent(-10, 3, 49, 62))
      extent(ncin) <- c(-10, 3, 49, 62)
      # Setting to missing if not in  UK
      ncin[is.na(r_uk[])] <- NA
      # Saving raster
      writeRaster(ncin,
                  filename = paste('original/Processed/PM25/CAMS-Europe/PM25_', date, '-', sprintf("%02d", k %% 24), "00.tif", sep = ''),
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
r_mcr <- raster(xmn = -2.8,
                xmx = -1.9, # Updated from -1.8
                ymn = 53.3, # Updated from 53.2
                ymx = 53.7,
                res = 0.1)

# Adding unique  ID
r_mcr[] <- 1:(dim(r_mcr)[1]*dim(r_mcr)[2])

# Subsetting Greater Manchester shapefiles
mcr_msoa <- subset(ew_msoa, parent_area_name %in% c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale',
                                                    'Salford', 'Stockport', 'Tameside', 'Trafford', 'Wigan'))

# Converting to long lat
mcr_msoa <- mcr_msoa %>%
  st_transform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Extracting values
a1_msoa <- raster::extract(
  r_mcr, # Grid unique IDs 
  mcr_msoa, # Shapefiles
  weights = TRUE, # Give us Weights of the cells so we can do a weighted average of the cells we overlap
  small = TRUE) # Small areas in comparison to the raster

# Empty dataset to append to
Weights_msoa <- NULL

# Loop for each area
for (i in 1:length(a1_msoa)){
  # Converting Weights_msoa to dataframes
  tmp <- as.data.frame(a1_msoa[[i]])
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

# Save weights
save(Weights_msoa, file = "original/Processed/PM25/Weights/weights_cams_MSOA.RData")

# Merging to skeleton dataset
pm25_cams <- NULL


#### TEMP DEBUG ########

r_file_in <- raster("original/Processed/PM25/CAMS-Europe/PM25_2021-01-01-0100.tif")
# Renaming raster
names(r_file_in) <- 'pm25'
# Creating aggregated estimates of PM25 by MSOA
tmp1 <- r_file_in %>%
  # Converting raster to dataframe
  crop(r_mcr)
tmp1 <- tmp1 %>%
  stack(r_mcr) 
tmp1 <- tmp1 %>%
  rasterToPoints() 
tmp1 <- tmp1 %>%
  as.data.frame()
tmp1 <- tmp1 %>%
  # Renaming columns
  dplyr::select(IDGRID = layer, pm25, x, y)
tmp1 <- tmp1 %>%
  # Merging on weights to aggregate
  right_join(Weights_msoa,
             by = 'IDGRID') 
tmp2 <- tmp1 %>%
  # Aggregating grid to
  ddply(.(area_id),
        summarize,
        pm25_cams_agg = weighted.mean(pm25, weight))
# Extracting PM2.5 values at centroids
tmp2$pm25_cams_cent <- raster::extract(r_uk, mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry())
# Adding on date and hour
tmp2 <- tmp2 %>%
  mutate(hour = j,
         date = i) %>%
  # Outputting datasets
  dplyr::select(area_id, date, hour, pm25_cams_cent, pm25_cams_agg)
# Appending together
pm25_cams <- rbind(pm25_cams, tmp2)
# Removing uncessary datasets

#########################


# Loop for each date
for (i in as.character(seq(as.Date('2021-01-01'), as.Date('2021-03-31'), by = 1))){
  # Loop for each time
  for (j in 0:23){
    # Reading in PM25 from CAMS
    r_file_in <- raster(paste('original/Processed/PM25/CAMS-Europe/PM25_', i, '-', sprintf("%02d", j), "00.tif", sep = ''))
    # Renaming raster
    names(r_file_in) <- 'pm25'
    # Creating aggregated estimates of PM25 by MSOA
    tmp1 <- r_file_in %>%
      # Converting raster to dataframe
      crop(r_mcr) %>%
      stack(r_mcr) %>%
      rasterToPoints() %>%
      as.data.frame()%>%
      # Renaming columns
      dplyr::select(IDGRID = layer, pm25) %>%
      # Merging on weights to aggregate
      right_join(Weights_msoa,
                 by = 'IDGRID') %>%
      # Aggregating grid to
      ddply(.(area_id),
            summarize,
            pm25_cams_agg = weighted.mean(pm25, weight))
    # Extracting PM2.5 values at centroids
    tmp1$pm25_cams_cent <- raster::extract(r_uk, mcr_msoa[,c('cent_long', 'cent_lat')] %>% st_drop_geometry())
    # Adding on date and hour
    tmp1 <- tmp1 %>%
      mutate(hour = j,
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
save(pm25_cams, file = "original/Processed/PM25/pm25_cams.RData")
