# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Process the Study Region data
#'
#' This function implements a data preparation step to process the Study Region
#' data. It is useful only for its side effects, i.e. for saving the processed
#' data.
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' run_data_prep_study_region()
#' }
#'
run_data_prep_study_region <- function() {

  #################################
  ### Preparing MSOA shapefiles ###
  #################################
  # Reading in whole UK shapefiles
  uk_full <- sf::st_read(dsn = 'Data_ref/Raw/Shapefiles',
                         layer = 'gadm36_GBR_0')

  # import shapefile, converte to longitude/latitude coordinates and organise data
  ew_msoa <- sf::st_read(dsn = "Data_ref/Raw/Shapefiles",
                         layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")

  # Getting MSOA centroids in long lat format
  tmp_longlat <- ew_msoa %>%
    # Extract
    sf::st_centroid(byid = TRUE) %>%
    # Transform
    sf::st_transform('+proj=longlat') %>%
    # Extract coordinates
    sf::st_coordinates() %>%
    # Converting to data frame
    as.data.frame() %>%
    # Renaming covariates
    dplyr::rename(long = X, lat = Y)

  # Adding on long/lat to original MSOAs
  ew_msoa <- cbind(ew_msoa, tmp_longlat)

  # Getting relevant columns
  ew_msoa <- ew_msoa %>%
    dplyr::rename(area_id = msoa11cd, area_name = msoa11nm, cent_long = long, cent_lat = lat)

  # Note: 'Data/Raw/Shapefiles/area_hierarchy.csv' for retrieving the parent
  # area name is not currently available as at 30sep24 - so derive the parent
  # area name instead; the parent_area_id does not seem to be used anywhere else
  # TODO: Check that parent_area_id is not needed elsewhere

  # Derive the parent_area_name from the area_name (being all but the last 4
  # chars)
  extract_nm <- function(nm_with_id) {
    stringr::str_trim(stringr::str_sub(nm_with_id, end = -4L))
  }
  ew_msoa <- ew_msoa %>%
    dplyr::mutate(parent_area_name = extract_nm(area_name))

  ###############################
  ### Preparing LA shapefiles ###
  ###############################
  # Aggregating to local authority
  ew_msoa_region <- ew_msoa %>%
    # Aggregating
    dplyr::group_by(parent_area_name) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()  %>%
    # Renaming column
    dplyr::rename(area_name = parent_area_name)

  # Getting LA centroids in long lat format
  tmp_longlat <- ew_msoa_region %>%
    # Extract
    sf::st_centroid(byid = TRUE) %>%
    # Transform
    sf::st_transform('+proj=longlat') %>%
    # Extract coordinates
    sf::st_coordinates() %>%
    # Converting to data frame
    as.data.frame() %>%
    # Renaming covariates
    dplyr::rename(long = X, lat = Y)

  # Adding on long/lat to original MSOAs
  ew_msoa_region <- cbind(ew_msoa_region, tmp_longlat)

  ######################
  ### Saving outputs ###
  ######################
  # Save shapefiles
  # Note: The objects in the reference shapefiles.RData are S4 objects of type
  # Large SpatialPolygonsDataFrame whilst these are data frames
  saveRDS(ew_msoa,        "Data_act/Processed/Shapefiles/ew_msoa.rds")
  saveRDS(ew_msoa_region, "Data_act/Processed/Shapefiles/ew_msoa_region.rds")
  saveRDS(uk_full,        "Data_act/Processed/Shapefiles/uk_full.rds")

  invisible(NULL)
}
