test_that("shapefile data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # This should load 3 objects: ew_msoa, ew_msoa_region and uk_full into the
  # current environment. They are S4 objects of type 'Large
  # SpatialPolygonsDataFrame'
  load(here::here("Data_ref/Processed/Shapefiles/shapefiles.RData"))

  # This will load the ew_msoa data which is produced by running the package
  # code
  # Note: The objects in the reference shapefiles.RData are S4 objects of type
  # Large SpatialPolygonsDataFrame whilst this is a data frame
  act_ew_msoa <- readRDS(here::here("Data_act/Processed/Shapefiles/ew_msoa.rds"))

  # Retain only vars required for comparison
  act_ew_msoa <- as.data.frame(act_ew_msoa) %>%
    dplyr::select(area_id,
                  area_name,
                  cent_long,
                  cent_lat,
                  parent_area_name)

  # Note: The reference data has row names numbered from zero instead of 1, so
  # make the data match this before checking for equality
  rownames(act_ew_msoa) <- seq_len(nrow(act_ew_msoa)) - 1

  expect_equal(act_ew_msoa, as.data.frame(ew_msoa), ignore_attr = TRUE)
})
