test_that("pm25 cams data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # This should load 1 object 'pm25_cams' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/PM25/pm25_cams.RData"))

  # This will load the pm25 data which is produced by running the package
  # code
  act_pm25_cams_dat <- readRDS(
    here::here("Data_act/Processed/PM25/pm25_cams.rds")
  )

  expect_equal(act_pm25_cams_dat, pm25_cams)
})
