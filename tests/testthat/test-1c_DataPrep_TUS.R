test_that("time use survey data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # This should load 1 object 'tus_dat' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/TimeUseSurvey/tus_dat.RData"))

  # This will load the time use survey data which is produced by running the
  # package code
  act_tus_dat <- readRDS(here::here("Data_act/Processed/TimeUseSurvey/tus_dat.rds"))

  expect_equal(act_tus_dat, tus_dat)
})
