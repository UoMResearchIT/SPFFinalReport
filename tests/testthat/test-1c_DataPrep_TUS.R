source(here::here("tests","testthat", "test-helpers.R"))

test_that("time use survey data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if(!env_should_run_test("SPF_RUN_HLDT_1C"),
          glue::glue("'SPF_RUN_HLDT_1C=false' and 'SPF_RUN_ALL_HLDT=false';",
                     " to run this test, set one or both to true"))

  # This should load 1 object 'tus_dat' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/TimeUseSurvey/tus_dat.RData"))

  # This will load the time use survey data which is produced by running the
  # package code
  act_tus_dat <- readRDS(here::here("Data_act/Processed/TimeUseSurvey/tus_dat.rds"))

  expect_equal(act_tus_dat, tus_dat)
})
