source(here::here("tests", "test-helpers.R"))

test_that("pm25 ground monitoring data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if(!env_should_run_test("SPF_RUN_HLDT_1F"),
          "Configured not to run (env var SPF_RUN_HLDT_1F is not 'true')")

  # This should load 1 object 'pm25_gm' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/PM25/pm25_gm.RData"))

  # This will load the pm25 gm data which is produced by running the package
  # code
  act_pm25_gm_dat <- readRDS(
    here::here("Data_act/Processed/PM25/pm25_gm.rds")
  )

  expect_equal(act_pm25_gm_dat, pm25_gm)
})
