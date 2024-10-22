source(here::here("tests","testthat", "test-helpers.R"))

test_that("pm25 emep data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if(!env_should_run_test("SPF_RUN_HLDT_1E"),
          glue::glue("'SPF_RUN_HLDT_1E=false' and 'SPF_RUN_ALL_HLDT=false';",
                     " to run this test, set one or both to true"))

  # This should load 1 object 'pm25_emep' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/PM25/pm25_emep.RData"))

  # This will load the pm25 emep data which is produced by running the package
  # code
  act_pm25_emep_dat <- readRDS(
    here::here("Data_act/Processed/PM25/pm25_emep.rds")
  )

  expect_equal(act_pm25_emep_dat, pm25_emep)
})
