source(here::here("tests", "testthat", "test-helpers.R"))

test_that("results jul 2021 data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if(!env_should_run_test("SPF_RUN_HLDT_4A"),
          glue::glue("'SPF_RUN_HLDT_4A=false' and 'SPF_RUN_ALL_HLDT=false';",
                     " to run this test, set one or both to true"))

  # This should load 1 object 'out_july2021' into the current environment, it is
  # a data frame
  load(file = here::here("Output_ref/CaseStudy2/Analysis/DailyAverage_July_2021.RData"))

  act_out_july2021 <- readRDS(here::here("Output_act/CaseStudy2/Analysis/DailyAverage_July_2021.rds"))

  expect_equal(act_out_july2021, out_july2021)
})
