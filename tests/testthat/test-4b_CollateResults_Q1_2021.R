test_that("results q1 2021 data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if_r_cmd_check_or_not_configured("SPF_RUN_HLDT_4B")

  # This should load 1 object 'out_q12021' into the current environment, it is
  # a data frame
  load(file = here::here("Output_ref/CaseStudy2/Analysis/DailyAverage_Q1_2021.RData"))

  act_out_q12021 <- readRDS(here::here("Output_act/CaseStudy2/Analysis/DailyAverage_Q1_2021.rds"))

  expect_equal(act_out_q12021, out_q12021)
})
