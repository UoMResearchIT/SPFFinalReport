source(here::here("tests", "test-helpers.R"))

test_that("activities 2021 data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # Check system env vars to determine whether to run this high level data test
  skip_if(!env_should_run_test("SPF_RUN_HLDT_2"),
          "Configured not to run (env var SPF_RUN_HLDT_2 is not 'true')")

  pop_dat <- readRDS(here::here("Data_act/Processed/Population/pop_dat.rds"))

  # Note: There are 346 areas (hence 346 tests to run)
  areas <- sort(unique(pop_dat$area_id))

  # TEMP: Temporarily limit no. of tests: test only 1st & last n
  # Set n to a small integer value to limit the number of tests to 2*n or NA
  # to run all tests
  n <- NA
  if (is.na(n)) {
    test_areas <- areas
  } else {
    test_areas <- areas[c(1:n, (length(areas) - n + 1):length(areas))]
  }

  # Example area IDs:
  #   c("E02000984", "E02000985", "E02006916", "E02006917")

  for (k in test_areas) {

    # Reference data
    path1 <- paste("Output_ref/CaseStudy2/Activities/activities_", k, ".RData",
                   sep = "")

    # Data produced by running package code
    path2 <- paste("Output_act/CaseStudy2/Activities/activities_", k, ".rds",
                   sep = "")

    # Example ref data path:
    #   "Output_ref/CaseStudy2/Activities/activities_E02000984.RData"

    # Example act data path:
    #   "Output_act/CaseStudy2/Activities/activities_E02000984.rds"

    # This should load 1 object 'activities_complete' into the current
    # environment; it is a data frame
    load(file = here::here(path1))

    act_activities_complete <- readRDS(here::here(path2))

    expect_equal(act_activities_complete, activities_complete)
  }

})
