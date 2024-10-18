test_that("population data matches reference data", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  # This should load 1 object 'pop_dat' into the current environment; it is a
  # data frame
  load(here::here("Data_ref/Processed/Population/pop_dat.RData"))

  # This will load the population data which is produced by running the package
  # code
  act_pop_dat <- readRDS(here::here("Data_act/Processed/Population/pop_dat.rds"))

  expect_equal(act_pop_dat, pop_dat)
})
