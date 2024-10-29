# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Process Exposures data for July 2021
#'
#' @param msoa_lim Number of MSOAs to process. Default: Number of unique MSOAs
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' process_exposures_jul_2021()
#' }
#'
process_exposures_jul_2021 <- function(msoa_lim = NULL) {

  # Read population data
  pop_dat <- readRDS("Data_act/Processed/Population/pop_dat.rds")
  pm25_gm <- readRDS("Data_act/Processed/PM25/pm25_gm.rds")

  # TEMP: Number of loops whilst getting the code working
  # msoa_lim <- msoa_lim %||% 2
  # msoa_lim <- msoa_lim %||% 7
  msoa_lim <- msoa_lim %||% length(unique(pop_dat$area_id))

  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  # Setting seed
  set.seed(1409)

  ############################
  ### Estimating exposures ###
  ############################
  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
  # area_ids <- sort(unique(pop_dat$area_id)[1:msoa_lim])
  # n_area_ids <- length(area_ids)
  # for (j in seq_along(area_ids)) {
  #   k <- area_ids[j]
    t1 <- Sys.time()
    # Saving datasets
    activities_complete <- readRDS(paste('Output_act/CaseStudy2/Activities/activities_', k, '.rds', sep = ''))

    # Parparing data for exposure modelling
    activities_complete <- activities_complete %>%
      # Only keeping specific period
      dplyr::filter(as.numeric(date) >= 18779 &
                      as.numeric(date) <= 18839) %>%
      # Adding on demographic variables
      dplyr::left_join(pop_dat %>%
                         dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
                       by = 'pop_id') %>%
      # Merging on pm data
      dplyr::left_join(pm25_gm %>%
                         # Only keeping specific period
                         dplyr::filter(date >= as.Date('2021-06-01') &
                                         date <= as.Date('2021-07-31')) %>%
                         dplyr::select(area_id, date, hour, pm25_aurn_near, pm25_gm_near, pm25_ltn_near),
                       by = c('area_id', 'date', 'hour')) %>%
      as.data.frame()

    # Transportation exposures
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_tns")
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_tns")
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_gm_near", outvar = "pm25_gm_near_tns")

    # Indoor-not-home exposures
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_inh")
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_inh")
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_gm_near", outvar = "pm25_gm_near_inh")

    # Household exposures
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_aurn_near", outvar = "pm25_aurn_near_hhd")
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_ltn_near", outvar = "pm25_ltn_near_hhd")
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_gm_near", outvar = "pm25_gm_near_hhd")
    # Saving datasets
    saveRDS(activities_complete, file = paste('Output_act/CaseStudy2/Exposures_July_2021/exposures_', k, '.rds', sep = ''))

    t2 <- Sys.time()
    # Printing index
    print(k)
    # cat("\n", glue::glue("\"{k}\" [{j} of {n_area_ids}]"), "\n")
    print(t2-t1)
  }

  invisible()
}
