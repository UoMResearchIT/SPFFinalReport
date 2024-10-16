#' Process Exposures data for July 2021
#'
#' @param msoa_lim Number of MSOAs to process. Default: Number of unique MSOAs
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' get_exposures_jul_2021()
#' }
#'
process_exposures_jul_2021 <- function(msoa_lim = NULL) {

  # Read population data
  load("Data_ref/Processed/Population/pop_dat.RData")
  load("Data_ref/Processed/PM25/pm25_gm.RData")

  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  # TEMP: Number of loops whilst getting the code working
  msoa_lim <- msoa_lim %||% 2
  # msoa_lim <- msoa_lim %||% 7
  # msoa_lim <- msoa_lim %||% length(unique(pop_dat$area_id))

  # Setting seed
  set.seed(1409)

  ############################
  ### Estimating exposures ###
  ############################
  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
    t1 <- Sys.time()
    # Saving datasets
    load(paste('Output_act/CaseStudy2/Activities/activities_', k, '.RData', sep = ''))

    # Parparing data for exposure modelling
    activities_complete <- activities_complete %>%
      # Only keeping specific period
      filter(as.numeric(date) >= 18779 &
               as.numeric(date) <= 18839) %>%
      # Adding on demographic variables
      left_join(pop_dat %>%
                  dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
                by = 'pop_id') %>%
      # Merging on pm data
      left_join(pm25_gm %>%
                  # Only keeping specific period
                  filter(date >= as.Date('2021-06-01') &
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
    save(activities_complete, file = paste('Output_act/CaseStudy2/Exposures_July_2021/exposures_', k, '.RData', sep = ''))

    t2 <- Sys.time()
    # Printing index
    print(k)
    print(t2-t1)
  }

  invisible(NULL)
}
