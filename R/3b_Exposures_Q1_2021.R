# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Process Exposures data for Q1 2021
#'
#' @param msoa_lim Number of MSOAs to process. Default: Number of unique MSOAs
#'
#' @inheritParams write_cfg_template
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' process_exposures_q1_2021()
#' }
#'
process_exposures_q1_2021 <- function(cfg = NULL, msoa_lim = NULL) {

  # Read population data
  pop_dat <- readRDS("Data/Processed/Population/pop_dat.rds")
  pm25_emep <- readRDS("Data/Processed/PM25/pm25_emep.rds")
  pm25_cams <- readRDS("Data/Processed/PM25/pm25_cams.rds")

  # TEMP: Number of loops whilst getting the code working
  msoa_lim <- msoa_lim %||% 2
  # msoa_lim <- msoa_lim %||% 7
  # msoa_lim <- msoa_lim %||% length(unique(pop_dat$area_id))

  # Suppress summarise info without changing global options
  op <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(op), add = TRUE, after = FALSE)

  # Setting seed
  set.seed(1409)

  # Merging pm2.5 data together
  pm25_ctm <- pm25_cams %>%
    dplyr::select(area_id, date, hour, pm25_cams_agg)%>%
    dplyr::left_join(pm25_emep %>%
                       dplyr::select(area_id, date, hour, pm25_emep_agg = pm25_cams_agg),
                     by = c("area_id", "date", "hour")) %>%
    dplyr::filter(as.Date(date) >= as.Date("2020-12-20") &
                    as.Date(date) <= as.Date("2021-03-31")) %>%
    dplyr::mutate(pm25_emep_agg = ifelse(is.na(pm25_emep_agg), pm25_cams_agg, pm25_emep_agg),
                  pm25_five = 5,
                  date = as.Date(date))

  # Removing uncessary datasets
  rm(pm25_cams, pm25_emep)

  ############################
  ### Estimating exposures ###
  ############################
  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
    t1 <- Sys.time()
    # Saving datasets
    activities_complete <- readRDS(paste('Output/CaseStudy2/Activities/activities_', k, '.rds', sep = ''))

    # Parparing data for exposure modelling
    activities_complete <- activities_complete %>%
      # Only keeping specific period
      dplyr::filter(as.numeric(date) >= 18616 &
                      as.numeric(date) <= 18717) %>%
      # Adding on demographic variables
      dplyr::left_join(pop_dat %>%
                         dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
                       by = 'pop_id') %>%
      # Merging on pm data
      dplyr::left_join(pm25_ctm %>%
                         dplyr::select(area_id, date, hour, pm25_cams_agg, pm25_five, pm25_emep_agg),
                       by = c('area_id', 'date', 'hour')) %>%
      as.data.frame()

    # Transportation exposures
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_tns")
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_tns")
    activities_complete <- calculate_transport(activities_complete, ambient = "pm25_five", outvar = "pm25_five_tns")

    # Indoor-not-home exposures
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_inh")
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_inh")
    activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_five", outvar = "pm25_five_inh")

    # Household exposures
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_hhd")
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_hhd")
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat,
                                               ambient = "pm25_five", outvar = "pm25_five_hhd")
    # Saving datasets
    saveRDS(activities_complete, file = paste('Output/CaseStudy2/Exposures_Q1_2021/exposures_', k, '.rds', sep = ''))

    t2 <- Sys.time()
    # Printing index
    print(k)
    print(t2-t1)
  }

  invisible()
}
