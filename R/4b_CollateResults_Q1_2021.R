# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Collate results for Q1 2021
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
#' collate_results_q1_2021()
#' }
#'
collate_results_q1_2021 <- function(cfg = NULL, msoa_lim = NULL) {

  # Read population data
  pop_dat <- readRDS("Data/Processed/Population/pop_dat.rds")

  # TEMP: Number of loops whilst getting the code working
  msoa_lim <- msoa_lim %||% 2
  # msoa_lim <- msoa_lim %||% 7
  # msoa_lim <- msoa_lim %||% 173
  # msoa_lim <- msoa_lim %||% length(unique(pop_dat$area_id))

  ############################
  ### Estimating exposures ###
  ############################
  # Empty datasets
  out_q12021 <- NULL

  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
    t1 <- Sys.time()
    # Saving datasets
    activities_complete <- readRDS(paste('Output/CaseStudy2/Exposures_Q1_2021/exposures_', k, '.rds', sep = ''))
    # Preparing case study 2
    activities_complete <- activities_complete %>%
      # Only keeping Q1 2021
      dplyr::filter(date >= as.Date("2021-01-01")) %>%
      # Getting exposures
      dplyr::mutate(exposure_cams = ifelse(micro_group == "outdoor", pm25_cams_agg,
                                           ifelse(micro_group == "indoor", pm25_cams_agg_inh,
                                                  ifelse(micro_group == "transport", pm25_cams_agg_tns,
                                                         ifelse(micro_group == "home", pm25_cams_agg_hhd, NA)))),
                    exposure_emep = ifelse(micro_group == "outdoor", pm25_emep_agg,
                                           ifelse(micro_group == "indoor", pm25_emep_agg_inh,
                                                  ifelse(micro_group == "transport", pm25_emep_agg_tns,
                                                         ifelse(micro_group == "home", pm25_emep_agg_hhd, NA)))),
                    exposure_five = ifelse(micro_group == "outdoor", pm25_five,
                                           ifelse(micro_group == "indoor", pm25_five_inh,
                                                  ifelse(micro_group == "transport", pm25_five_tns,
                                                         ifelse(micro_group == "home", pm25_five_hhd, NA)))))%>%
      # Averaging by day
      # Note: Using the .(area_id, pop_id, ...) syntax causes a name conflict
      # with dplyr - so use the formula syntax for the 'group by' var instead,
      # i.e. '~ area_id + pop_id + ...' rather than '.(area_id, pop_id, ...)'
      plyr::ddply(~ area_id + pop_id + date + daytype + daytype_label + season +
                    season_label + sex + sex_label + agegr4 + agegr4_label +
                    nssec5 + nssec5_label,
                  plyr::summarize,
                  exposure_cams = mean(exposure_cams),
                  exposure_emep = mean(exposure_emep),
                  exposure_five = mean(exposure_five),
                  pm25_cams_agg = mean(pm25_cams_agg),
                  pm25_emep_agg = mean(pm25_emep_agg),
                  pm25_five = mean(pm25_five))
    # Appending on
    out_q12021 <- rbind(out_q12021, activities_complete)
    t2 <- Sys.time()
    # Printing index
    print(k)
    print(t2-t1)
  }

  # Saving outputs
  saveRDS(out_q12021, file = 'Output/CaseStudy2/Analysis/DailyAverage_Q1_2021.rds')

  invisible()
}
