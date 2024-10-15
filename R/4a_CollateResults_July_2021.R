#' Collate results for July 2021
#'
#' @param msoa_lim Number of MSOAs to process. Default: Number of unique MSOAs
#'
#' @return NULL (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' collate_results_jul_2021()
#' }
#'
collate_results_jul_2021 <- function(msoa_lim = NULL) {

  # Read population data
  load("Data/Processed/Population/pop_dat.RData")

  # TEMP: Number of loops whilst getting the code working
  msoa_lim <- msoa_lim %||% 2
  # msoa_lim <- msoa_lim %||% 7
  # msoa_lim <- msoa_lim %||% 173
  # msoa_lim <- msoa_lim %||% length(unique(pop_dat$area_id))

  ############################
  ### Estimating exposures ###
  ############################
  # Empty datasets
  out_july2021 <- NULL

  # Loop for each MSOA
  for (k in unique(pop_dat$area_id)[1:msoa_lim]) {
    t1 <- Sys.time()
    # Saving datasets
    load(paste('Output/CaseStudy2/Exposures/exposures_', k, '_July_2021.RData', sep = ''))
    # Preparing case study 2
    activities_complete <- activities_complete %>%
      # Only keepting July
      dplyr::filter(date >= as.Date("2021-07-01")) %>%
      # Getting exposures
      dplyr::mutate(exposure_aurn = ifelse(micro_group == "outdoor", pm25_aurn_near,
                                           ifelse(micro_group == "indoor", pm25_aurn_near_inh,
                                                  ifelse(micro_group == "transport", pm25_aurn_near_tns,
                                                         ifelse(micro_group == "home", pm25_aurn_near_hhd, NA)))),
                    exposure_ltn = ifelse(micro_group == "outdoor", pm25_ltn_near,
                                          ifelse(micro_group == "indoor", pm25_ltn_near_inh,
                                                 ifelse(micro_group == "transport", pm25_ltn_near_tns,
                                                        ifelse(micro_group == "home", pm25_ltn_near_hhd, NA)))),
                    exposure_gm = ifelse(micro_group == "outdoor", pm25_gm_near,
                                         ifelse(micro_group == "indoor", pm25_gm_near_inh,
                                                ifelse(micro_group == "transport", pm25_gm_near_tns,
                                                       ifelse(micro_group == "home", pm25_gm_near_hhd, NA)))))%>%
      # Averaging by day
      ddply(.(area_id, pop_id, date, daytype, daytype_label, season, season_label,
              sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
            summarize,
            exposure_aurn = mean(exposure_aurn),
            exposure_ltn = mean(exposure_ltn),
            exposure_gm = mean(exposure_gm),
            pm25_aurn_near = mean(pm25_aurn_near),
            pm25_ltn_near = mean(pm25_ltn_near),
            pm25_gm_near = mean(pm25_gm_near))
    # Appending on
    out_july2021 <- rbind(out_july2021, activities_complete)
    t2 <- Sys.time()
    # Printing index
    print(k)
    print(t2-t1)
  }

  # Saving outputs
  save(out_july2021, file = 'Output/CaseStudy2/Analysis/DailyAverage_July_2021.RData')

  invisible(NULL)
}
