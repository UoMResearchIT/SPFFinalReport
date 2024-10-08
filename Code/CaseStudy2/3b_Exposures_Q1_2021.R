#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())


# Setting working directory
setwd('C:/Users/mcassag/Data/personal_exposure_model')

# Loading source code
source('C:/Users/mcassag/Code/Personal_Exposure_Model/SPFFinalReport/Code/CaseStudy2/0_Source.R')

# Read population data
load("original/Processed/Population/pop_dat.RData")
load("original/Processed/PM25/CAMS-Europe/pm25_cams.RData")
load("original/Processed/PM25/EMEP/pm25_emep.RData")


# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Setting seed
set.seed(1409)

# Merging pm2.5 data together
pm25_ctm <- pm25_cams %>%
  dplyr::select(area_id, date, hour, pm25_cams_agg) %>%
  left_join(pm25_emep %>%
              # NOTE: = pm25_cams_agg commented out as it wouldn't run with that, as it doesn't exist
              #         Also, it doesn't make sense to match on float measurement values.
              dplyr::select(area_id, date, hour, pm25_emep_agg), #= pm25_cams_agg),
            by = c("area_id", "date", "hour")) %>%
  filter(as.Date(date) >= as.Date("2020-12-31") & 
           as.Date(date) <= as.Date("2021-03-31")) %>%
  mutate(pm25_emep_agg = ifelse(is.na(pm25_emep_agg), pm25_cams_agg, pm25_emep_agg),
         pm25_five = 5,
         date = as.Date(date))

# Removing uncessary datasets
rm(pm25_cams, pm25_emep)

############################
### Estimating exposures ###
############################
# Loop for each MSOA
for (k in unique(pop_dat$area_id)){  #[1:7]){
  t1 <- Sys.time()
  # Saving datasets 
  load(paste('original/Processed/Activities/activities_', k, '.RData', sep = ''))
  
  # Parparing data for exposure modelling
  activities_complete <- activities_complete %>%
    # Only keeping specific period
    filter(as.numeric(date) >= as.Date("2020-12-31") & 
             as.numeric(date) <= as.Date("2021-03-31")) %>%
    # Adding on demographic variables
    left_join(pop_dat %>%
                dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
              by = 'pop_id') %>%
    # Merging on pm data
    left_join(pm25_ctm %>%
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
  save(activities_complete, file = paste('original/Processed/Exposures/exposures_', k, '.RData', sep = ''))
  
  t2 <- Sys.time()
  # Printing index
  print(k)
  print(t2-t1)
}

# Clearing Workspace
rm(list = ls())




