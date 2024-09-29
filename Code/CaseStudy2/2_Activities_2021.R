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
load("original/Processed/TimeUseSurvey/tus_dat.RData")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

library(SyncRNG)
# Setting seed
#set.seed(1409)
s <- SyncRNG(seed=1409)

##############################################
### Filling in missings in the TUS dataset ###
##############################################
# Filling in missings using most popular activities
tus_dat <- tus_dat %>%
  # Setting missings to NA
  dplyr::mutate(location_popular = ifelse(location %in% c(-9, 0, 10, 99), NA, location)) %>%
  # Merging on most popular location for each activity by strata
  left_join(tus_dat %>%
              # Only keeping non-missing locations
              filter(!(location %in% c(-9, 0, 10, 99))) %>%
              # Summarising by stratum and activity
              dplyr::group_by_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity', 'location')) %>%
              dplyr::summarise(n = length(location))  %>% 
              dplyr::group_by_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity')) %>%
              # Getting proportion of time spent in each location by activity
              dplyr::mutate(p = n/sum(n),
                            tmp = max(p)) %>%
              # Only keeping most popular activity
              dplyr::filter(p == tmp) %>%
              # Keeping first if more than one selected
              dplyr::mutate(tmp = min(location)) %>%
              dplyr::filter(location == tmp) %>%
              # Ungrouping
              dplyr::ungroup() %>%
              # Only keeping relevant columns
              dplyr::select_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity', 'location')) %>%
              # Renaming columns
              dplyr::rename(tmp = location),
            by = c('sex', 'agegr4', 'nssec5', 'daytype', 'activity'))  %>%
  # Filling in the missings
  dplyr::mutate(location_popular = ifelse(is.na(location_popular), 
                                          tmp,
                                          location_popular)) %>%
  # Removing uncessary columns 
  dplyr::select(-c(tmp))  %>%
  # Merging on most popular location for each activity by strata
  left_join(tus_dat %>%
              # Only keeping non-missing locations
              filter(!(location %in% c(-9, 0, 10, 99))) %>%
              # Summarising by stratum and activity
              dplyr::group_by_at(c('activity', 'location')) %>%
              dplyr::summarise(n = length(location))  %>% 
              dplyr::group_by_at(c('activity')) %>%
              # Getting proportion of time spent in each location by activity
              dplyr::mutate(p = n/sum(n),
                            tmp = max(p)) %>%
              # Only keeping most popular activity
              dplyr::filter(p == tmp) %>%
              # Keeping first if more than one selected
              dplyr::mutate(tmp = min(location)) %>%
              dplyr::filter(location == tmp) %>%
              # Ungrouping
              dplyr::ungroup() %>%
              # Only keeping relevant columns
              dplyr::select_at(c('activity', 'location')) %>%
              # Renaming columns
              dplyr::rename(tmp = location),
            by = c('activity'))  %>%
  # Filling in the missings
  dplyr::mutate(location_popular = ifelse(is.na(location_popular), 
                                          tmp,
                                          location_popular)) %>%
  # Removing uncessary columns 
  dplyr::select(-c(tmp))%>%
  # Merging on location labels
  left_join(read_csv("original/Raw/TimeUseSurvey/uktus_metadata_location.csv") %>%
              dplyr::select(location_popular = location, 
                            location_popular_label = location_label),
            by = 'location_popular')

########################################################################
### Sampling activity sequences (Method 1) - Complete sequences only ###
########################################################################

# Merging to skeleton dataset
activities_all <- NULL

#######################################
############ TEMP for debug ###########
#######################################

pop_dat_tmp = subset(pop_dat, area_id == 'E02000984')
tus_dat_tmp = subset(tus_dat, percmissing == 0)
nsample = 1
weights = "weights_diary"
pop_strata = c('area_id')
tus_strata = c('sex', 'agegr4', 'nssec5', 'daytype')
start_date = '2021-01-01'
end_date = '2021-03-31'
keep = c('activity', 'activity_label', 'location', 'location_label')
pop_id_debug = c(47)



################
### Preamble ###
################

# If no weights are provided for sampling then use equal weighting 
if (is.null(weights) == TRUE){
  tus_dat_tmp$weights <- 1
}else{
  # Else use the weights specified
  tus_dat_tmp$weights <- tus_dat_tmp[,weights]
}


########################
### Getting metadata ###
########################
# Getting a list of strata for activities 
lst_strata <- tus_dat_tmp %>% 
  # Grouping by stratification variables
  group_by_at(.vars = tus_strata)%>% 
  # Summarising 
  dplyr::summarise(n = dplyr::n()) %>%
  ungroup() %>%
  # Adding label to each strata
  dplyr::mutate(strata = 1:dplyr::n()) %>%
  dplyr::select(-c(n))

# Getting activities ID and 
tus_act_id <- tus_dat_tmp %>%
  # Merging on stratification labels
  left_join(lst_strata,
            by = tus_strata) 
# Normalising the weights within each strata
tus_act_id <- tus_act_id %>%
  # Merging on summary of weights in each strata
  left_join(tus_act_id%>%
              dplyr::group_by(strata) %>%
              dplyr::summarise(sums = sum(weights)),
            by = 'strata') %>%
  # Normalising weights
  mutate(weights = weights/sums) %>%
  # Removing unnecessary columns 
  dplyr::select(-c(sums))

##########################################
### Preparing population time profiles ###
##########################################
# Sampling population to find exposures for
pop_dat2 <- pop_dat_tmp %>%
  group_by_at(.vars = pop_strata) %>%
  sample_n_acts(size = nsample,
                prob = NULL,
                replace = FALSE,
                fixed_seed = TRUE)
#sample_n(size = nsample, 
#         replace = FALSE)

# Preparing shell dataset for sampling 
activities <- expand.grid(pop_id = pop_dat2$pop_id,
                          date = seq(as.Date(start_date), as.Date(end_date), by = 1)) %>%
  # Adding on day information
  mutate(day_label = weekdays(date),
         day = as.numeric(factor(weekdays(date), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))), 
         daytype = case_when(day %in% c(1,7) ~ 1,
                             day %in% 2:6 ~ 2),
         daytype_label = case_when(day %in% c(1,7) ~ 'Weekend',
                                   day %in% 2:6 ~ 'Weekday'),
         season = case_when(month(date) %in% c(12, 1, 2) ~ 1,
                            month(date) %in% c(3:5) ~ 2,
                            month(date) %in% c(6:8) ~ 3,
                            month(date) %in% c(9:11) ~ 4),
         season_label = case_when(month(date) %in% c(12, 1, 2) ~ 'Winter',
                                  month(date) %in% c(3:5) ~ 'Spring',
                                  month(date) %in% c(6:8) ~ 'Summer',
                                  month(date) %in% c(9:11) ~ 'Autumn')) %>%
  # Merging on population data
  left_join(pop_dat_tmp %>%
              select_at(c('pop_id', 'sex', 'agegr4', 'nssec5')),
            by = 'pop_id') %>%
  # Merging on stratification labels
  left_join(lst_strata,
            by = tus_strata)
# Removing unecessary datasets
rm(pop_dat2, lst_strata)

###################################
### Sampling activity sequences ###
###################################
# Empty dataset 
activities$act_id <- as.numeric(NA)
# Loop for each strata
for (i in unique(activities$strata)){
  # Sampling within each strata
  
  # ATTEMPT AT HARD CODING RESULT
  activities$act_id[which(activities$strata == i)] <- 
  #  c(5363.0, 12052.0, 15865.0, 16023.0,7702.0, 15560.0,15343.0,4325.0, 5791.0,
  #    13345.0, 16023.0, 15024.0,8144.0, 5285.0, 8828.0, 12949.0,11262.0,14496.0,
  #    1403.0, 98.0, 12053.0,6555.0, 13537.0,13401.0, 9697.0, 15006.0, 98.0,
  #    4325.0, 11841.0, 13401.0, 7703.0, 15311.0,7119.0, 12053.0,14496.0,11261.0,
  #    97.0, 10735.0,13400.0,10123.0,9226.0, 12931.0,5106.0, 15005.0, 4334.0,
  #    8277.0, 15024.0, 959.0, 12416.0,14981.0,8145.0, 8145.0, 15311.0,4325.0,
  #    15006.0,424.0,15712.0,8827.0, 8145.0, 15712.0,12931.0,2375.0, 1403.0,
  #    13400.0,12696.0,12949.0,9773.0, 12736.0,8526.0, 13614.0,8828.0, 12504.0,
  #    12504.0,12697.0,15006.0,12200.0,16129.0,2375.0, 1402.0, 3767.0, 11217.0,
  #    12697.0,8234.0, 4068.0, 12697.0,8525.0, 2376.0, 16023.0,12144.0,9226.0)
  
  # ATTEMPT AT USING SyncRNG LIB.
    #sample_acts(x = tus_act_id$act_id[which(tus_act_id$strata == i)], 
    #            size = length(activities$pop_id[which(activities$strata == i)]), 
    #            prob = tus_act_id$weights[which(tus_act_id$strata == i)], 
    #            replace = TRUE, 
    #            fixed_seed = TRUE)
  
  # ORIGINAL CODE
  sample(x = tus_act_id$act_id[which(tus_act_id$strata == i)], 
              size = length(activities$pop_id[which(activities$strata == i)]), 
              prob = tus_act_id$weights[which(tus_act_id$strata == i)], 
              replace = TRUE)
}

tus_dat_tmp <- tus_dat_tmp[, c('act_id', 'time', 'time_label', keep)]

save(activities, 
     file = paste('anns_run/Processed/Activities/debug_activities_for_merging_R.RData', sep = ''))
save(tus_dat_tmp, 
     file = paste('anns_run/Processed/Activities/debug_tus_dat_for_merging_R.RData', sep = ''))

# Merging on the activity data 
activities_merged <- merge(activities, 
                    tus_dat_tmp[, c('act_id', 'time', 'time_label', keep)],
                    by = 'act_id') %>%
  arrange(pop_id, date, time) %>%
  dplyr::select(-c(sex, agegr4, nssec5, strata))
# Returning activity samples 


#######################################
####### END TEMP for debug ############
#######################################


# Loop for each MSOA
for (k in unique(pop_dat$area_id)){
  # Sampling activities 
  activities_complete <- sample_population(subset(pop_dat, area_id == k), 
                                           subset(tus_dat, percmissing == 0), 
                                           nsample = 1,
                                           weights = "weights_diary",
                                           pop_strata = c('area_id'),
                                           tus_strata = c('sex', 'agegr4', 'nssec5', 'daytype'),
                                           start_date = '2021-01-01',
                                           end_date = '2021-03-31',
                                           keep = c('activity', 'activity_label', 'location', 'location_label'))

  # Activity sequences run from 04:00-03:59 so need to "shift"
  # the end of the profiles into the next day
  activities_complete <- activities_complete %>%
    # Getting hour and resting date after 00:00
    mutate(hour = (floor((time-1)/6) + 4) %% 24,
           date = if_else(hour %in% 0:3, date + 1, date)) %>%
    # Removign day information as we have to shift the day
    dplyr::select(-c(day, day_label, daytype, daytype_label, season, season_label)) %>%
    # Adding on day information
    mutate(day_label = weekdays(date),
           day = as.numeric(factor(weekdays(date), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))), 
           daytype = case_when(day %in% c(1,7) ~ 1,
                               day %in% 2:6 ~ 2),
           daytype_label = case_when(day %in% c(1,7) ~ 'Weekend',
                                     day %in% 2:6 ~ 'Weekday'),
           season = case_when(month(date) %in% c(12, 1, 2) ~ 1,
                              month(date) %in% c(3:5) ~ 2,
                              month(date) %in% c(6:8) ~ 3,
                              month(date) %in% c(9:11) ~ 4),
           season_label = case_when(month(date) %in% c(12, 1, 2) ~ 'Winter',
                                    month(date) %in% c(3:5) ~ 'Spring',
                                    month(date) %in% c(6:8) ~ 'Summer',
                                    month(date) %in% c(9:11) ~ 'Autumn')) %>%
    # Removing first day
    dplyr::filter(date >= as.Date('2020-12-01') & date <= as.Date('2021-12-31'))
  
  # Adding micro-environments to the dataset 
  activities_complete <- activities_complete %>%
    dplyr::mutate(micro_group = case_when(location %in% c(11:12) ~ "home",
                                          location %in% c(13, 14, 15, 16, 17, 19, 20, 21) ~ "indoor",
                                          location %in% c(18, 31, 32) ~ "outdoor",
                                          location %in% c(30, 33:49, 90) ~ "transport"))
  
  # Aggregating to the hourly time series
  activities_complete <- activities_complete %>%
    # Getting time within hour 
    dplyr::mutate(minutes = ((time - 1) %% 6) + 1) %>%
    # Grouping by population, date and hour to reduce
    dplyr::group_by(pop_id, date, hour) %>%
    
    # Sample prop to the environments
    # NEW WAY: SAMPLE WITHOUT REPLACEMENT
    dplyr::mutate(sample = sample_acts(x=c(1,2,3,4,5,6), size=1, replace = FALSE, fixed_seed = TRUE)) %>%

    # Only keep sampled time point
    dplyr::filter(minutes == sample) %>%
    # Removing unecesary columns
    dplyr::select(-c(minutes, sample))
  
  # Saving datasets 
  save(activities_complete, file = paste('original/Processed/Activities/activities_', k, '.RData', sep = ''))
  
  # Appending together
  activities_all <- rbind(activities_all, activities_complete)
  
  # Printing index
  print(k)
}

save(activities_all, file = paste('original/Processed/Activities/activities_all_areas.RData', sep = ''))

# Clearing Workspace
rm(list = ls())


