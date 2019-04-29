source("lib/climate_simulator.R")
source("lib/climate_metrics.R")

replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}

narr_statistics <- readRDS("narr_statistics_pyromes.rds")
cmip5_pr_historical <- readRDS("cmip5_daily_pr_statistics_historical_pyromes.rds")
cmip5_tas_historical <- readRDS("cmip5_daily_tas_statistics_historical_pyromes.rds")
cmip5_pr_rcp45 <- readRDS("cmip5_daily_pr_statistics_rcp45_pyromes.rds")
cmip5_tas_rcp45 <- readRDS("cmip5_daily_tas_statistics_rcp45_pyromes.rds")

pyromes <- sf::st_read("Pyrome_20150605/Pyrome_20150605.shp")%>% 
  rmapshaper::ms_simplify() %>% 
  sf::st_transform("+init=epsg:4326")

pyromes_centroids <- sf::st_centroid(pyromes) %>%
  select(FPU_CODE, geometry) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

features_tas_train <- cmip5_tas_historical %>% 
  dplyr::filter(Year >= 1979 & Year <= 1998) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_tas_test <- cmip5_tas_rcp45 %>% 
  dplyr::filter(Year >= 1999 & Year <= 2018) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)



features_pr_train <- cmip5_pr_historical %>% 
  dplyr::filter(Year >= 1979 & Year <= 1998) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)


features_pr_test <- cmip5_pr_rcp45 %>% 
  dplyr::filter(Year >= 1999 & Year <= 2018) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)



truth_train <- narr_statistics %>%
  dplyr::filter(Year >= 1979 & Year <= 1998) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month) %>% 
  group_by(RegionName, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup() 


truth_test <- narr_statistics %>%
  dplyr::filter(Year >= 1999 & Year <= 2018) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month) %>% 
  group_by(RegionName, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup() 



features_train <- features_tas_train %>%
  inner_join(features_pr_train, 
             by = c("RegionName", "Year", "Season", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup() 


features_test <- features_tas_test %>%
  inner_join(features_pr_test, 
             by = c("RegionName", "Year", "Season", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup() 



train <- features_train %>% 
  inner_join(truth_train %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max, 
                             cdd_truth = cdd, 
                             cwd_truth = cwd, 
                             wet_days_percent_truth = wet_days_percent),  
             by = c("RegionName", "Season")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth, 
                cdd_error = cdd - cdd_truth, 
                cwd_error = cwd - cwd_truth, 
                wet_days_percent_error = wet_days_percent - wet_days_percent_truth) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(pyromes_centroids, by = c("RegionName"= "FPU_CODE")) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))




test <- features_test %>% 
  inner_join(truth_test %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max, 
                             cdd_truth = cdd, 
                             cwd_truth = cwd, 
                             wet_days_percent_truth = wet_days_percent),  
             by = c("RegionName", "Season")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth, 
                cdd_error = cdd - cdd_truth, 
                cwd_error = cwd - cwd_truth, 
                wet_days_percent_error = wet_days_percent - wet_days_percent_truth) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(pyromes_centroids, by = c("RegionName"= "FPU_CODE")) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))


shared_meta <- inner_join(
  x = train %>% 
    select(RegionName, gcm, ic, gcm_ic) %>% 
    distinct(), 
  y = test %>%
    select(RegionName, gcm, ic, gcm_ic) %>% 
    distinct()
)

final_train <- train %>% 
  inner_join(shared_meta) %>%
  mutate(gcm = as.factor(gcm), 
         gcm_ic = as.factor(gcm_ic), 
         Season = as.factor(Season)) 

final_test <- test %>% 
  inner_join(shared_meta) %>%
  mutate(gcm = as.factor(gcm), 
         gcm_ic = as.factor(gcm_ic), 
         Season = as.factor(Season)) 


final_train %>% 
  group_by(Season) %>%
  summarise(mean(wet_days_percent), mean(wet_days_percent_truth))

final_test %>% 
  group_by(Season) %>%
  summarise(mean(wet_days_percent), mean(wet_days_percent_truth))



t1 <- Sys.time()
mod_usfs1 <- mgcv::gam(list(
  tas_mean_error ~ s(lat, lon, bs = "gp", by = gcm) + s(gcm, Season, bs="re"), 
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm) + s(gcm, Season, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1


t1 <- Sys.time()
mod_usfs2 <- mgcv::gam(list(
  cwd_error ~ s(lat, lon, bs = "gp", by = gcm) + s(gcm, Season, bs="re"), 
  cdd_error ~ s(lat, lon, bs = "gp", by = gcm) + s(gcm, Season, bs="re"), 
  wet_days_percent_error ~ s(lat, lon, bs = "gp", by = gcm) + s(gcm, Season, bs="re")
),
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1



tas_mean_simulations <- climate_simulator(model = mod_usfs1, 
                                          response = "tas_mean", 
                                          response_index = 1,
                                          training_df = final_train,
                                          prediction_df = final_test, 
                                          N = 10000, 
                                          grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = tas_mean_truth) %>%
               distinct())


tas_mean_simulations_delta <- tas_mean_simulations %>%
  dplyr::mutate( 
    classic_lo_pct = ((classic_lo - previous_truth) / previous_truth), 
    classic_med_pct = ((classic_med - previous_truth) / previous_truth), 
    classic_upr_pct = ((classic_upr - previous_truth) / previous_truth), 
    hatQ_0_pct = ((hatQ_0 - previous_truth) / previous_truth), 
    hatQ_0.025_pct = ((hatQ_0.025 - previous_truth) / previous_truth),
    hatQ_0.25_pct = ((hatQ_0.25 - previous_truth) / previous_truth),
    hatQ_0.5_pct = ((hatQ_0.5 - previous_truth) / previous_truth), 
    hatQ_0.75_pct = ((hatQ_0.75 - previous_truth) / previous_truth), 
    hatQ_0.975_pct = ((hatQ_0.975 - previous_truth) / previous_truth), 
    hatQ_1_pct = ((hatQ_1 - previous_truth) / previous_truth), 
    
    classic_lo = ((classic_lo - previous_truth)), 
    classic_med = ((classic_med - previous_truth)), 
    classic_upr = ((classic_upr - previous_truth)), 
    hatQ_0 = ((hatQ_0 - previous_truth)), 
    hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
    hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
    hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
    hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
    hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
    hatQ_1 = ((hatQ_1 - previous_truth))
  )

tas_mean_simulations_delta_sf <- tas_mean_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(pyromes, by = c("RegionName" = "FPU_CODE")) %>% 
  st_as_sf()




pr_mean_simulations <- climate_simulator(model = mod_usfs1, 
                                          response = "pr_mean", 
                                          response_index = 2,
                                          training_df = final_train,
                                          prediction_df = final_test, 
                                          N = 10000, 
                                          grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = pr_mean_truth) %>%
               distinct())

pr_mean_simulations[,-c(1:2)] <- pr_mean_simulations[,-c(1:2)]^2


pr_mean_simulations_delta <- pr_mean_simulations %>%
  dplyr::mutate( 
    classic_lo_pct = ((classic_lo - previous_truth) / previous_truth), 
    classic_med_pct = ((classic_med - previous_truth) / previous_truth), 
    classic_upr_pct = ((classic_upr - previous_truth) / previous_truth), 
    hatQ_0_pct = ((hatQ_0 - previous_truth) / previous_truth), 
    hatQ_0.025_pct = ((hatQ_0.025 - previous_truth) / previous_truth),
    hatQ_0.25_pct = ((hatQ_0.25 - previous_truth) / previous_truth),
    hatQ_0.5_pct = ((hatQ_0.5 - previous_truth) / previous_truth), 
    hatQ_0.75_pct = ((hatQ_0.75 - previous_truth) / previous_truth), 
    hatQ_0.975_pct = ((hatQ_0.975 - previous_truth) / previous_truth), 
    hatQ_1_pct = ((hatQ_1 - previous_truth) / previous_truth), 
    
    classic_lo = ((classic_lo - previous_truth)), 
    classic_med = ((classic_med - previous_truth)), 
    classic_upr = ((classic_upr - previous_truth)), 
    hatQ_0 = ((hatQ_0 - previous_truth)), 
    hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
    hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
    hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
    hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
    hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
    hatQ_1 = ((hatQ_1 - previous_truth))
  )

pr_mean_simulations_delta_sf <- pr_mean_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(pyromes, by = c("RegionName" = "FPU_CODE")) %>% 
  st_as_sf()









cdd_simulations <- climate_simulator(model = mod_usfs2, 
                                         response = "cdd", 
                                         response_index = 2,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = cdd_truth) %>%
               distinct())

cdd_simulations[,-c(1:2)] <- cdd_simulations[,-c(1:2)]^2


cdd_simulations_delta <- cdd_simulations %>%
  dplyr::mutate( 
    classic_lo_pct = ((classic_lo - previous_truth) / previous_truth), 
    classic_med_pct = ((classic_med - previous_truth) / previous_truth), 
    classic_upr_pct = ((classic_upr - previous_truth) / previous_truth), 
    hatQ_0_pct = ((hatQ_0 - previous_truth) / previous_truth), 
    hatQ_0.025_pct = ((hatQ_0.025 - previous_truth) / previous_truth),
    hatQ_0.25_pct = ((hatQ_0.25 - previous_truth) / previous_truth),
    hatQ_0.5_pct = ((hatQ_0.5 - previous_truth) / previous_truth), 
    hatQ_0.75_pct = ((hatQ_0.75 - previous_truth) / previous_truth), 
    hatQ_0.975_pct = ((hatQ_0.975 - previous_truth) / previous_truth), 
    hatQ_1_pct = ((hatQ_1 - previous_truth) / previous_truth), 
    
    classic_lo = ((classic_lo - previous_truth)), 
    classic_med = ((classic_med - previous_truth)), 
    classic_upr = ((classic_upr - previous_truth)), 
    hatQ_0 = ((hatQ_0 - previous_truth)), 
    hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
    hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
    hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
    hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
    hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
    hatQ_1 = ((hatQ_1 - previous_truth))
  )

cdd_simulations_delta_sf <- cdd_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(pyromes, by = c("RegionName" = "FPU_CODE")) %>% 
  st_as_sf()








cwd_simulations <- climate_simulator(model = mod_usfs2, 
                                     response = "cwd", 
                                     response_index = 1,
                                     training_df = final_train,
                                     prediction_df = final_test, 
                                     N = 10000, 
                                     grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = cwd_truth) %>%
               distinct())

cwd_simulations[,-c(1:2)] <- cwd_simulations[,-c(1:2)]^2


cwd_simulations_delta <- cwd_simulations %>%
  dplyr::mutate( 
    classic_lo_pct = ((classic_lo - previous_truth) / previous_truth), 
    classic_med_pct = ((classic_med - previous_truth) / previous_truth), 
    classic_upr_pct = ((classic_upr - previous_truth) / previous_truth), 
    hatQ_0_pct = ((hatQ_0 - previous_truth) / previous_truth), 
    hatQ_0.025_pct = ((hatQ_0.025 - previous_truth) / previous_truth),
    hatQ_0.25_pct = ((hatQ_0.25 - previous_truth) / previous_truth),
    hatQ_0.5_pct = ((hatQ_0.5 - previous_truth) / previous_truth), 
    hatQ_0.75_pct = ((hatQ_0.75 - previous_truth) / previous_truth), 
    hatQ_0.975_pct = ((hatQ_0.975 - previous_truth) / previous_truth), 
    hatQ_1_pct = ((hatQ_1 - previous_truth) / previous_truth), 
    
    classic_lo = ((classic_lo - previous_truth)), 
    classic_med = ((classic_med - previous_truth)), 
    classic_upr = ((classic_upr - previous_truth)), 
    hatQ_0 = ((hatQ_0 - previous_truth)), 
    hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
    hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
    hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
    hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
    hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
    hatQ_1 = ((hatQ_1 - previous_truth))
  )

cwd_simulations_delta_sf <- cwd_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(pyromes, by = c("RegionName" = "FPU_CODE")) %>% 
  st_as_sf()







wet_days_percent_simulations <- climate_simulator(model = mod_usfs2, 
                                     response = "wet_days_percent", 
                                     response_index = 3,
                                     training_df = final_train,
                                     prediction_df = final_test, 
                                     N = 10000, 
                                     grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = wet_days_percent_truth) %>%
               distinct())

wet_days_percent_simulations[,-c(1:2)] <- wet_days_percent_simulations[,-c(1:2)]^2


wet_days_percent_simulations_delta <- wet_days_percent_simulations %>%
  dplyr::mutate( 
    classic_lo_pct = ((classic_lo - previous_truth) / previous_truth), 
    classic_med_pct = ((classic_med - previous_truth) / previous_truth), 
    classic_upr_pct = ((classic_upr - previous_truth) / previous_truth), 
    hatQ_0_pct = ((hatQ_0 - previous_truth) / previous_truth), 
    hatQ_0.025_pct = ((hatQ_0.025 - previous_truth) / previous_truth),
    hatQ_0.25_pct = ((hatQ_0.25 - previous_truth) / previous_truth),
    hatQ_0.5_pct = ((hatQ_0.5 - previous_truth) / previous_truth), 
    hatQ_0.75_pct = ((hatQ_0.75 - previous_truth) / previous_truth), 
    hatQ_0.975_pct = ((hatQ_0.975 - previous_truth) / previous_truth), 
    hatQ_1_pct = ((hatQ_1 - previous_truth) / previous_truth), 
    
    classic_lo = ((classic_lo - previous_truth)), 
    classic_med = ((classic_med - previous_truth)), 
    classic_upr = ((classic_upr - previous_truth)), 
    hatQ_0 = ((hatQ_0 - previous_truth)), 
    hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
    hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
    hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
    hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
    hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
    hatQ_1 = ((hatQ_1 - previous_truth))
  )

wet_days_percent_simulations_delta_sf <- wet_days_percent_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(pyromes, by = c("RegionName" = "FPU_CODE")) %>% 
  st_as_sf()




# ACCURACY SUMMARIES ------------------------------------------------------

tas_mean_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))


pr_mean_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))

cwd_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))

cdd_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))

wet_days_percent_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))



