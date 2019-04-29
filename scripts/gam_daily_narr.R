source("lib/climate_simulator.R")

replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}


narr_statistics <- readRDS("narr_statistics.rds")

cmip5_pr_historical <- readRDS("cmip5_daily_pr_statistics_historical.rds")
cmip5_tas_historical <- readRDS("cmip5_daily_tas_statistics_historical.rds")
cmip5_pr_rcp85 <- readRDS("cmip5_daily_pr_statistics_rcp85.rds")
cmip5_tas_rcp85 <- readRDS("cmip5_daily_tas_statistics_rcp85.rds")


hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/") %>% 
  rmapshaper::ms_simplify()

hu4_centroids <- sf::st_centroid(hu4) %>%
  select(NAME, geometry=Shape) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)


# split with 20-20 years in each. one thing to consider is that rcp85 has an assumed trajectory
# and does leak into the years 2006:2018, so it's not a completely clean "backtest"
# 

features_tas_train <- cmip5_tas_historical %>% 
  dplyr::filter(Year >= 1979 & Year <= 1998) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)


features_tas_test <- cmip5_tas_rcp85 %>% 
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


features_pr_test <- cmip5_pr_rcp85 %>% 
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
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
  ungroup() 



train <- features_train %>% 
  inner_join(truth_train %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max, 
                             cdd_truth = cdd, 
                             cwd_truth = cwd),  
             by = c("RegionName", "Season")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth, 
                cdd_error = cdd - cdd_truth, 
                cwd_error = cwd - cwd_truth ) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(hu4_centroids, by = c("RegionName"= "NAME")) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))




test <- features_test %>% 
  inner_join(truth_test %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max, 
                             cdd_truth = cdd, 
                             cwd_truth = cwd),  
             by = c("RegionName", "Season")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth, 
                cdd_error = cdd - cdd_truth, 
                cwd_error = cwd - cwd_truth ) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(hu4_centroids, by = c("RegionName"= "NAME")) %>%
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

# FIT MODELS --------------------------------------------------------------
t1 <- Sys.time()
mod_means <- mgcv::gam(list(
  tas_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1

t1 <- Sys.time()
mod_degrees <- mgcv::gam(list(
  heating_degree_days_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  cooling_degree_days_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train  )
Sys.time() - t1


t1 <- Sys.time()
mod_pr <- mgcv::gam(list(
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  pr_max_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1

t1 <- Sys.time()
mod_consec <- mgcv::gam(list(
  cdd_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  cwd_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1


# RUN CLIMATE SIMULATOR ---------------------------------------------------
tas_mean_simulations <- climate_simulator(model = mod_means, 
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
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

tas_mean_simulations_delta_sf <- tas_mean_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()
cor(tas_mean_simulations[,-c(1:2)])

pr_mean_simulations <- climate_simulator(model = mod_means, 
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
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

pr_mean_simulations_delta_sf <- pr_mean_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()


pr_max_simulations <- climate_simulator(model = mod_pr, 
                                         response = "pr_max", 
                                         response_index = 2,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = pr_max_truth) %>%
               distinct())

pr_max_simulations[,-c(1:2)] <- pr_max_simulations[,-c(1:2)]^2



pr_max_simulations_delta <- pr_max_simulations %>%
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

pr_max_simulations_delta_sf <- pr_max_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



cor(pr_max_simulations[,-c(1:2)])


heating_degree_days_simulations <- climate_simulator(model = mod_degrees, 
                                        response = "heating_degree_days", 
                                        response_index = 1,
                                        training_df = final_train,
                                        prediction_df = final_test, 
                                        N = 10000, 
                                        grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = heating_degree_days_truth) %>%
               distinct())

heating_degree_days_simulations[,-c(1:2)] <- heating_degree_days_simulations[,-c(1:2)]^2



heating_degree_days_simulations_delta <- heating_degree_days_simulations %>%
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

heating_degree_days_simulations_delta_sf <- heating_degree_days_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



cor(heating_degree_days_simulations[,-c(1:2)])




cooling_degree_days_simulations <- climate_simulator(model = mod_degrees, 
                                                     response = "cooling_degree_days", 
                                                     response_index = 2,
                                                     training_df = final_train,
                                                     prediction_df = final_test, 
                                                     N = 10000, 
                                                     grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = cooling_degree_days_truth) %>%
               distinct())

cooling_degree_days_simulations[,-c(1:2)] <- cooling_degree_days_simulations[,-c(1:2)]^2



cooling_degree_days_simulations_delta <- cooling_degree_days_simulations %>%
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

cooling_degree_days_simulations_delta_sf <- cooling_degree_days_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()


cor(cooling_degree_days_simulations[,-c(1:2)])





cdd_simulations <- climate_simulator(model = mod_consec, 
                                                     response = "cdd", 
                                                     response_index = 1,
                                                     training_df = final_train,
                                                     prediction_df = final_test, 
                                                     N = 10000, 
                                                     grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = cdd_truth) %>%
               distinct())

cdd_simulations[,-c(1:2)] <- cdd_simulations[,-c(1:2)]^2




cdd_simulations_delta <- cdd_simulations %>%
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

cdd_simulations_delta_sf <- cdd_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



cor(cdd_simulations[,-c(1:2)])


cwd_simulations <- climate_simulator(model = mod_consec, 
                                     response = "cwd", 
                                     response_index = 2,
                                     training_df = final_train,
                                     prediction_df = final_test, 
                                     N = 10000, 
                                     grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, previous_truth = cwd_truth) %>%
               distinct())

cwd_simulations[,-c(1:2)] <- cwd_simulations[,-c(1:2)]^2


cwd_simulations_delta <- cwd_simulations %>%
  dplyr::mutate(delta_truth = ((truth - previous_truth)), 
                classic_lo = ((classic_lo - previous_truth)), 
                classic_med = ((classic_med - previous_truth)), 
                classic_upr = ((classic_upr - previous_truth)), 
                hatQ_0 = ((hatQ_0 - previous_truth)), 
                hatQ_0.025 = ((hatQ_0.025 - previous_truth)),
                hatQ_0.25 = ((hatQ_0.25 - previous_truth)),
                hatQ_0.5 = ((hatQ_0.5 - previous_truth)), 
                hatQ_0.75 = ((hatQ_0.75 - previous_truth)), 
                hatQ_0.975 = ((hatQ_0.975 - previous_truth)), 
                hatQ_1 = ((hatQ_1 - previous_truth))) %>%
  select(-truth, -previous_truth)

cwd_simulations_delta_sf <- cwd_simulations_delta %>%
  dplyr::select(RegionName, hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



cor(cwd_simulations[,-c(1:2)])




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


pr_max_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))


cooling_degree_days_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))


heating_degree_days_simulations %>% 
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
# 
# cdd_simulations %>% 
#   group_by(Season) %>%
#   summarise(mean(truth), sd(truth), sd(truth)/mean(truth))

cdd_simulations %>% 
  #mutate(Season = as.numeric(Season)) %>%
  group_by(Season) %>% 
  summarise(RMSE_Ratio = rmse(truth, hatQ_0.5) / rmse(truth, classic_med), 
            risQ_Coverage = coverage(truth, hatQ_0.025, hatQ_0.975), 
            ensemble_Coverage = coverage(truth, classic_lo, classic_upr))

  

hist(cdd_simulations$truth)

