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
  dplyr::filter(Year >= 1979 & Year <= 2005) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_tas_test <- cmip5_tas_rcp85 %>% 
  dplyr::filter(Year >= 2016 & Year <= 2042) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)


features_pr_train <- cmip5_pr_historical %>% 
  dplyr::filter(Year >= 1979 & Year <= 2005) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_pr_test <- cmip5_pr_rcp85 %>% 
  dplyr::filter(Year >= 2016 & Year <= 2042) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)


truth_train <- narr_statistics %>%
  dplyr::filter(Year >= 1979 & Year <= 2005)  %>% 
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

# 
# truth_test <- narr_statistics %>%
#   dplyr::filter(Year >= 1999 & Year <= 2018) %>% 
#   group_by(RegionName, Month) %>%
#   summarise(tas_mean = mean(tas_mean), 
#             heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
#             cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
#             pr_mean = mean(sqrt(pr_mean)),
#             pr_max = max(sqrt(pr_max)), 
#             cdd = sqrt(mean(replace_inf(cdd))), 
#             cwd = sqrt(mean(replace_inf(cwd)))) %>% 
#   ungroup() 



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



# 
# test <- features_test %>% 
#   inner_join(truth_test %>% 
#                dplyr::rename(tas_mean_truth = tas_mean, 
#                              pr_mean_truth = pr_mean, 
#                              heating_degree_days_truth = heating_degree_days, 
#                              cooling_degree_days_truth = cooling_degree_days,
#                              pr_max_truth = pr_max, 
#                              cdd_truth = cdd, 
#                              cwd_truth = cwd),  
#              by = c("RegionName", "Month")) %>%
#   ### get errors too
#   dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
#                 pr_mean_error = pr_mean - pr_mean_truth, 
#                 heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
#                 cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
#                 pr_max_error = pr_max - pr_max_truth, 
#                 cdd_error = cdd - cdd_truth, 
#                 cwd_error = cwd - cwd_truth ) %>%
#   arrange(RegionName, gcm_ic) %>%
#   inner_join(hu4_centroids, by = c("RegionName"= "NAME")) %>%
#   mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))


shared_meta <- inner_join(
  x = train %>% 
    select(RegionName, gcm, ic, gcm_ic) %>% 
    distinct(), 
  y = features_test %>%
    select(RegionName, gcm, ic, gcm_ic) %>% 
    distinct()
)

final_train <- train %>% 
  inner_join(shared_meta) %>%
  mutate(gcm = as.factor(gcm), 
         gcm_ic = as.factor(gcm_ic), 
         Season = as.factor(Season)) %>%
  distinct()

final_test <- features_test %>% 
  inner_join(shared_meta) %>%
  mutate(gcm = as.factor(gcm), 
         gcm_ic = as.factor(gcm_ic), 
         Season = as.factor(Season)) %>%
  distinct()

# FIT MODELS --------------------------------------------------------------
t1 <- Sys.time()
mod_jba1 <- mgcv::gam(list(
  tas_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re"), 
  pr_max_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, Season, bs="re")
),
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train )
Sys.time() - t1

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
               select(RegionName, Season, previous_truth = tas_mean_truth) %>%
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
  dplyr::select(RegionName, Season, hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()

pr_mean_simulations <- climate_simulator(model = mod_means, 
                                         response = "pr_mean", 
                                         response_index = 2,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, Season, previous_truth = pr_mean_truth) %>%
               distinct()) %>%
  select(-truth)

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
  dplyr::select(RegionName, Season,  hatQ_0.5, classic_med) %>%
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
               select(RegionName, Season, previous_truth = pr_max_truth) %>%
               distinct()) %>%
  select(-truth)

pr_max_simulations[,-c(1:2)] <- pr_max_simulations[,-c(1:2)]^2

pr_max_simulations_delta <- pr_max_simulations %>%
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

pr_max_simulations_delta_sf <- pr_max_simulations_delta %>%
  dplyr::select(RegionName, Season, hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()




heating_degree_days_simulations <- climate_simulator(model = mod_degrees, 
                                         response = "heating_degree_days", 
                                         response_index = 1,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, Season, previous_truth = heating_degree_days_truth) %>%
               distinct()) %>%
  select(-truth)

heating_degree_days_simulations[,-c(1:2)] <- heating_degree_days_simulations[,-c(1:2)]^2

heating_degree_days_simulations_delta <- heating_degree_days_simulations %>%
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

heating_degree_days_simulations_delta_sf <- heating_degree_days_simulations_delta %>%
  dplyr::select(RegionName, Season, hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



cooling_degree_days_simulations <- climate_simulator(model = mod_degrees, 
                                         response = "cooling_degree_days", 
                                         response_index = 2,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, Season, previous_truth = cooling_degree_days_truth) %>%
               distinct()) %>%
  select(-truth)

cooling_degree_days_simulations[,-c(1:2)] <- cooling_degree_days_simulations[,-c(1:2)]^2

cooling_degree_days_simulations_delta <- cooling_degree_days_simulations %>%
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

cooling_degree_days_simulations_delta_sf <- cooling_degree_days_simulations_delta %>%
  dplyr::select(RegionName, Season, hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()




cdd_simulations <- climate_simulator(model = mod_consec, 
                                         response = "cdd", 
                                         response_index =1,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, Season, previous_truth = cdd_truth) %>%
               distinct()) %>%
  select(-truth)

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
  dplyr::select(RegionName,Season,  hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()


cwd_simulations <- climate_simulator(model = mod_consec, 
                                         response = "cwd", 
                                         response_index = 2,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_vars = c("RegionName", "Season")) %>% 
  inner_join(final_train %>%
               select(RegionName, Season, previous_truth = cwd_truth) %>%
               distinct()) %>%
  select(-truth)

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
  dplyr::select(RegionName, Season, hatQ_0.5, classic_med) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()




# SAVE OUT AN RDS MODEL PKG, WITH EVERYTHING -----------------------------------------------

model_pkg <- list(mod_means = mod_means, 
     mod_pr = mod_pr,
     mod_consec = mod_consec, 
     mod_degrees = mod_degrees, 
     final_train = final_train, 
     final_test = final_test, 
     tas_mean_simulations = tas_mean_simulations, 
     pr_mean_simulations = pr_mean_simulations, 
     pr_max_simulations = pr_max_simulations, 
     heating_degree_days_simulations = heating_degree_days_simulations, 
     cooling_degree_days_simulations = cooling_degree_days_simulations,
     cdd_simulations = cdd_simulations, 
     tas_mean_simulations_delta = tas_mean_simulations_delta, 
     pr_mean_simulations_delta = pr_mean_simulations_delta, 
     pr_max_simulations_delta = pr_max_simulations_delta, 
     heating_degree_days_simulations_delta = heating_degree_days_simulations_delta, 
     cooling_degree_days_simulations_delta = cooling_degree_days_simulations_delta,
     cdd_simulations_delta = cdd_simulations_delta 
     )




# MATRIX SUMMARY ----------------------------------------------------------

tas_mean_simulations_delta %>% 
  select(RegionName, Season, MeanTemp = hatQ_0.5) %>%
  inner_join(pr_mean_simulations_delta %>% 
               select(RegionName, Season, MeanPrecip = hatQ_0.5)) %>%
  inner_join(pr_max_simulations_delta %>% 
               select(RegionName, Season, MaxPrecip = hatQ_0.5)) %>%
  inner_join(cooling_degree_days_simulations_delta %>% 
               select(RegionName, Season, CoolingDegreeDays = hatQ_0.5)) %>%
  inner_join(heating_degree_days_simulations_delta %>% 
               select(RegionName, Season, HeatingDegreeDays = hatQ_0.5)) %>%
  inner_join(cdd_simulations_delta %>% 
               select(RegionName, Season, ConsecDryDays = hatQ_0.5)) %>%
  inner_join(cwd_simulations_delta %>% 
               select(RegionName, Season, ConsecWetDays = hatQ_0.5)) %>%
  group_by(Season) %>%
  summarise(
            MeanTemp = mean(MeanTemp),
            MeanPrecip = mean(MeanPrecip), 
            MaxPrecip = mean(MaxPrecip), 
            CoolingDegreeDays = mean(CoolingDegreeDays), 
            HeatingDegreeDays = mean(HeatingDegreeDays),
            ConsecDryDays = mean(ConsecDryDays),
            ConsecWetDays = mean(ConsecWetDays)
            )


tas_mean_simulations_delta %>% 
  select(RegionName, Season, MeanTemp = previous_truth) %>%
  inner_join(pr_mean_simulations_delta %>% 
               select(RegionName, Season, MeanPrecip = previous_truth)) %>%
  inner_join(pr_max_simulations_delta %>% 
               select(RegionName, Season, MaxPrecip = previous_truth)) %>%
  inner_join(cooling_degree_days_simulations_delta %>% 
               select(RegionName, Season, CoolingDegreeDays = previous_truth)) %>%
  inner_join(heating_degree_days_simulations_delta %>% 
               select(RegionName, Season, HeatingDegreeDays = previous_truth)) %>%
  inner_join(cdd_simulations_delta %>% 
               select(RegionName, Season, ConsecDryDays = previous_truth)) %>%
  inner_join(cwd_simulations_delta %>% 
               select(RegionName, Season, ConsecWetDays = previous_truth)) %>%
  group_by(Season) %>%
  summarise(
    MeanTemp = mean(MeanTemp),
    MeanPrecip = mean(MeanPrecip), 
    MaxPrecip = mean(MaxPrecip), 
    CoolingDegreeDays = mean(CoolingDegreeDays), 
    HeatingDegreeDays = mean(HeatingDegreeDays),
    ConsecDryDays = mean(ConsecDryDays),
    ConsecWetDays = mean(ConsecWetDays)
  )
  


tas_mean_simulations_delta %>% 
  select(RegionName, Season, MeanTemp = classic_med) %>%
  inner_join(pr_mean_simulations_delta %>% 
               select(RegionName, Season, MeanPrecip = classic_med)) %>%
  inner_join(pr_max_simulations_delta %>% 
               select(RegionName, Season, MaxPrecip = classic_med)) %>%
  inner_join(cooling_degree_days_simulations_delta %>% 
               select(RegionName, Season, CoolingDegreeDays = classic_med)) %>%
  inner_join(heating_degree_days_simulations_delta %>% 
               select(RegionName, Season, HeatingDegreeDays = classic_med)) %>%
  inner_join(cdd_simulations_delta %>% 
               select(RegionName, Season, ConsecDryDays = classic_med)) %>%
  inner_join(cwd_simulations_delta %>% 
               select(RegionName, Season, ConsecWetDays = classic_med)) %>%
  group_by(Season) %>%
  summarise(
    MeanTemp = mean(MeanTemp),
    MeanPrecip = mean(MeanPrecip), 
    MaxPrecip = mean(MaxPrecip), 
    CoolingDegreeDays = mean(CoolingDegreeDays), 
    HeatingDegreeDays = mean(HeatingDegreeDays),
    ConsecDryDays = mean(ConsecDryDays),
    ConsecWetDays = mean(ConsecWetDays)
  )



# CHANGE SUMMARIES AND MAPS -----------------------------------------------
# Cooling degree days for JJA

pal <- colorNumeric(palette = "Spectral", 
                    domain = range(cooling_degree_days_simulations_delta_sf %>% 
                                     filter(Season == "JJA") %>% .$hatQ_0.5), reverse = TRUE)


cooling_degree_days_simulations_delta_sf %>% 
  filter(Season == "JJA") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Cooling Degree Days, JJA")


pal <- colorNumeric(palette = "Spectral", 
                    domain = range(cooling_degree_days_simulations_delta_sf %>% 
                                     filter(Season == "JJA") %>% .$classic_med), reverse = TRUE)


cooling_degree_days_simulations_delta_sf %>% 
  filter(Season == "JJA") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(classic_med), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ classic_med, title = "Change, Cooling Degree Days, JJA")

# hdd djf
pal <- colorNumeric(palette = "Spectral", 
                    domain = range(heating_degree_days_simulations_delta_sf %>% 
                                     filter(Season == "DJF") %>% .$hatQ_0.5), reverse = FALSE)


heating_degree_days_simulations_delta_sf %>% 
  filter(Season == "DJF") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black', 
              popup = ~ hatQ_0.5) %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Heating Degree Days, DJF")


# cdd mam
pal <- colorNumeric(palette = "BrBG", 
                    domain = range(cdd_simulations_delta_sf %>% 
                                     filter(Season == "MAM") %>% .$hatQ_0.5), reverse = TRUE)


cdd_simulations_delta_sf %>% 
  filter(Season == "MAM") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Consecutive<br>Dry Days, MAM")



# cwd mam
pal <- colorNumeric(palette = "BrBG", 
                    domain = c(-3,3), reverse = FALSE)


cwd_simulations_delta_sf %>% 
  filter(Season == "MAM" & hatQ_0.5 > -3 & hatQ_0.5 < 3) %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Consecutive<br>Wet Days, MAM")



# pr_max mam
pal <- colorNumeric(palette = "BrBG", 
                    domain = range(pr_max_simulations_delta_sf %>% 
                                     filter(Season == "JJA") %>% .$hatQ_0.5), reverse = FALSE)


pr_max_simulations_delta_sf %>% 
  filter(Season == "JJA") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Avg. Max<br>Daily Precip, JJA")



# pr_max mam
pal <- colorNumeric(palette = "Spectral", 
                    domain = range(tas_mean_simulations_delta_sf %>% 
                                     filter(Season == "DJF") %>% .$hatQ_0.5), reverse = TRUE)


tas_mean_simulations_delta_sf %>% 
  filter(Season == "DJF") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Mean<br>Temperature, DJF")


# pr_mean mam
pal <- colorNumeric(palette = "BrBG", 
                    domain = range(pr_mean_simulations_delta_sf %>% 
                                     filter(Season == "MAM") %>% .$hatQ_0.5), reverse = FALSE)


pr_mean_simulations_delta_sf %>% 
  filter(Season == "MAM") %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend("bottomright", pal = pal, values = ~ hatQ_0.5, title = "Change, Mean<br>Precipitation, MAM")





# SUMMARY SPATIAL CHANGES ---------------------------------------------------------
tas_mean_simulations_delta%>%
  select(RegionName, Season, hatQ_0.5, previous_truth) %>%

  ggplot2::ggplot(aes(x= Season, y = RegionName)) + 
  geom_tile(aes(fill = hatQ_0.5, colour = hatQ_0.5)) +
  scale_fill_gradient2(low="blue", high = "red", mid = "white", midpoint = 0) + 
  scale_colour_gradient2(low="blue", high = "red", mid = "white", midpoint = 0) +
  theme(axis.text.y = element_text(size=3))

pr_max_simulations_delta












