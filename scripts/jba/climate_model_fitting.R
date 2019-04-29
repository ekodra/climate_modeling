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



features_pr_train <- cmip5_pr_historical %>% 
  dplyr::filter(Year >= 1979 & Year <= 2005) %>% 
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


mod_obj <- list(mod = mod_jba1, final_train = final_train, shared_meta = shared_meta)


saveRDS(mod_obj, "mod_jba1_obj.rds")








