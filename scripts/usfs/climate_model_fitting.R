source("lib/climate_simulator.R")

replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}

narr_statistics <- readRDS("narr_statistics_pyromes.rds")
cmip5_pr_historical <- readRDS("cmip5_daily_pr_statistics_historical_pyromes.rds")
cmip5_tas_historical <- readRDS("cmip5_daily_tas_statistics_historical_pyromes.rds")
cmip5_pr_rcp85 <- readRDS("cmip5_daily_pr_statistics_rcp85_pyromes.rds")
cmip5_tas_rcp85 <- readRDS("cmip5_daily_tas_statistics_rcp85_pyromes.rds")
cmip5_pr_rcp45 <- readRDS("cmip5_daily_pr_statistics_rcp45_pyromes.rds")
cmip5_tas_rcp45 <- readRDS("cmip5_daily_tas_statistics_rcp45_pyromes.rds")


# so we know we have gcms that overlap for projecting. 
gcms_future <- cmip5_tas_rcp45 %>% 
  select(Year, gcm) %>% distinct() %>%
  select(gcm) %>%
  distinct() %>% inner_join( 
    cmip5_tas_rcp85 %>% 
      select(Year, gcm) %>% distinct() %>%
      select(gcm) %>%
      distinct()) %>% 
      inner_join(cmip5_tas_historical %>% 
                   select(Year, gcm) %>% distinct() %>%
                   select(gcm) %>%
                   distinct())



pyromes <- sf::st_read("Pyrome_20150605/Pyrome_20150605.shp")%>% 
  rmapshaper::ms_simplify() %>% 
  sf::st_transform("+init=epsg:4326")

pyromes_centroids <- sf::st_centroid(pyromes) %>%
  select(FPU_CODE, geometry) %>%
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
                cwd_error = cwd - cwd_truth , 
                wet_days_percent_error = wet_days_percent - wet_days_percent_truth) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(pyromes_centroids, by = c("RegionName"= "FPU_CODE")) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))


final_train <- train %>% 
  filter(gcm %in% gcms_future$gcm) %>%
  mutate(gcm = as.factor(gcm), 
         gcm_ic = as.factor(gcm_ic), 
         Season = as.factor(Season)) %>%
  distinct()

# FIT MODELS --------------------------------------------------------------

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

mod_obj <- list(mod1 = mod_usfs1, mod2 = mod_usfs2, 
                final_train = final_train)

saveRDS(mod_obj, "mod_usfs12_obj.rds")








