source("lib/climate_simulator.R")
library(leaflet)
library(mapview)
replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}
replace_na <- function(x){
  x[is.na(x)] <- 0
  x
}


narr_statistics <- readRDS("narr_statistics_pyromes.rds")
cmip5_pr_historical <- readRDS("cmip5_daily_pr_statistics_historical_pyromes.rds")
cmip5_tas_historical <- readRDS("cmip5_daily_tas_statistics_historical_pyromes.rds")
cmip5_pr_rcp85 <- readRDS("cmip5_daily_pr_statistics_rcp85_pyromes.rds")
cmip5_tas_rcp85 <- readRDS("cmip5_daily_tas_statistics_rcp85_pyromes.rds")
cmip5_pr_rcp45 <- readRDS("cmip5_daily_pr_statistics_rcp45_pyromes.rds")
cmip5_tas_rcp45 <- readRDS("cmip5_daily_tas_statistics_rcp45_pyromes.rds")

pyromes <- sf::st_read("Pyrome_20150605/Pyrome_20150605.shp")%>% 
 rmapshaper::ms_simplify() %>% 
  sf::st_transform("+init=epsg:4326")

pyromes_centroids <- sf::st_centroid(pyromes) %>%
  select(FPU_CODE, geometry) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)


# CREATE LISTS OF NON OVERLAPPING DATA FRAMES, WILL DO INTERPOLATION BETWEEN THEM --------
features_tas_rcp85 <- cmip5_tas_rcp85 %>% 
  dplyr::filter(Year >= 2006) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_pr_rcp85 <- cmip5_pr_rcp85 %>% 
  dplyr::filter(Year >= 2006) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_tas_rcp45 <- cmip5_tas_rcp45 %>% 
  dplyr::filter(Year >= 2006) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)

features_pr_rcp45 <- cmip5_pr_rcp45 %>% 
  dplyr::filter(Year >= 2006) %>% 
  dplyr::mutate(Season = case_when(Month %in% c("3", "4", "5") ~ "MAM", 
                                   Month %in% c("12", "1", "2") ~ "DJF",
                                   Month %in% c("9", "10", "11") ~ "SON",
                                   Month %in% c("6", "7", "8") ~ "JJA")) %>% 
  select(-Month)



features_rcp85 <- features_tas_rcp85 %>%
  inner_join(features_pr_rcp85, 
             by = c("RegionName", "Year", "Season", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(replace_na(sqrt(pr_mean))),
            cdd = max(sqrt(cdd)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup()  


features_rcp45 <- features_tas_rcp45 %>%
  inner_join(features_pr_rcp45, 
             by = c("RegionName", "Year", "Season", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic, Season) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(replace_na(sqrt(pr_mean))),
            cdd = max(sqrt(cdd)), 
            cdd = sqrt(mean(replace_inf(cdd))), 
            cwd = sqrt(mean(replace_inf(cwd))), 
            wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
  ungroup() 

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


# TIME SLICES -------------------------------------------------------------


year_starts <- seq(1979, 2071, by = 5)
year_ends <- seq(2008, 2100, by = 5)
climatology_labels <- purrr::map2(.x = year_starts, .y = year_ends, .f = ~ median(c(.x, .y))) %>%
  unlist() %>% 
  c() %>% 
  ceiling()


# load model obj
mod_usfs12_obj <- readRDS("mod_usfs12_obj.rds")
orig_train <- mod_usfs12_obj$final_train
mod1 <- mod_usfs12_obj$mod1 # tas_mean, pr_mean
mod2 <- mod_usfs12_obj$mod2 # cwd, cdd, wdp

multiClimateSimulator <- function(start_year = year_starts[1], 
                                  end_year = year_ends[1], 
                                  df_tas = features_tas_rcp45,
                                  df_pr = features_pr_rcp45,
                                  original_training_df = orig_train, 
                                  bias_model = mod, 
                                  varnames = c("tas_mean", "pr_mean", "cdd")){
  
  tmp_df <- df_tas %>% 
    dplyr::filter(Year >= start_year & Year <= end_year) %>% 
    inner_join(df_pr %>% 
                 dplyr::filter(Year >= start_year & Year <= end_year), 
               by = c("RegionName", "Year", "Season", "gcm", "ic", "gcm_ic")) %>%
    group_by(RegionName, gcm, ic, gcm_ic, Season) %>%
    summarise(tas_mean = mean(tas_mean), 
              heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
              cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
              pr_mean = mean(replace_na(sqrt(pr_mean))),
              cdd = max(sqrt(cdd)), 
              cdd = sqrt(mean(replace_inf(cdd))), 
              cwd = sqrt(mean(replace_inf(cwd))), 
              wet_days_percent = sqrt(mean(wet_days_percent))) %>% 
    ungroup() %>%
    mutate()
  
  pred_df <- dplyr::inner_join(tmp_df, 
                               original_training_df %>% 
                                 dplyr::select(RegionName, gcm, ic, gcm_ic, Season) %>% 
                                 dplyr::distinct(.), 
                               by = c("RegionName", "gcm", "ic", "gcm_ic", "Season")) %>% 
    mutate(gcm = as.factor(gcm), 
           gcm_ic = as.factor(gcm_ic), 
           Season = as.factor(Season)
    )
  
  relevant_training_df <- dplyr::inner_join(original_training_df, 
                                            tmp_df %>% 
                                              dplyr::select(RegionName, gcm, ic, gcm_ic, Season) %>% 
                                              dplyr::distinct(.), 
                                            by = c("RegionName", "gcm", "ic", "gcm_ic", "Season")) %>% 
    mutate(gcm = as.factor(gcm), 
           gcm_ic = as.factor(gcm_ic), 
           Season = as.factor(Season)
    )
  
  
  # run simulator
  # t1 <- Sys.time()
  simulations <- purrr::map2(.x = varnames, .y = 1:length(varnames),
                             .f= ~ climate_simulator(model = bias_model, 
                                                     response = .x, 
                                                     response_index = .y,
                                                     training_df = relevant_training_df,
                                                     prediction_df = pred_df, 
                                                     N = 10000, 
                                                     grouping_vars = c("RegionName", "Season"))
  )
  # Sys.time() - t1
  
  names(simulations) <- varnames
  
  return(simulations)
  
}


# run simulator over all regimes via purrr

rcp45_projections_1 <- purrr::map2(.x = year_starts, .y = year_ends, 
                                 .f = ~multiClimateSimulator(start_year = .x, 
                                                             end_year = .y, 
                                                             df_tas = features_tas_rcp45, 
                                                             df_pr = features_pr_rcp45,
                                                             bias_model = mod1,
                                                             varnames = c("tas_mean", "pr_mean")
                                                             ))

names(rcp45_projections_1) <- paste("rcp45_climate1_", climatology_labels, sep='_')

rcp45_projections_2 <- purrr::map2(.x = year_starts, .y = year_ends, 
                                   .f = ~multiClimateSimulator(start_year = .x, 
                                                               end_year = .y, 
                                                               df_tas = features_tas_rcp45, 
                                                               df_pr = features_pr_rcp45,
                                                               bias_model = mod2,
                                                               varnames = c("cwd", "cdd", "wet_days_percent")
                                   ))

names(rcp45_projections_2) <- paste("rcp45_climate2_", climatology_labels, sep='_')



rcp85_projections_1 <- purrr::map2(.x = year_starts, .y = year_ends, 
                                   .f = ~multiClimateSimulator(start_year = .x, 
                                                               end_year = .y, 
                                                               df_tas = features_tas_rcp85, 
                                                               df_pr = features_pr_rcp85,
                                                               bias_model = mod1,
                                                               varnames = c("tas_mean", "pr_mean")
                                   ))

names(rcp85_projections_1) <- paste("rcp85_climate1_", climatology_labels, sep='_')

rcp85_projections_2 <- purrr::map2(.x = year_starts, .y = year_ends, 
                                   .f = ~multiClimateSimulator(start_year = .x, 
                                                               end_year = .y, 
                                                               df_tas = features_tas_rcp85, 
                                                               df_pr = features_pr_rcp85,
                                                               bias_model = mod2,
                                                               varnames = c("cwd", "cdd", "wet_days_percent")
                                   ))

names(rcp85_projections_2) <- paste("rcp85_climate2_", climatology_labels, sep='_')



# NEXT STEP IS INTERPOLATING EACH YEAR AND CLIMATE % CHANGE FACTOR! -------
# BASICALLY A SQL DF LIKE LOOKUP TO USE FOR JBA FILE MODIFICATION (% change factors)


rcp85_tas_mean_projections <- purrr::map2(.x = rcp85_projections_1, .y = climatology_labels, 
                                          .f = ~.x$tas_mean %>%
                                            dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 


rcp85_pr_mean_projections <- purrr::map2(.x = rcp85_projections_1, .y = climatology_labels, 
                                         .f = ~.x$pr_mean %>%
                                           dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))

rcp85_cwd_projections <- purrr::map2(.x = rcp85_projections_2, .y = climatology_labels, 
                                        .f = ~.x$cwd %>%
                                          dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 


rcp85_cdd_projections <- purrr::map2(.x = rcp85_projections_2, .y = climatology_labels, 
                                     .f = ~.x$cdd %>%
                                       dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 


rcp85_wet_days_percent_projections <- purrr::map2(.x = rcp85_projections_2, .y = climatology_labels, 
                                     .f = ~.x$wet_days_percent %>%
                                       dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))


rcp85_tas_mean_summary_raw <- rcp85_tas_mean_projections %>% 
  group_by(Year, Season) %>% 
  summarise(hatQ_0.5 = mean(hatQ_0.5), 
            hatQ_0.975 = mean(hatQ_0.975),
            hatQ_0.025 = mean(hatQ_0.025)) %>% 
  ungroup()

rcp85_wdp_summary_raw <- rcp85_wet_days_percent_projections %>% 
  group_by(Year, Season) %>% 
  summarise(hatQ_0.5 = mean(hatQ_0.5), 
            hatQ_0.975 = mean(hatQ_0.975),
            hatQ_0.025 = mean(hatQ_0.025)) %>% 
  ungroup()

rcp85_cdd_summary_raw <- rcp85_cdd_projections %>% 
  group_by(Year, Season) %>% 
  summarise(hatQ_0.5 = mean(hatQ_0.5), 
            hatQ_0.975 = mean(hatQ_0.975),
            hatQ_0.025 = mean(hatQ_0.025)) %>% 
  ungroup() 


rcp85_cwd_summary_raw <- rcp85_cwd_projections %>% 
  group_by(Year, Season) %>% 
  summarise(hatQ_0.5 = mean(hatQ_0.5), 
            hatQ_0.975 = mean(hatQ_0.975),
            hatQ_0.025 = mean(hatQ_0.025)) %>% 
  ungroup() 

rcp85_pr_mean_summary_raw <- rcp85_pr_mean_projections %>% 
  group_by(Year, Season) %>% 
  summarise(hatQ_0.5 = mean(hatQ_0.5), 
            hatQ_0.975 = mean(hatQ_0.975),
            hatQ_0.025 = mean(hatQ_0.025)) %>% 
  ungroup() 



summary(lm(hatQ_0.5 ~ Year + Season + Year:Season, data = rcp85_wdp_summary_raw))

summary(lm(hatQ_0.5 ~ Year + Season + Year:Season, data = rcp85_cdd_summary_raw))

summary(lm(hatQ_0.5 ~ Year + Season + Year:Season, data = rcp85_cwd_summary_raw))



rcp85_wdp_summary_raw %>%
  ggplot2::ggplot(aes(x = Year, y = hatQ_0.5^2, color = Season)) + 
  geom_line() + 
  geom_smooth(method = "lm")

rcp85_tas_mean_summary_raw %>%
  ggplot2::ggplot(aes(x = Year, y = hatQ_0.5, color = Season)) + 
  geom_line() + 
  geom_smooth(method = "lm")

rcp85_pr_mean_summary_raw %>%
  ggplot2::ggplot(aes(x = Year, y = hatQ_0.5^2, color = Season)) + 
  geom_line() + 
  geom_smooth(method = "lm")

rcp85_cdd_summary_raw %>%
  ggplot2::ggplot(aes(x = Year, y = hatQ_0.5^2, color = Season)) + 
  geom_line() + 
  geom_smooth(method = "lm")


rcp85_cwd_summary_raw %>%
  ggplot2::ggplot(aes(x = Year, y = hatQ_0.5^2, color = Season)) + 
  geom_line() + 
  geom_smooth(method = "lm")
# rcp85_tas_mean_projections %>%
#   group_by(Year) %>%
#   summarise(mean(hatQ_0.5))
# 
# rcp85_pr_mean_projections %>%
#   group_by(Year) %>%
#   summarise(mean(hatQ_0.5))

# 
# rcp85_cwd_projections %>%
#   group_by(Year) %>% 
#   summarise(mean(hatQ_0.5))
# 
# 
# rcp85_cdd_projections %>%
#   group_by(Year) %>% 
#   summarise(mean(hatQ_0.5))
# 
# rcp85_wet_days_percent_projections %>%
#   group_by(Year) %>% 
#   summarise(mean(hatQ_0.5))



rcp45_tas_mean_projections <- purrr::map2(.x = rcp45_projections_1, .y = climatology_labels, 
                                          .f = ~.x$tas_mean %>%
                                            dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))


rcp45_pr_mean_projections <- purrr::map2(.x = rcp45_projections_1, .y = climatology_labels, 
                                         .f = ~.x$pr_mean %>%
                                           dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 

rcp45_cwd_projections <- purrr::map2(.x = rcp45_projections_2, .y = climatology_labels, 
                                     .f = ~.x$cwd %>%
                                       dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 

rcp45_cdd_projections <- purrr::map2(.x = rcp45_projections_2, .y = climatology_labels, 
                                     .f = ~.x$cdd %>%
                                       dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))


rcp45_wet_days_percent_projections <- purrr::map2(.x = rcp45_projections_2, .y = climatology_labels, 
                                                  .f = ~.x$wet_days_percent %>%
                                                    dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(pyromes_centroids, by =c("RegionName" = "FPU_CODE")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))




# SMOOTH PREDICTIONS OVER SPACE JUST A BIT -------------------------------------
# experiment

## ^ isolates the spatial signal. then need to go back and apply to year?

t1 <- Sys.time()
mod_rcp85_tas_mean_projections_JJA <- mgcv::gam(list(
  # hatQ_0.025 ~ s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100),  
  # hatQ_0.5 ~  s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100),  
  # hatQ_0.975 ~ s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100)
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_tas_mean_projections %>% filter(Season == "JJA") )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_tas_mean_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_tas_mean_projections %>% filter(Season == "DJF" ))  
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_tas_mean_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE, 
control = gam.control(trace = TRUE), 
data = rcp85_tas_mean_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_tas_mean_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_tas_mean_projections %>% filter(Season == "MAM" )  )
Sys.time() - t1






#### tas_mean rcp85 compile smoothed projections ------------

d <- distinct(rcp85_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_tas_mean_projections_full_JJA <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_tas_mean_projections_JJA, newdata = . ))) 

baseline <- rcp85_tas_mean_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_tas_mean_projections_full_delta_JJA <- rcp85_tas_mean_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_tas_mean_projections_full_DJF <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_tas_mean_projections_DJF, newdata = . ))) 

baseline <- rcp85_tas_mean_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_tas_mean_projections_full_delta_DJF <- rcp85_tas_mean_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_tas_mean_projections_full_SON <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_tas_mean_projections_SON, newdata = . ))) 

baseline <- rcp85_tas_mean_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_tas_mean_projections_full_delta_SON <- rcp85_tas_mean_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp85_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_tas_mean_projections_full_MAM <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_tas_mean_projections_MAM, newdata = . ))) 

baseline <- rcp85_tas_mean_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_tas_mean_projections_full_delta_MAM <- rcp85_tas_mean_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp85_tas_mean_projections_full_delta <- purrr::reduce(.x = list(rcp85_tas_mean_projections_full_delta_JJA, 
                                                                rcp85_tas_mean_projections_full_delta_DJF, 
                                                                rcp85_tas_mean_projections_full_delta_MAM, 
                                                                rcp85_tas_mean_projections_full_delta_SON), 
                                                      .f = rbind)
rcp85_tas_mean_projections_full_delta$lwr <- apply(X = rcp85_tas_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = min, MARGIN = 1) 

rcp85_tas_mean_projections_full_delta$mle <- apply(X = rcp85_tas_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = median, MARGIN = 1) 

rcp85_tas_mean_projections_full_delta$upr <- apply(X = rcp85_tas_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_tas_mean_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_tas_mean_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_tas_mean_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_tas_mean_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_tas_mean_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_tas_mean_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_tas_mean_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_tas_mean_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp85_tas_mean_summary <- rcp85_tas_mean_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_tas_mean_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. tas_mean Change from 2019") +
  ggplot2::ylab("Deg C Change from 2019 Climate") 




#### REPEAT ALL THAT FOR PR_MEAN -------------

t1 <- Sys.time()
mod_rcp85_pr_mean_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_mean_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_mean_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_mean_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_mean_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_mean_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_mean_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_mean_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp85_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_pr_mean_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_mean_projections_JJA, newdata = . ))) 

baseline <- rcp85_pr_mean_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_mean_projections_full_delta_JJA <- rcp85_pr_mean_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_pr_mean_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_mean_projections_DJF, newdata = . ))) 

baseline <- rcp85_pr_mean_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_mean_projections_full_delta_DJF <- rcp85_pr_mean_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_pr_mean_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_mean_projections_SON, newdata = . ))) 

baseline <- rcp85_pr_mean_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_mean_projections_full_delta_SON <- rcp85_pr_mean_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp85_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_pr_mean_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_mean_projections_MAM, newdata = . ))) 

baseline <- rcp85_pr_mean_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_mean_projections_full_delta_MAM <- rcp85_pr_mean_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp85_pr_mean_projections_full_delta <- purrr::reduce(.x = list(rcp85_pr_mean_projections_full_delta_JJA, 
                                                            rcp85_pr_mean_projections_full_delta_DJF, 
                                                            rcp85_pr_mean_projections_full_delta_MAM, 
                                                            rcp85_pr_mean_projections_full_delta_SON), 
                                                  .f = rbind)
rcp85_pr_mean_projections_full_delta$lwr <- apply(X = rcp85_pr_mean_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp85_pr_mean_projections_full_delta$mle <- apply(X = rcp85_pr_mean_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp85_pr_mean_projections_full_delta$upr <- apply(X = rcp85_pr_mean_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_mean_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_mean_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_mean_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_mean_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_mean_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_mean_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_mean_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_mean_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp85_pr_mean_summary <- rcp85_pr_mean_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_pr_mean_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. pr_mean Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate") +
  ggplot2::ylim(c(-30,30))





#### REPEAT ALL THAT FOR cdd -------------

t1 <- Sys.time()
mod_rcp85_cdd_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cdd_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cdd_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cdd_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cdd_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cdd_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cdd_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cdd_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp85_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_cdd_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cdd_projections_JJA, newdata = . ))) 

baseline <- rcp85_cdd_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cdd_projections_full_delta_JJA <- rcp85_cdd_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_cdd_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cdd_projections_DJF, newdata = . ))) 

baseline <- rcp85_cdd_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cdd_projections_full_delta_DJF <- rcp85_cdd_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_cdd_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cdd_projections_SON, newdata = . ))) 

baseline <- rcp85_cdd_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cdd_projections_full_delta_SON <- rcp85_cdd_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp85_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_cdd_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cdd_projections_MAM, newdata = . ))) 

baseline <- rcp85_cdd_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cdd_projections_full_delta_MAM <- rcp85_cdd_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp85_cdd_projections_full_delta <- purrr::reduce(.x = list(rcp85_cdd_projections_full_delta_JJA, 
                                                            rcp85_cdd_projections_full_delta_DJF, 
                                                            rcp85_cdd_projections_full_delta_MAM, 
                                                            rcp85_cdd_projections_full_delta_SON), 
                                                  .f = rbind)
rcp85_cdd_projections_full_delta$lwr <- apply(X = rcp85_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp85_cdd_projections_full_delta$mle <- apply(X = rcp85_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp85_cdd_projections_full_delta$upr <- apply(X = rcp85_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'pyrome_rcp85_cdd_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cdd_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'pyrome_rcp85_cdd_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cdd_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'pyrome_rcp85_cdd_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cdd_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'hu4_graphs_rcp85_tas_mean_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p hu4_rcp85_tas_mean_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp85_cdd_summary <- rcp85_cdd_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_cdd_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. cdd Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")







#### REPEAT ALL THAT FOR cwd -------------

t1 <- Sys.time()
mod_rcp85_cwd_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cwd_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cwd_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cwd_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cwd_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cwd_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_cwd_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_cwd_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp85_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_cwd_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cwd_projections_JJA, newdata = . ))) 

baseline <- rcp85_cwd_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cwd_projections_full_delta_JJA <- rcp85_cwd_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_cwd_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cwd_projections_DJF, newdata = . ))) 

baseline <- rcp85_cwd_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cwd_projections_full_delta_DJF <- rcp85_cwd_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp85_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_cwd_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cwd_projections_SON, newdata = . ))) 

baseline <- rcp85_cwd_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cwd_projections_full_delta_SON <- rcp85_cwd_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp85_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_cwd_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_cwd_projections_MAM, newdata = . ))) 

baseline <- rcp85_cwd_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_cwd_projections_full_delta_MAM <- rcp85_cwd_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp85_cwd_projections_full_delta <- purrr::reduce(.x = list(rcp85_cwd_projections_full_delta_JJA, 
                                                            rcp85_cwd_projections_full_delta_DJF, 
                                                            rcp85_cwd_projections_full_delta_MAM, 
                                                            rcp85_cwd_projections_full_delta_SON), 
                                                  .f = rbind)
rcp85_cwd_projections_full_delta$lwr <- apply(X = rcp85_cwd_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = min, MARGIN = 1) 

rcp85_cwd_projections_full_delta$mle <- apply(X = rcp85_cwd_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = median, MARGIN = 1) 

rcp85_cwd_projections_full_delta$upr <- apply(X = rcp85_cwd_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_cwd_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cwd_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_cwd_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cwd_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_cwd_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cwd_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_cwd_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_cwd_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp85_cwd_summary <- rcp85_cwd_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_cwd_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. cwd Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")






#### REPEAT ALL THAT FOR wet_days_percent -------------
t1 <- Sys.time()
mod_rcp85_wet_days_percent_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_wet_days_percent_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_wet_days_percent_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_wet_days_percent_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_wet_days_percent_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_wet_days_percent_projections %>% filter(Season == "SON" )  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_wet_days_percent_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_wet_days_percent_projections %>% filter(Season == "MAM")  )
Sys.time() - t1


d <- distinct(rcp85_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_wet_days_percent_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_wet_days_percent_projections_JJA, newdata = . ))) 

baseline <- rcp85_wet_days_percent_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp85_wet_days_percent_projections_full_delta_JJA <- rcp85_wet_days_percent_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




d <- distinct(rcp85_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_wet_days_percent_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_wet_days_percent_projections_DJF, newdata = . ))) 

baseline <- rcp85_wet_days_percent_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp85_wet_days_percent_projections_full_delta_DJF <- rcp85_wet_days_percent_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()



d <- distinct(rcp85_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_wet_days_percent_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_wet_days_percent_projections_SON, newdata = . ))) 

baseline <- rcp85_wet_days_percent_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp85_wet_days_percent_projections_full_delta_SON <- rcp85_wet_days_percent_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




d <- distinct(rcp85_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_wet_days_percent_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_wet_days_percent_projections_MAM, newdata = . ))) 

baseline <- rcp85_wet_days_percent_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp85_wet_days_percent_projections_full_delta_MAM <- rcp85_wet_days_percent_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()


rcp85_wet_days_percent_projections_full_delta <- purrr::reduce(.x = list(rcp85_wet_days_percent_projections_full_delta_JJA, 
                                                            rcp85_wet_days_percent_projections_full_delta_DJF, 
                                                            rcp85_wet_days_percent_projections_full_delta_MAM, 
                                                            rcp85_wet_days_percent_projections_full_delta_SON), 
                                                  .f = rbind) 

rcp85_wet_days_percent_projections_full_delta$lwr <- apply(X = rcp85_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = min, MARGIN = 1) 

rcp85_wet_days_percent_projections_full_delta$mle <- apply(X = rcp85_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = median, MARGIN = 1) 

rcp85_wet_days_percent_projections_full_delta$upr <- apply(X = rcp85_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_wet_days_percent_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_wet_days_percent_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_wet_days_percent_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_wet_days_percent_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_wet_days_percent_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_wet_days_percent_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_wet_days_percent_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_wet_days_percent_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp85_wet_days_percent_summary <- rcp85_wet_days_percent_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_wet_days_percent_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. wet_days_percent Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")










# video code --------------------------------------------------------------
# 

rng_abs <- max(abs(range(rcp85_tas_mean_projections_full_delta %>%
        filter(lat < 50) %>%
        .$mle)))

rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "Spectral",
                    domain = rng,
                    reverse = TRUE)
# 
for(yr in 1998:2088){
  print(yr)

  # JJA
  x <- rcp85_tas_mean_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_tas_mean_projections_full_delta$lon),
            lat = median(rcp85_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA tas_mean change", yr))

  mapshot(x, file = paste0("pyrome_graphs_rcp85_tas_mean_JJA_", yr, ".png"), remove_url = TRUE)



  # DJF
  x <- rcp85_tas_mean_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_tas_mean_projections_full_delta$lon),
            lat = median(rcp85_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF tas_mean change", yr))

  mapshot(x, file = paste0("pyrome_graphs_rcp85_tas_mean_DJF_", yr, ".png"), remove_url = TRUE)


  # SON
  x <- rcp85_tas_mean_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_tas_mean_projections_full_delta$lon),
            lat = median(rcp85_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON tas_mean change", yr))

  mapshot(x, file = paste0("pyrome_graphs_rcp85_tas_mean_SON_", yr, ".png"), remove_url = TRUE)

  # MAM
  x <- rcp85_tas_mean_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_tas_mean_projections_full_delta$lon),
            lat = median(rcp85_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM tas_mean change", yr))

  mapshot(x, file = paste0("pyrome_graphs_rcp85_tas_mean_MAM_", yr, ".png"), remove_url = TRUE)
}


rng_abs <- max(abs(range(rcp85_pr_mean_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp85_pr_mean_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_mean_projections_full_delta$lon),
            lat = median(rcp85_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_pr_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp85_pr_mean_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_mean_projections_full_delta$lon),
            lat = median(rcp85_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_pr_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp85_pr_mean_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_mean_projections_full_delta$lon),
            lat = median(rcp85_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_pr_mean_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp85_pr_mean_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_mean_projections_full_delta$lon),
            lat = median(rcp85_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_pr_mean_MAM_", yr, ".png"), remove_url = TRUE)
}










rng_abs <- max(abs(range(rcp85_cdd_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = TRUE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp85_cdd_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cdd_projections_full_delta$lon),
            lat = median(rcp85_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cdd_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp85_cdd_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cdd_projections_full_delta$lon),
            lat = median(rcp85_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cdd_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp85_cdd_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cdd_projections_full_delta$lon),
            lat = median(rcp85_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cdd_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp85_cdd_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cdd_projections_full_delta$lon),
            lat = median(rcp85_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cdd_MAM_", yr, ".png"), remove_url = TRUE)
}








rng_abs <- max(abs(range(rcp85_cwd_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp85_cwd_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cwd_projections_full_delta$lon),
            lat = median(rcp85_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cwd_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp85_cwd_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cwd_projections_full_delta$lon),
            lat = median(rcp85_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cwd_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp85_cwd_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cwd_projections_full_delta$lon),
            lat = median(rcp85_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cwd_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp85_cwd_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_cwd_projections_full_delta$lon),
            lat = median(rcp85_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_cwd_MAM_", yr, ".png"), remove_url = TRUE)
}








rng_abs <- max(abs(range(rcp85_wet_days_percent_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp85_wet_days_percent_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp85_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_wet_days_percent_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp85_wet_days_percent_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp85_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_wet_days_percent_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp85_wet_days_percent_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp85_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_wet_days_percent_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp85_wet_days_percent_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp85_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp85_wet_days_percent_MAM_", yr, ".png"), remove_url = TRUE)
}













###########################################################################

# SMOOTH PREDICTIONS OVER SPACE JUST A BIT -------------------------------------
# experiment

## ^ isolates the spatial signal. then need to go back and apply to year?

t1 <- Sys.time()
mod_rcp45_tas_mean_projections_JJA <- mgcv::gam(list(
  # hatQ_0.025 ~ s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100),  
  # hatQ_0.5 ~  s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100),  
  # hatQ_0.975 ~ s(Year, bs = "tp", k=5) + s(lat, lon, Year, bs = "gp", k=100)
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_tas_mean_projections %>% filter(Season == "JJA") )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_tas_mean_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_tas_mean_projections %>% filter(Season == "DJF" ))  
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_tas_mean_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE, 
control = gam.control(trace = TRUE), 
data = rcp45_tas_mean_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_tas_mean_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_tas_mean_projections %>% filter(Season == "MAM" )  )
Sys.time() - t1






#### tas_mean rcp45 compile smoothed projections ------------

d <- distinct(rcp45_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_tas_mean_projections_full_JJA <- do.call("rbind", 
                                               replicate(n = length(1998:2088),
                                                         d , 
                                                         simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_tas_mean_projections_JJA, newdata = . ))) 

baseline <- rcp45_tas_mean_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_tas_mean_projections_full_delta_JJA <- rcp45_tas_mean_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_tas_mean_projections_full_DJF <- do.call("rbind", 
                                               replicate(n = length(1998:2088),
                                                         d , 
                                                         simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_tas_mean_projections_DJF, newdata = . ))) 

baseline <- rcp45_tas_mean_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_tas_mean_projections_full_delta_DJF <- rcp45_tas_mean_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_tas_mean_projections_full_SON <- do.call("rbind", 
                                               replicate(n = length(1998:2088),
                                                         d , 
                                                         simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_tas_mean_projections_SON, newdata = . ))) 

baseline <- rcp45_tas_mean_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_tas_mean_projections_full_delta_SON <- rcp45_tas_mean_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp45_tas_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_tas_mean_projections_full_MAM <- do.call("rbind", 
                                               replicate(n = length(1998:2088),
                                                         d , 
                                                         simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_tas_mean_projections_MAM, newdata = . ))) 

baseline <- rcp45_tas_mean_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_tas_mean_projections_full_delta_MAM <- rcp45_tas_mean_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025, 
         hatQ_0.5 = hatQ_0.5,
         hatQ_0.975 = hatQ_0.975, 
         current = current, 
         current_lwr = current_lwr, 
         current_upr = current_upr) %>%
  mutate(hatQ_0.025_delta = (hatQ_0.025 - current_lwr),
         hatQ_0.5_delta = (hatQ_0.5 - current), 
         hatQ_0.975_delta = (hatQ_0.975 - current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp45_tas_mean_projections_full_delta <- purrr::reduce(.x = list(rcp45_tas_mean_projections_full_delta_JJA, 
                                                                 rcp45_tas_mean_projections_full_delta_DJF, 
                                                                 rcp45_tas_mean_projections_full_delta_MAM, 
                                                                 rcp45_tas_mean_projections_full_delta_SON), 
                                                       .f = rbind)
rcp45_tas_mean_projections_full_delta$lwr <- apply(X = rcp45_tas_mean_projections_full_delta %>%
                                                     as.data.frame() %>%
                                                     select(ends_with("delta")) , 
                                                   FUN = min, MARGIN = 1) 

rcp45_tas_mean_projections_full_delta$mle <- apply(X = rcp45_tas_mean_projections_full_delta %>%
                                                     as.data.frame() %>%
                                                     select(ends_with("delta")) , 
                                                   FUN = median, MARGIN = 1) 

rcp45_tas_mean_projections_full_delta$upr <- apply(X = rcp45_tas_mean_projections_full_delta %>%
                                                     as.data.frame() %>%
                                                     select(ends_with("delta")) , 
                                                   FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_tas_mean_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_tas_mean_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_tas_mean_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_tas_mean_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_tas_mean_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_tas_mean_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_tas_mean_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_tas_mean_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp45_tas_mean_summary <- rcp45_tas_mean_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_tas_mean_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. tas_mean Change from 2019") +
  ggplot2::ylab("Deg C Change from 2019 Climate") 




#### REPEAT ALL THAT FOR PR_MEAN -------------

t1 <- Sys.time()
mod_rcp45_pr_mean_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_mean_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_mean_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_mean_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_mean_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_mean_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_mean_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_mean_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp45_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_pr_mean_projections_full_JJA <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_mean_projections_JJA, newdata = . ))) 

baseline <- rcp45_pr_mean_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_mean_projections_full_delta_JJA <- rcp45_pr_mean_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_pr_mean_projections_full_DJF <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_mean_projections_DJF, newdata = . ))) 

baseline <- rcp45_pr_mean_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_mean_projections_full_delta_DJF <- rcp45_pr_mean_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_pr_mean_projections_full_SON <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_mean_projections_SON, newdata = . ))) 

baseline <- rcp45_pr_mean_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_mean_projections_full_delta_SON <- rcp45_pr_mean_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp45_pr_mean_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_pr_mean_projections_full_MAM <- do.call("rbind", 
                                              replicate(n = length(1998:2088),
                                                        d , 
                                                        simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_mean_projections_MAM, newdata = . ))) 

baseline <- rcp45_pr_mean_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_mean_projections_full_delta_MAM <- rcp45_pr_mean_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp45_pr_mean_projections_full_delta <- purrr::reduce(.x = list(rcp45_pr_mean_projections_full_delta_JJA, 
                                                                rcp45_pr_mean_projections_full_delta_DJF, 
                                                                rcp45_pr_mean_projections_full_delta_MAM, 
                                                                rcp45_pr_mean_projections_full_delta_SON), 
                                                      .f = rbind)
rcp45_pr_mean_projections_full_delta$lwr <- apply(X = rcp45_pr_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = min, MARGIN = 1) 

rcp45_pr_mean_projections_full_delta$mle <- apply(X = rcp45_pr_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = median, MARGIN = 1) 

rcp45_pr_mean_projections_full_delta$upr <- apply(X = rcp45_pr_mean_projections_full_delta %>%
                                                    as.data.frame() %>%
                                                    select(ends_with("delta")) , 
                                                  FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_mean_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_mean_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_mean_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_mean_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_mean_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_mean_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_mean_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_mean_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp45_pr_mean_summary <- rcp45_pr_mean_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_pr_mean_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. pr_mean Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate") +
  ggplot2::ylim(c(-30,30))





#### REPEAT ALL THAT FOR cdd -------------

t1 <- Sys.time()
mod_rcp45_cdd_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cdd_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cdd_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cdd_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cdd_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cdd_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cdd_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cdd_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp45_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_cdd_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cdd_projections_JJA, newdata = . ))) 

baseline <- rcp45_cdd_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cdd_projections_full_delta_JJA <- rcp45_cdd_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_cdd_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cdd_projections_DJF, newdata = . ))) 

baseline <- rcp45_cdd_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cdd_projections_full_delta_DJF <- rcp45_cdd_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_cdd_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cdd_projections_SON, newdata = . ))) 

baseline <- rcp45_cdd_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cdd_projections_full_delta_SON <- rcp45_cdd_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp45_cdd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_cdd_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cdd_projections_MAM, newdata = . ))) 

baseline <- rcp45_cdd_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cdd_projections_full_delta_MAM <- rcp45_cdd_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp45_cdd_projections_full_delta <- purrr::reduce(.x = list(rcp45_cdd_projections_full_delta_JJA, 
                                                            rcp45_cdd_projections_full_delta_DJF, 
                                                            rcp45_cdd_projections_full_delta_MAM, 
                                                            rcp45_cdd_projections_full_delta_SON), 
                                                  .f = rbind)
rcp45_cdd_projections_full_delta$lwr <- apply(X = rcp45_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp45_cdd_projections_full_delta$mle <- apply(X = rcp45_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp45_cdd_projections_full_delta$upr <- apply(X = rcp45_cdd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cdd_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cdd_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cdd_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cdd_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cdd_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cdd_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cdd_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cdd_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp45_cdd_summary <- rcp45_cdd_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_cdd_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. cdd Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")







#### REPEAT ALL THAT FOR cwd -------------

t1 <- Sys.time()
mod_rcp45_cwd_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cwd_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cwd_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cwd_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cwd_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cwd_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_cwd_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_cwd_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp45_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_cwd_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cwd_projections_JJA, newdata = . ))) 

baseline <- rcp45_cwd_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cwd_projections_full_delta_JJA <- rcp45_cwd_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_cwd_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cwd_projections_DJF, newdata = . ))) 

baseline <- rcp45_cwd_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cwd_projections_full_delta_DJF <- rcp45_cwd_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()






d <- distinct(rcp45_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_cwd_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cwd_projections_SON, newdata = . ))) 

baseline <- rcp45_cwd_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cwd_projections_full_delta_SON <- rcp45_cwd_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()





d <- distinct(rcp45_cwd_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_cwd_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_cwd_projections_MAM, newdata = . ))) 

baseline <- rcp45_cwd_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_cwd_projections_full_delta_MAM <- rcp45_cwd_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




rcp45_cwd_projections_full_delta <- purrr::reduce(.x = list(rcp45_cwd_projections_full_delta_JJA, 
                                                            rcp45_cwd_projections_full_delta_DJF, 
                                                            rcp45_cwd_projections_full_delta_MAM, 
                                                            rcp45_cwd_projections_full_delta_SON), 
                                                  .f = rbind)
rcp45_cwd_projections_full_delta$lwr <- apply(X = rcp45_cwd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp45_cwd_projections_full_delta$mle <- apply(X = rcp45_cwd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp45_cwd_projections_full_delta$upr <- apply(X = rcp45_cwd_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cwd_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cwd_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cwd_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cwd_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cwd_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cwd_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_cwd_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_cwd_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp45_cwd_summary <- rcp45_cwd_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_cwd_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. cwd Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")






#### REPEAT ALL THAT FOR wet_days_percent -------------
t1 <- Sys.time()
mod_rcp45_wet_days_percent_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_wet_days_percent_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_wet_days_percent_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_wet_days_percent_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_wet_days_percent_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_wet_days_percent_projections %>% filter(Season == "SON" )  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_wet_days_percent_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_wet_days_percent_projections %>% filter(Season == "MAM")  )
Sys.time() - t1


d <- distinct(rcp45_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_wet_days_percent_projections_full_JJA <- do.call("rbind", 
                                                       replicate(n = length(1998:2088),
                                                                 d , 
                                                                 simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_wet_days_percent_projections_JJA, newdata = . ))) 

baseline <- rcp45_wet_days_percent_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp45_wet_days_percent_projections_full_delta_JJA <- rcp45_wet_days_percent_projections_full_JJA %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




d <- distinct(rcp45_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_wet_days_percent_projections_full_DJF <- do.call("rbind", 
                                                       replicate(n = length(1998:2088),
                                                                 d , 
                                                                 simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_wet_days_percent_projections_DJF, newdata = . ))) 

baseline <- rcp45_wet_days_percent_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp45_wet_days_percent_projections_full_delta_DJF <- rcp45_wet_days_percent_projections_full_DJF %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()



d <- distinct(rcp45_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_wet_days_percent_projections_full_SON <- do.call("rbind", 
                                                       replicate(n = length(1998:2088),
                                                                 d , 
                                                                 simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_wet_days_percent_projections_SON, newdata = . ))) 

baseline <- rcp45_wet_days_percent_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp45_wet_days_percent_projections_full_delta_SON <- rcp45_wet_days_percent_projections_full_SON %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()




d <- distinct(rcp45_wet_days_percent_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_wet_days_percent_projections_full_MAM <- do.call("rbind", 
                                                       replicate(n = length(1998:2088),
                                                                 d , 
                                                                 simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_wet_days_percent_projections_MAM, newdata = . ))) 

baseline <- rcp45_wet_days_percent_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_lwr = V1, current_upr = V3)

rcp45_wet_days_percent_projections_full_delta_MAM <- rcp45_wet_days_percent_projections_full_MAM %>% 
  inner_join(baseline) %>%
  rename( hatQ_0.025 = V1, 
          hatQ_0.5 = V2,
          hatQ_0.975 = V3)  %>%
  mutate(hatQ_0.025 = hatQ_0.025^2, 
         hatQ_0.5 = hatQ_0.5^2,
         hatQ_0.975 = hatQ_0.975^2, 
         current = current^2, 
         current_lwr = current_lwr^2, 
         current_upr = current_upr^2) %>%
  mutate(hatQ_0.025_delta = 100*((hatQ_0.025 - current_lwr)/current_lwr),
         hatQ_0.5_delta = 100*((hatQ_0.5 - current)/current), 
         hatQ_0.975_delta = 100*((hatQ_0.975 - current_upr)/current_upr)) %>% 
  inner_join(pyromes %>% select(RegionName = FPU_CODE, geometry)) %>% 
  st_as_sf()


rcp45_wet_days_percent_projections_full_delta <- purrr::reduce(.x = list(rcp45_wet_days_percent_projections_full_delta_JJA, 
                                                                         rcp45_wet_days_percent_projections_full_delta_DJF, 
                                                                         rcp45_wet_days_percent_projections_full_delta_MAM, 
                                                                         rcp45_wet_days_percent_projections_full_delta_SON), 
                                                               .f = rbind) 

rcp45_wet_days_percent_projections_full_delta$lwr <- apply(X = rcp45_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = min, MARGIN = 1) 

rcp45_wet_days_percent_projections_full_delta$mle <- apply(X = rcp45_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = median, MARGIN = 1) 

rcp45_wet_days_percent_projections_full_delta$upr <- apply(X = rcp45_wet_days_percent_projections_full_delta %>%
                                                             as.data.frame() %>%
                                                             select(ends_with("delta")) , 
                                                           FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_wet_days_percent_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_wet_days_percent_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_wet_days_percent_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_wet_days_percent_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_wet_days_percent_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_wet_days_percent_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_wet_days_percent_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_wet_days_percent_JJA.mp4

areas <- tibble(RegionName = pyromes$FPU_CODE, RelativeSize = as.numeric((pyromes %>% st_area())))

rcp45_wet_days_percent_summary <- rcp45_wet_days_percent_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_wet_days_percent_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. wet_days_percent Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate")










# video code --------------------------------------------------------------
# 

rng_abs <- max(abs(range(rcp45_tas_mean_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "Spectral",
                    domain = rng,
                    reverse = TRUE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp45_tas_mean_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_tas_mean_projections_full_delta$lon),
            lat = median(rcp45_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA tas_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_tas_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_tas_mean_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_tas_mean_projections_full_delta$lon),
            lat = median(rcp45_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF tas_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_tas_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_tas_mean_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_tas_mean_projections_full_delta$lon),
            lat = median(rcp45_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON tas_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_tas_mean_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_tas_mean_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_tas_mean_projections_full_delta$lon),
            lat = median(rcp45_tas_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM tas_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_tas_mean_MAM_", yr, ".png"), remove_url = TRUE)
}


rng_abs <- max(abs(range(rcp45_pr_mean_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp45_pr_mean_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_mean_projections_full_delta$lon),
            lat = median(rcp45_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_pr_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_pr_mean_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_mean_projections_full_delta$lon),
            lat = median(rcp45_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_pr_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_pr_mean_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_mean_projections_full_delta$lon),
            lat = median(rcp45_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_pr_mean_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_pr_mean_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_mean_projections_full_delta$lon),
            lat = median(rcp45_pr_mean_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM pr_mean change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_pr_mean_MAM_", yr, ".png"), remove_url = TRUE)
}










rng_abs <- max(abs(range(rcp45_cdd_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = TRUE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp45_cdd_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cdd_projections_full_delta$lon),
            lat = median(rcp45_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cdd_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_cdd_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cdd_projections_full_delta$lon),
            lat = median(rcp45_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cdd_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_cdd_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cdd_projections_full_delta$lon),
            lat = median(rcp45_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cdd_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_cdd_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cdd_projections_full_delta$lon),
            lat = median(rcp45_cdd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM cdd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cdd_MAM_", yr, ".png"), remove_url = TRUE)
}








rng_abs <- max(abs(range(rcp45_cwd_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp45_cwd_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cwd_projections_full_delta$lon),
            lat = median(rcp45_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cwd_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_cwd_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cwd_projections_full_delta$lon),
            lat = median(rcp45_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cwd_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_cwd_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cwd_projections_full_delta$lon),
            lat = median(rcp45_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cwd_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_cwd_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_cwd_projections_full_delta$lon),
            lat = median(rcp45_cwd_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM cwd change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_cwd_MAM_", yr, ".png"), remove_url = TRUE)
}








rng_abs <- max(abs(range(rcp45_wet_days_percent_projections_full_delta %>%
                           filter(lat < 50) %>%
                           .$mle)))

#rng_abs <- c(-20, 20)
rng <- c(-1*rng_abs, rng_abs)

pal <- colorNumeric(palette = "BrBG",
                    domain = rng,
                    reverse = FALSE)
# 
for(yr in 1998:2088){
  print(yr)
  
  # JJA
  x <- rcp45_wet_days_percent_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp45_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_wet_days_percent_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_wet_days_percent_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp45_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_wet_days_percent_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_wet_days_percent_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp45_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_wet_days_percent_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_wet_days_percent_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_wet_days_percent_projections_full_delta$lon),
            lat = median(rcp45_wet_days_percent_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM wet_days_percent change", yr))
  
  mapshot(x, file = paste0("pyrome_graphs_rcp45_wet_days_percent_MAM_", yr, ".png"), remove_url = TRUE)
}



# WRITE OUT EVERYTHING ----------------------------------------------------

wildfire_climate_conditioning_data_product_v1 <- list(
  rcp45_tas_mean_projections_full_delta = rcp45_tas_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  rcp85_tas_mean_projections_full_delta = rcp85_tas_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  
  rcp45_pr_mean_projections_full_delta = rcp45_pr_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  rcp85_pr_mean_projections_full_delta = rcp85_pr_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  
  rcp45_cdd_projections_full_delta = rcp45_cdd_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  rcp85_cdd_projections_full_delta = rcp85_cdd_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  
  rcp45_cwd_projections_full_delta = rcp45_cwd_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  rcp85_cwd_projections_full_delta = rcp85_cwd_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  
  rcp45_wet_days_percent_projections_full_delta = rcp45_wet_days_percent_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry), 
  rcp85_wet_days_percent_projections_full_delta = rcp85_wet_days_percent_projections_full_delta %>% 
    as.data.frame() %>% select(-geometry) 
)


saveRDS(wildfire_climate_conditioning_data_product_v1, 
        "wildfire_climate_conditioning_data_product_v1.rds")



