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

narr_statistics <- readRDS("narr_statistics.rds")
cmip5_pr_historical <- readRDS("cmip5_daily_pr_statistics_historical.rds")
cmip5_tas_historical <- readRDS("cmip5_daily_tas_statistics_historical.rds")

cmip5_pr_rcp85 <- readRDS("cmip5_daily_pr_statistics_rcp85.rds")
cmip5_tas_rcp85 <- readRDS("cmip5_daily_tas_statistics_rcp85.rds")

cmip5_pr_rcp45 <- readRDS("cmip5_daily_pr_statistics_rcp45.rds")
cmip5_tas_rcp45 <- readRDS("cmip5_daily_tas_statistics_rcp45.rds")


hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/") %>% 
  rmapshaper::ms_simplify()

hu4_centroids <- sf::st_centroid(hu4) %>%
  select(NAME, Shape=Shape) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_set_Shape(NULL)



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
            pr_max = max(sqrt(pr_max)), 
            pr_max = sqrt(mean(replace_inf(pr_max))), 
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
            pr_max = max(sqrt(pr_max)), 
            pr_max = sqrt(mean(replace_inf(pr_max))), 
            cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
mod_jba1_obj <- readRDS("mod_jba1_obj.rds")
shared_meta <- mod_jba1_obj$shared_meta
orig_train <- mod_jba1_obj$final_train
mod <- mod_jba1_obj$mod

multiClimateSimulator <- function(start_year = year_starts[1], 
                                end_year = year_ends[1], 
                                df_tas = features_tas_rcp45,
                                df_pr = features_pr_rcp45,
                                original_training_df = orig_train, 
                                bias_model = mod, 
                                varnames = c("tas_mean", "pr_mean", "pr_max")){
  
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
              pr_max = max(sqrt(pr_max)), 
              pr_max = sqrt(mean(replace_inf(pr_max))), 
              cwd = sqrt(mean(replace_inf(cwd)))) %>% 
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
              .f= ~ climate_simulator(model = mod, 
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

rcp45_projections <- purrr::map2(.x = year_starts, .y = year_ends, 
            .f = ~multiClimateSimulator(start_year = .x, 
                                      end_year = .y, 
                                      df_tas = features_tas_rcp45, 
                                      df_pr = features_pr_rcp45, 
                                      original_training_df = orig_train, 
                                      bias_model = mod, 
                                      varnames = c("tas_mean", "pr_mean", "pr_max")
                                      ))

names(rcp45_projections) <- paste("rcp45_climate", climatology_labels, sep='_')


rcp85_projections <- purrr::map2(.x = year_starts, .y = year_ends, 
                                 .f = ~multiClimateSimulator(start_year = .x, 
                                                             end_year = .y, 
                                                             df_tas = features_tas_rcp85, 
                                                             df_pr = features_pr_rcp85, 
                                                             original_training_df = orig_train, 
                                                             bias_model = mod, 
                                                             varnames = c("tas_mean", "pr_mean", "pr_max")
                                 ))

names(rcp85_projections) <- paste("rcp85_climate_", climatology_labels, sep='_')




# NEXT STEP IS INTERPOLATING EACH YEAR AND CLIMATE % CHANGE FACTOR! -------
# BASICALLY A SQL DF LIKE LOOKUP TO USE FOR JBA FILE MODIFICATION (% change factors)

rcp85_tas_mean_projections <- purrr::map2(.x = rcp85_projections, .y = climatology_labels, 
            .f = ~.x$tas_mean %>%
              dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))


rcp85_pr_mean_projections <- purrr::map2(.x = rcp85_projections, .y = climatology_labels, 
                                          .f = ~.x$pr_mean %>%
                                            dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))

rcp85_pr_max_projections <- purrr::map2(.x = rcp85_projections, .y = climatology_labels, 
                                         .f = ~.x$pr_max %>%
                                           dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 





rcp45_tas_mean_projections <- purrr::map2(.x = rcp45_projections, .y = climatology_labels, 
                                          .f = ~.x$tas_mean %>%
                                            dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 


rcp45_pr_mean_projections <- purrr::map2(.x = rcp45_projections, .y = climatology_labels, 
                                         .f = ~.x$pr_mean %>%
                                           dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season))

rcp45_pr_max_projections <- purrr::map2(.x = rcp45_projections, .y = climatology_labels, 
                                        .f = ~.x$pr_max %>%
                                          dplyr::mutate(Year = .y)) %>% 
  purrr::reduce(.f = rbind) %>% 
  # mutate(Group = as.factor(paste(RegionName, Season, sep = "_"))) %>% 
  inner_join(hu4_centroids, by =c("RegionName" = "NAME")) %>% 
  select(RegionName, Season, hatQ_0.025, hatQ_0.5, hatQ_0.975, lon, lat, Year) %>%
  mutate(Season = as.factor(Season)) 





# # SMOOTH ----------------------------------------------------------------

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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

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





#### REPEAT ALL THAT FOR pr_max -------------

t1 <- Sys.time()
mod_rcp85_pr_max_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_max_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_max_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_max_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_max_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_max_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp85_pr_max_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp85_pr_max_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp85_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp85_pr_max_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_max_projections_JJA, newdata = . ))) 

baseline <- rcp85_pr_max_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_max_projections_full_delta_JJA <- rcp85_pr_max_projections_full_JJA %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()






d <- distinct(rcp85_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp85_pr_max_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_max_projections_DJF, newdata = . ))) 

baseline <- rcp85_pr_max_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_max_projections_full_delta_DJF <- rcp85_pr_max_projections_full_DJF %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()






d <- distinct(rcp85_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp85_pr_max_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_max_projections_SON, newdata = . ))) 

baseline <- rcp85_pr_max_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_max_projections_full_delta_SON <- rcp85_pr_max_projections_full_SON %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()





d <- distinct(rcp85_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp85_pr_max_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp85_pr_max_projections_MAM, newdata = . ))) 

baseline <- rcp85_pr_max_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp85_pr_max_projections_full_delta_MAM <- rcp85_pr_max_projections_full_MAM %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()




rcp85_pr_max_projections_full_delta <- purrr::reduce(.x = list(rcp85_pr_max_projections_full_delta_JJA, 
                                                            rcp85_pr_max_projections_full_delta_DJF, 
                                                            rcp85_pr_max_projections_full_delta_MAM, 
                                                            rcp85_pr_max_projections_full_delta_SON), 
                                                  .f = rbind)
rcp85_pr_max_projections_full_delta$lwr <- apply(X = rcp85_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp85_pr_max_projections_full_delta$mle <- apply(X = rcp85_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp85_pr_max_projections_full_delta$upr <- apply(X = rcp85_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_max_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_max_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_max_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_max_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_max_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_max_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp85_pr_max_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp85_pr_max_JJA.mp4

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

rcp85_pr_max_summary <- rcp85_pr_max_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp85_pr_max_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("RCP85: Contiguous U.S. pr_max Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate") +
  ggplot2::ylim(c(-60, 60))

















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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
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

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

rcp45_pr_mean_summary <- rcp45_pr_mean_projections_full_delta %>%
  filter(lat < 50) %>%
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





#### REPEAT ALL THAT FOR pr_max -------------

t1 <- Sys.time()
mod_rcp45_pr_max_projections_JJA <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_max_projections %>% filter(Season == "JJA")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_max_projections_DJF <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_max_projections %>% filter(Season == "DJF")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_max_projections_SON <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_max_projections %>% filter(Season == "SON")  )
Sys.time() - t1

t1 <- Sys.time()
mod_rcp45_pr_max_projections_MAM <- mgcv::gam(list(
  hatQ_0.025 ~ s(lat, lon, Year,  bs = "gp"),  
  hatQ_0.5 ~ s(lat, lon, Year, bs = "gp"),  
  hatQ_0.975 ~ s(lat, lon, Year, bs = "gp")
), 
family=mvn(d=3), discrete = TRUE,
control = gam.control(trace = TRUE),
data = rcp45_pr_max_projections %>% filter(Season == "MAM")  )
Sys.time() - t1

d <- distinct(rcp45_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "JJA"))

rcp45_pr_max_projections_full_JJA <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_max_projections_JJA, newdata = . ))) 

baseline <- rcp45_pr_max_projections_full_JJA %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_max_projections_full_delta_JJA <- rcp45_pr_max_projections_full_JJA %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()






d <- distinct(rcp45_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "DJF"))

rcp45_pr_max_projections_full_DJF <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_max_projections_DJF, newdata = . ))) 

baseline <- rcp45_pr_max_projections_full_DJF %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_max_projections_full_delta_DJF <- rcp45_pr_max_projections_full_DJF %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()






d <- distinct(rcp45_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "SON"))

rcp45_pr_max_projections_full_SON <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_max_projections_SON, newdata = . ))) 

baseline <- rcp45_pr_max_projections_full_SON %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_max_projections_full_delta_SON <- rcp45_pr_max_projections_full_SON %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()





d <- distinct(rcp45_pr_max_projections %>% 
                select(RegionName, Season, lon, lat) %>% 
                filter(Season == "MAM"))

rcp45_pr_max_projections_full_MAM <- do.call("rbind", 
                                          replicate(n = length(1998:2088),
                                                    d , 
                                                    simplify = FALSE)) %>%
  mutate(Year = replicate(n = nrow(d), 1998:2088, simplify = FALSE) %>% 
           unlist() %>% 
           sort()) %>%
  bind_cols(as_tibble(predict(mod_rcp45_pr_max_projections_MAM, newdata = . ))) 

baseline <- rcp45_pr_max_projections_full_MAM %>% 
  filter(Year == 2019) %>%
  select(RegionName, Season, lon, lat, current = V2, current_upr = V3, current_lwr = V1)

rcp45_pr_max_projections_full_delta_MAM <- rcp45_pr_max_projections_full_MAM %>% 
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
  inner_join(hu4 %>% select(RegionName = NAME, Shape)) %>% 
  st_as_sf()




rcp45_pr_max_projections_full_delta <- purrr::reduce(.x = list(rcp45_pr_max_projections_full_delta_JJA, 
                                                            rcp45_pr_max_projections_full_delta_DJF, 
                                                            rcp45_pr_max_projections_full_delta_MAM, 
                                                            rcp45_pr_max_projections_full_delta_SON), 
                                                  .f = rbind)
rcp45_pr_max_projections_full_delta$lwr <- apply(X = rcp45_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = min, MARGIN = 1) 

rcp45_pr_max_projections_full_delta$mle <- apply(X = rcp45_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = median, MARGIN = 1) 

rcp45_pr_max_projections_full_delta$upr <- apply(X = rcp45_pr_max_projections_full_delta %>%
                                                as.data.frame() %>%
                                                select(ends_with("delta")) , 
                                              FUN = max, MARGIN = 1) 

## ffmpeg calls in terminal
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_max_MAM_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_max_MAM.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_max_SON_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_max_SON.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_max_DJF_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_max_DJF.mp4
# ffmpeg -framerate 5 -pattern_type glob -i 'rcp45_pr_max_JJA_*.png' -c:v libx264 -r 30 -pix_fmt yuv420p rcp45_pr_max_JJA.mp4

areas <- tibble(RegionName = hu4$NAME, RelativeSize = as.numeric((hu4 %>% st_area())))

rcp45_pr_max_summary <- rcp45_pr_max_projections_full_delta %>%
  as.data.frame() %>%
  inner_join(areas, by = "RegionName") %>%
  # filter(RegionName == "Upper Colorado") %>%
  group_by(Year, Season) %>%
  
  summarise(mle = mean(mle), upr = mean(upr), 
            lwr= mean(lwr)) %>% 
  ungroup()

rcp45_pr_max_summary %>% 
  #filter(Season=="DJF") %>%
  ggplot2::ggplot(aes(x = Year, y = mle, colour = Season)) + 
  ggplot2::geom_line(size=2) + 
  ggplot2::geom_line(aes(y = upr), linetype=2) + 
  ggplot2::geom_line(aes(y = lwr), linetype=2) + 
  ggplot2::geom_line(aes(y = 0, x=Year), linetype=1, colour = "black") +
  #ggplot2::geom_smooth(method='lm') +
  #ggplot2::geom_smooth(method='loess', span = 3) +
  #geom_point() + 
  ggplot2::ggtitle("rcp45: Contiguous U.S. pr_max Change from 2019") +
  ggplot2::ylab("% Change from 2019 Climate") %>%
  ggplot2::ylim(c(-50,50))









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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_tas_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_tas_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_tas_mean_SON_", yr, ".png"), remove_url = TRUE)
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_tas_mean_MAM_", yr, ".png"), remove_url = TRUE)
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_mean_SON_", yr, ".png"), remove_url = TRUE)
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_mean_MAM_", yr, ".png"), remove_url = TRUE)
}




rng_abs <- max(abs(range(rcp45_pr_max_projections_full_delta %>%
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
  x <- rcp45_pr_max_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_max_projections_full_delta$lon),
            lat = median(rcp45_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_max_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp45_pr_max_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_max_projections_full_delta$lon),
            lat = median(rcp45_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_max_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp45_pr_max_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_max_projections_full_delta$lon),
            lat = median(rcp45_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_max_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp45_pr_max_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp45_pr_max_projections_full_delta$lon),
            lat = median(rcp45_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp45_pr_max_MAM_", yr, ".png"), remove_url = TRUE)
}



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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_tas_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_tas_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_tas_mean_SON_", yr, ".png"), remove_url = TRUE)
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_tas_mean_MAM_", yr, ".png"), remove_url = TRUE)
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_mean_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_mean_DJF_", yr, ".png"), remove_url = TRUE)
  
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_mean_SON_", yr, ".png"), remove_url = TRUE)
  
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
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_mean_MAM_", yr, ".png"), remove_url = TRUE)
}










rng_abs <- max(abs(range(rcp85_pr_max_projections_full_delta %>%
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
  x <- rcp85_pr_max_projections_full_delta %>%
    filter(Season=="JJA" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_max_projections_full_delta$lon),
            lat = median(rcp85_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng ,
              title = paste("JJA pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_max_JJA_", yr, ".png"), remove_url = TRUE)
  
  
  
  # DJF
  x <- rcp85_pr_max_projections_full_delta %>%
    filter(Season=="DJF" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_max_projections_full_delta$lon),
            lat = median(rcp85_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("DJF pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_max_DJF_", yr, ".png"), remove_url = TRUE)
  
  
  # SON
  x <- rcp85_pr_max_projections_full_delta %>%
    filter(Season=="SON" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_max_projections_full_delta$lon),
            lat = median(rcp85_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("SON pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_max_SON_", yr, ".png"), remove_url = TRUE)
  
  # MAM
  x <- rcp85_pr_max_projections_full_delta %>%
    filter(Season=="MAM" & Year == yr & lat < 50) %>%
    leaflet() %>%
    setView(lng = median(rcp85_pr_max_projections_full_delta$lon),
            lat = median(rcp85_pr_max_projections_full_delta$lat), zoom = 4.2) %>%
    addTiles() %>%
    addPolygons(popup = ~as.character(round(mle, 2)),
                fillColor = ~pal(mle), weight = 0.5, color='black', fillOpacity = 0.8) %>%
    addLegend("bottomleft", pal = pal,
              values = ~ rng,
              title = paste("MAM pr_max change", yr))
  
  mapshot(x, file = paste0("hu4_graphs_rcp85_pr_max_MAM_", yr, ".png"), remove_url = TRUE)
}








# WRITE OUT EVERYTHING ----------------------------------------------------

flood_climate_conditioning_data_product_v1 <- list(
  rcp45_tas_mean_projections_full_delta = rcp45_tas_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape), 
  rcp85_tas_mean_projections_full_delta = rcp85_tas_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape), 
  
  rcp45_pr_mean_projections_full_delta = rcp45_pr_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape), 
  rcp85_pr_mean_projections_full_delta = rcp85_pr_mean_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape), 
  
  rcp45_pr_max_projections_full_delta = rcp45_pr_max_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape), 
  rcp85_pr_max_projections_full_delta = rcp85_pr_max_projections_full_delta %>% 
    as.data.frame() %>% select(-Shape)
)


saveRDS(flood_climate_conditioning_data_product_v1, 
        "flood_climate_conditioning_data_product_v1.rds")




