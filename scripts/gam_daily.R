library(dplyr)
library(sf)
library(tidyverse)
library(purrr)
library(mgcv)
library(rmapshaper)


# DAILY -------------------------------------------------------------------
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


features_tas_train <- cmip5_tas_historical %>% 
  dplyr::filter(Year >= 1976 & Year <= 2005) %>%
  dplyr::filter(gcm != "NorESM1-M")

features_tas_test <- cmip5_tas_rcp85 %>% 
  dplyr::filter(Year >= 2020 & Year <= 2049) %>%
  dplyr::filter(gcm != "NorESM1-M")


features_pr_train <- cmip5_pr_historical %>% 
  dplyr::filter(Year >= 1976 & Year <= 2005) %>%
  dplyr::filter(gcm != "NorESM1-M")

features_pr_test <- cmip5_pr_rcp85 %>% 
  dplyr::filter(Year >= 2020 & Year <= 2049) %>%
  dplyr::filter(gcm != "NorESM1-M")



truth_tas_train <- cmip5_tas_historical %>% 
  dplyr::filter(Year >= 1976 & Year <= 2005) %>%
  dplyr::filter(gcm == "NorESM1-M")

truth_tas_test <- cmip5_tas_rcp85 %>% 
  dplyr::filter(Year >= 2020 & Year <= 2049) %>%
  dplyr::filter(gcm == "NorESM1-M")


truth_pr_train <- cmip5_pr_historical %>% 
  dplyr::filter(Year >= 1976 & Year <= 2005) %>%
  dplyr::filter(gcm == "NorESM1-M")

truth_pr_test <- cmip5_pr_rcp85 %>% 
  dplyr::filter(Year >= 2020 & Year <= 2049) %>%
  dplyr::filter(gcm == "NorESM1-M")


features_train <- features_tas_train %>%
  inner_join(features_pr_train, 
             by = c("RegionName", "Year", "Month", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max))) %>% 
  ungroup() 

truth_train <- truth_tas_train %>%
  inner_join(truth_pr_train, 
             by = c("RegionName", "Year", "Month", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max))) %>% 
  ungroup() 

train <- features_train %>% 
  inner_join(truth_train %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max), 
             by = c("RegionName")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth) %>%
  arrange(RegionName, gcm_ic) %>%
  inner_join(hu4_centroids, by = c("RegionName"= "NAME")) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))




features_test <- features_tas_test %>%
  inner_join(features_pr_test, 
             by = c("RegionName", "Year", "Month", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName, gcm, ic, gcm_ic) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max))) %>% 
  ungroup() 

truth_test <- truth_tas_test %>%
  inner_join(truth_pr_test, 
             by = c("RegionName", "Year", "Month", "gcm", "ic", "gcm_ic")) %>%
  # aggregate for now
  group_by(RegionName) %>%
  summarise(tas_mean = mean(tas_mean), 
            heating_degree_days = sqrt(sum(heating_degree_days)/length(unique(Year))), # norm b/c 30 years
            cooling_degree_days = sqrt(sum(cooling_degree_days)/length(unique(Year))), 
            pr_mean = mean(sqrt(pr_mean)),
            pr_max = max(sqrt(pr_max))) %>% 
  ungroup() 

test <- features_test %>% 
  inner_join(truth_test %>% 
               dplyr::rename(tas_mean_truth = tas_mean, 
                             pr_mean_truth = pr_mean, 
                             heating_degree_days_truth = heating_degree_days, 
                             cooling_degree_days_truth = cooling_degree_days,
                             pr_max_truth = pr_max), 
             by = c("RegionName")) %>%
  ### get errors too
  dplyr::mutate(tas_mean_error = tas_mean - tas_mean_truth, 
                pr_mean_error = pr_mean - pr_mean_truth, 
                heating_degree_days_error = heating_degree_days - heating_degree_days_truth,
                cooling_degree_days_error = cooling_degree_days - cooling_degree_days_truth,
                pr_max_error = pr_max - pr_max_truth) %>%
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
  arrange(RegionName, gcm, ic) %>% 
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))

final_test <- test %>% 
  inner_join(shared_meta) %>%
  arrange(RegionName, gcm, ic) %>%
  mutate(gcm = as.factor(gcm), gcm_ic = as.factor(gcm_ic))


# FIT MODELS ---------------------------------------------------------------

t1 <- Sys.time()
mod_means <- mgcv::gam(list(
  tas_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, bs="re"), 
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm, bs="re")
  ),
  family=mvn(d=2), discrete = TRUE,
  control = gam.control(trace = TRUE),
  data = final_train)
Sys.time() - t1


bias_sf <- final_train %>% cbind(as_tibble(mod_means$fitted.values)) %>% 
  filter(gcm == "ACCESS1-3") %>% 
  inner_join(hu4, by=c("RegionName" = "NAME")) %>% 
  st_as_sf()

pal <- colorNumeric(palette = "Spectral", domain = range(bias_sf$V2))
bias_sf %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(V2), color = "black", weight = 0.5, fillOpacity = 0.8) %>%
  addLegend(values = ~V2, pal = pal, title="Modeled Bias")

plot(norm_density(final_train$pr_mean_error - mod_means$fitted.values[,2]), 
     main = "Raw Bias - Smoothed Bias", xlab = "Random Error (Raw Bias - Smoothed Bias)")


pal <- colorNumeric(palette = "Spectral", domain = range(bias_sf$pr_mean_error))
bias_sf %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(pr_mean_error), color = "black", weight = 0.5, fillOpacity = 0.8) %>%
  addLegend(values = ~pr_mean_error, pal = pal, title = "Raw Bias")



t1 <- Sys.time()
mod_degree_days <- mgcv::gam(list(
  cooling_degree_days_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re"), 
  heating_degree_days_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train)
Sys.time() - t1


t1 <- Sys.time()
mod_pr <- mgcv::gam(list(
  pr_mean_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re"), 
  pr_max_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re")
),
family=mvn(d=2), discrete = TRUE,
control = gam.control(trace = TRUE),
data = final_train)
Sys.time() - t1


climate_simulator <- function(model = mod_means, 
                              response = "pr_mean", 
                              response_index = 2,
                              training_df = final_train,
                              prediction_df = final_test, 
                              N = 10000, 
                              grouping_var = "RegionName"){
  # gives 10k bias adjusted simulations per GCM-IC
  response_error_str <- paste0(response, "_error")
  
  yhat <- predict(model, se.fit = TRUE)
  ee <- yhat$fit[,response_index] - training_df[[response_error_str]]
  
  ee_density <- norm_density(ee)
  
  boostrapped_structural_error <- purrr::map2(.x = yhat$fit[,response_index], 
                                                 .y = yhat$se.fit[,response_index], 
                                                 .f = ~ rnorm(N, mean = .x, sd = .y))
  
  bootstrapped_sampling_error <- sample(x = ee_density$x,
                                           replace = TRUE, 
                                           size = length(boostrapped_structural_error),
                                           prob = ee_density$y )
  
  total_bootstrap <- purrr::map2(.x = boostrapped_structural_error, 
                                 .y = bootstrapped_sampling_error, 
                                 .f = ~ .y - .x)
  
  full_uq_spread <- purrr::map2(.x = total_bootstrap, 
                                   .y = prediction_df[[response]], 
                                   .f = ~ .x + .y)
  
  prediction_df$UQ <- full_uq_spread
  
  # interal, goes from lists of lots of simulations to succinct quantiles
  ComputeUQStats <- function(df){
    df$UQ %>% purrr::reduce(c) %>% 
      quantile(x = ., 
               probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
  }
  
  ComputeAllUQStats <- function(df = prediction_df,
                                Grouping_Var = grouping_var){
    
    prediction_df %>%
      split(., f = .[[Grouping_Var]]) %>%
      purrr::map(.f = ~ ComputeUQStats(.x)) %>%
      purrr::reduce(rbind)
  }
  
  uq_stats <- as_tibble(ComputeAllUQStats())
  colnames(uq_stats) <- paste0("hatQ_", c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
  
  prediction_df_summary <- prediction_df %>%
    group_by(RegionName) %>%
    summarise(truth = mean(eval(parse(text = paste0(response, "_truth")))), 
              classic_lo = min(eval(parse(text = paste0(response)))), 
              classic_med = median(eval(parse(text = paste0(response)))), 
              classic_upr = max(eval(parse(text = paste0(response))))
              )
  
  return(prediction_df_summary %>% bind_cols(uq_stats))
  
}

pr_mean_simulations <- climate_simulator(model = mod_means, 
                  response = "pr_mean", 
                  response_index = 2,
                  training_df = final_train,
                  prediction_df = final_test, 
                  N = 10000, 
                  grouping_var = "RegionName") %>%
  inner_join(final_train %>%
               select(RegionName, previous_truth = pr_mean_truth) %>%
               distinct())

pr_mean_simulations[,-1] <- pr_mean_simulations[,-1]^2

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
  dplyr::mutate(hatQ_0.5 = 86400*hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



tas_mean_simulations <- climate_simulator(model = mod_means, 
                                         response = "tas_mean", 
                                         response_index = 1,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_var = "RegionName") %>%
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

cooling_degree_days_simulations <- climate_simulator(model = mod_degree_days, 
                                         response = "cooling_degree_days", 
                                         response_index = 1,
                                         training_df = final_train,
                                         prediction_df = final_test, 
                                         N = 10000, 
                                         grouping_var = "RegionName")%>%
  inner_join(final_train %>%
               select(RegionName, previous_truth = cooling_degree_days_truth) %>%
               distinct())

cooling_degree_days_simulations[,-1] <- cooling_degree_days_simulations[,-1]^2

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


heating_degree_days_simulations <- climate_simulator(model = mod_degree_days, 
                                                    response = "heating_degree_days", 
                                                    response_index = 2,
                                                    training_df = final_train,
                                                    prediction_df = final_test, 
                                                    N = 10000, 
                                                    grouping_var = "RegionName") %>%
  inner_join(final_train %>%
               select(RegionName, previous_truth = heating_degree_days_truth) %>%
               distinct())


heating_degree_days_simulations[,-1] <- heating_degree_days_simulations[,-1]^2

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

pr_max_simulations <- climate_simulator(model = mod_pr, 
                                                     response = "pr_max", 
                                                     response_index = 2,
                                                     training_df = final_train,
                                                     prediction_df = final_test, 
                                                     N = 10000, 
                                                     grouping_var = "RegionName")%>%
  inner_join(final_train %>%
               select(RegionName, previous_truth = pr_max_truth) %>%
               distinct())


pr_max_simulations[,-1] <- pr_max_simulations[,-1]^2

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
  dplyr::mutate(hatQ_0.5 = 86400*hatQ_0.5) %>%
  inner_join(hu4, by = c("RegionName" = "NAME")) %>% 
  st_as_sf()



# 
# pr_mean_simulations[,-1] <- pr_mean_simulations[,-1]^2
# cor(pr_mean_simulations[,-1])
# rmse(pr_mean_simulations$truth, pr_mean_simulations$hatQ_0.5)
# rmse(pr_mean_simulations$truth, pr_mean_simulations$classic_med)
# coverage(x = pr_mean_simulations$truth, 
#          lwr = pr_mean_simulations$hatQ_0.025, 
#          upr = pr_mean_simulations$hatQ_0.975)
# coverage(x = pr_mean_simulations$truth, 
#          lwr = pr_mean_simulations$classic_lo, 
#          upr = pr_mean_simulations$classic_upr)
# 
# cor(tas_mean_simulations[,-1])
# rmse(tas_mean_simulations$truth, tas_mean_simulations$hatQ_0.5)
# rmse(tas_mean_simulations$truth, tas_mean_simulations$classic_med)
# coverage(x = tas_mean_simulations$truth, 
#          lwr = tas_mean_simulations$hatQ_0.025, 
#          upr = tas_mean_simulations$hatQ_0.975)
# coverage(x = tas_mean_simulations$truth, 
#          lwr = tas_mean_simulations$classic_lo, 
#          upr = tas_mean_simulations$classic_upr)
# 
# cooling_degree_days_simulations[,-1] <- cooling_degree_days_simulations[,-1]^2
# cor(cooling_degree_days_simulations[,-1])
# rmse(cooling_degree_days_simulations$truth, cooling_degree_days_simulations$hatQ_0.5)
# rmse(cooling_degree_days_simulations$truth, cooling_degree_days_simulations$classic_med)
# coverage(x = cooling_degree_days_simulations$truth, 
#          lwr = cooling_degree_days_simulations$hatQ_0.025, 
#          upr = cooling_degree_days_simulations$hatQ_0.975)
# coverage(x = cooling_degree_days_simulations$truth, 
#          lwr = cooling_degree_days_simulations$classic_lo, 
#          upr = cooling_degree_days_simulations$classic_upr)
# 
# heating_degree_days_simulations[,-1] <- heating_degree_days_simulations[,-1]^2
# cor(heating_degree_days_simulations[,-1])
# rmse(heating_degree_days_simulations$truth, heating_degree_days_simulations$hatQ_0.5)
# rmse(heating_degree_days_simulations$truth, heating_degree_days_simulations$classic_med)
# coverage(x = heating_degree_days_simulations$truth, 
#          lwr = heating_degree_days_simulations$hatQ_0.025, 
#          upr = heating_degree_days_simulations$hatQ_0.975)
# coverage(x = heating_degree_days_simulations$truth, 
#          lwr = heating_degree_days_simulations$classic_lo, 
#          upr = heating_degree_days_simulations$classic_upr)
# 
# cor(pr_max_simulations[,-1])
# rmse(pr_max_simulations$truth, pr_max_simulations$hatQ_0.5)
# rmse(pr_max_simulations$truth, pr_max_simulations$classic_med)
# coverage(x = pr_max_simulations$truth, 
#          lwr = pr_max_simulations$hatQ_0.025, 
#          upr = pr_max_simulations$hatQ_0.975)
# coverage(x = pr_max_simulations$truth, 
#          lwr = pr_max_simulations$classic_lo, 
#          upr = pr_max_simulations$classic_upr)


len <- 75
cooling_degree_days_simulations[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = cooling_degree_days_simulations$hatQ_0.975[1:len], 
                         ymin = cooling_degree_days_simulations$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = cooling_degree_days_simulations$classic_upr[1:len], 
                         ymin = cooling_degree_days_simulations$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(cooling_degree_days_simulations[1:len,-1]),
                max(cooling_degree_days_simulations[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Mean Cooling Degree Days, 2020-2049, RMSE Ratio:", 
    round(rmse(cooling_degree_days_simulations$truth, cooling_degree_days_simulations$hatQ_0.5)/
      rmse(cooling_degree_days_simulations$truth, cooling_degree_days_simulations$classic_med), 2), 
    ", Coverage:", round(coverage(x = cooling_degree_days_simulations$truth, 
                           lwr = cooling_degree_days_simulations$hatQ_0.025, 
                           upr = cooling_degree_days_simulations$hatQ_0.975), 2))) + 
  ggplot2::ylab("CDD") +
  ggplot2::xlab("Watershed")



len <- 75
cooling_degree_days_simulations_delta[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = delta_truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = cooling_degree_days_simulations_delta$hatQ_0.975[1:len], 
                         ymin = cooling_degree_days_simulations_delta$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = cooling_degree_days_simulations_delta$classic_upr[1:len], 
                         ymin = cooling_degree_days_simulations_delta$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(cooling_degree_days_simulations_delta[1:len,-1]),
                max(cooling_degree_days_simulations_delta[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Change in Mean Cooling Degree Days, 2020-2049, RMSE Ratio:", 
    round(rmse(cooling_degree_days_simulations_delta$delta_truth, cooling_degree_days_simulations_delta$hatQ_0.5)/
            rmse(cooling_degree_days_simulations_delta$delta_truth, cooling_degree_days_simulations_delta$classic_med), 2), 
    ", Coverage:", round(coverage(x = cooling_degree_days_simulations_delta$delta_truth, 
                                  lwr = cooling_degree_days_simulations_delta$hatQ_0.025, 
                                  upr = cooling_degree_days_simulations_delta$hatQ_0.975), 2))) + 
  ggplot2::ylab("CDD") +
  ggplot2::xlab("Watershed")


pal <- colorNumeric(palette = "Spectral", domain = range(cooling_degree_days_simulations_delta_sf$hatQ_0.5), reverse = TRUE)

cooling_degree_days_simulations_delta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend(pal = pal, values = ~ hatQ_0.5)








len <- 75
heating_degree_days_simulations[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = heating_degree_days_simulations$hatQ_0.975[1:len], 
                         ymin = heating_degree_days_simulations$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = heating_degree_days_simulations$classic_upr[1:len], 
                         ymin = heating_degree_days_simulations$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(heating_degree_days_simulations[1:len,-1]),
                max(heating_degree_days_simulations[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Mean Heating Degree Days, 2020-2049, RMSE Ratio:", 
    round(rmse(heating_degree_days_simulations$truth, heating_degree_days_simulations$hatQ_0.5)/
            rmse(heating_degree_days_simulations$truth, heating_degree_days_simulations$classic_med), 2), 
    ", Coverage:", round(coverage(x = heating_degree_days_simulations$truth, 
                                  lwr = heating_degree_days_simulations$hatQ_0.025, 
                                  upr = heating_degree_days_simulations$hatQ_0.975), 2))) + 
  ggplot2::ylab("HDD") +
  ggplot2::xlab("Watershed")



len <- 75
heating_degree_days_simulations_delta[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = delta_truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = heating_degree_days_simulations_delta$hatQ_0.975[1:len], 
                         ymin = heating_degree_days_simulations_delta$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = heating_degree_days_simulations_delta$classic_upr[1:len], 
                         ymin = heating_degree_days_simulations_delta$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(heating_degree_days_simulations_delta[1:len,-1]),
                max(heating_degree_days_simulations_delta[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Change in Mean Heating Degree Days, 2020-2049, RMSE Ratio:", 
    round(rmse(heating_degree_days_simulations_delta$delta_truth, heating_degree_days_simulations_delta$hatQ_0.5)/
            rmse(heating_degree_days_simulations_delta$delta_truth, heating_degree_days_simulations_delta$classic_med), 2), 
    ", Coverage:", round(coverage(x = heating_degree_days_simulations_delta$delta_truth, 
                                  lwr = heating_degree_days_simulations_delta$hatQ_0.025, 
                                  upr = heating_degree_days_simulations_delta$hatQ_0.975), 2))) + 
  ggplot2::ylab("HDD") +
  ggplot2::xlab("Watershed")


pal <- colorNumeric(palette = "Spectral", domain = range(heating_degree_days_simulations_delta_sf$hatQ_0.5), reverse = TRUE)

heating_degree_days_simulations_delta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend(pal = pal, values = ~ hatQ_0.5)



len <- 75
pr_max_simulations[1:len,] %>%
  
  ggplot2::ggplot(aes(x = RegionName, y = truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.8) +
  ggplot2::geom_errorbar(ymax = pr_max_simulations$hatQ_0.975[1:len], 
                         ymin = pr_max_simulations$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.8) +
  ggplot2::geom_errorbar(ymax = pr_max_simulations$classic_upr[1:len], 
                         ymin = pr_max_simulations$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =6)) + 
  ggplot2::ylim(min(pr_max_simulations[1:len,-1]),
                max(pr_max_simulations[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Average Annual 24-Hour Max Precip, 2020-2049, RMSE Ratio:", 
    round(rmse(pr_max_simulations$truth, pr_max_simulations$hatQ_0.5)/
            rmse(pr_max_simulations$truth, pr_max_simulations$classic_med), 2), 
    ", Coverage:", round(coverage(x = pr_max_simulations$truth, 
                                  lwr = pr_max_simulations$hatQ_0.025, 
                                  upr = pr_max_simulations$hatQ_0.975), 2))) + 
  ggplot2::ylab("Average Annual 24-Hour Max Precip (kgm2/s)") +
  ggplot2::xlab("Watershed")


len <- 75
pr_max_simulations_delta[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = delta_truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = pr_max_simulations_delta$hatQ_0.975[1:len], 
                         ymin = pr_max_simulations_delta$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = pr_max_simulations_delta$classic_upr[1:len], 
                         ymin = pr_max_simulations_delta$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(pr_max_simulations_delta[1:len,-1]),
                max(pr_max_simulations_delta[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Change in Average Annual 24-Hour Max Precip, 2020-2049, RMSE Ratio:", 
    round(rmse(pr_max_simulations_delta$delta_truth, pr_max_simulations_delta$hatQ_0.5)/
            rmse(pr_max_simulations_delta$delta_truth, pr_max_simulations_delta$classic_med), 2), 
    ", Coverage:", round(coverage(x = pr_max_simulations_delta$delta_truth, 
                                  lwr = pr_max_simulations_delta$hatQ_0.025, 
                                  upr = pr_max_simulations_delta$hatQ_0.975), 2))) + 
  ggplot2::ylab("Change, Average Annual 24-Hour Max Precip (kgm2/s)") +
  ggplot2::xlab("Watershed")




pal <- colorNumeric(palette = "BrBG", domain = range(pr_max_simulations_delta_sf$hatQ_0.5))

pr_max_simulations_delta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend(pal = pal, values = ~ hatQ_0.5)





len <- 75
pr_mean_simulations[1:len,] %>%
  
  ggplot2::ggplot(aes(x = RegionName, y = truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.8) +
  ggplot2::geom_errorbar(ymax = pr_mean_simulations$hatQ_0.975[1:len], 
                         ymin = pr_mean_simulations$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.8) +
  ggplot2::geom_errorbar(ymax = pr_mean_simulations$classic_upr[1:len], 
                         ymin = pr_mean_simulations$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =6)) + 
  ggplot2::ylim(min(pr_mean_simulations[1:len,-1]),
                max(pr_mean_simulations[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Mean Daily Mean Precip, 2020-2049, RMSE Ratio:", 
    round(rmse(pr_mean_simulations$truth, pr_mean_simulations$hatQ_0.5)/
            rmse(pr_mean_simulations$truth, pr_mean_simulations$classic_med), 2), 
    ", Coverage:", round(coverage(x = pr_mean_simulations$truth, 
                                  lwr = pr_mean_simulations$hatQ_0.025, 
                                  upr = pr_mean_simulations$hatQ_0.975), 2))) + 
  ggplot2::ylab("Annual Mean Daily Precipitation") +
  ggplot2::xlab("Watershed")


len <- 75
pr_mean_simulations_delta[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = delta_truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = pr_mean_simulations_delta$hatQ_0.975[1:len], 
                         ymin = pr_mean_simulations_delta$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = pr_mean_simulations_delta$classic_upr[1:len], 
                         ymin = pr_mean_simulations_delta$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(pr_mean_simulations_delta[1:len,-1]),
                max(pr_mean_simulations_delta[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Change in Mean Precipitation, 2020-2049, RMSE Ratio:", 
    round(rmse(pr_mean_simulations_delta$delta_truth, pr_mean_simulations_delta$hatQ_0.5)/
            rmse(pr_mean_simulations_delta$delta_truth, pr_mean_simulations_delta$classic_med), 2), 
    ", Coverage:", round(coverage(x = pr_mean_simulations_delta$delta_truth, 
                                  lwr = pr_mean_simulations_delta$hatQ_0.025, 
                                  upr = pr_mean_simulations_delta$hatQ_0.975), 2))) + 
  ggplot2::ylab("Precipitation (kgm2/s)") +
  ggplot2::xlab("Watershed")




pal <- colorNumeric(palette = "BrBG", domain = range(pr_mean_simulations_delta_sf$hatQ_0.5))

pr_mean_simulations_delta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend(pal = pal, values = ~ hatQ_0.5)


len <- 75
tas_mean_simulations[1:len,] %>%
  
  ggplot2::ggplot(aes(x = RegionName, y = truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue") +
  ggplot2::geom_errorbar(ymax = tas_mean_simulations$hatQ_0.975[1:len], 
                         ymin = tas_mean_simulations$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red") +
  ggplot2::geom_errorbar(ymax = tas_mean_simulations$classic_upr[1:len], 
                         ymin = tas_mean_simulations$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =6)) + 
  ggplot2::ylim(min(tas_mean_simulations[1:len,-1]),
                max(tas_mean_simulations[1:len,-1])
  ) +
  ggplot2::ggtitle(paste(
    "Mean Daily Mean Temperature, 2020-2049, RMSE Ratio:", 
    round(rmse(tas_mean_simulations$truth, tas_mean_simulations$hatQ_0.5)/
            rmse(tas_mean_simulations$truth, tas_mean_simulations$classic_med), 2), 
    ", Coverage:", round(coverage(x = tas_mean_simulations$truth, 
                                  lwr = tas_mean_simulations$hatQ_0.025, 
                                  upr = tas_mean_simulations$hatQ_0.975), 2))) + 
  ggplot2::ylab("Annual Mean Daily Temperature") +
  ggplot2::xlab("Watershed")



len <- 75
tas_mean_simulations_delta[1:len,] %>%
  ggplot2::ggplot(aes(x = RegionName, y = delta_truth)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = hatQ_0.5), color = "blue", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = tas_mean_simulations_delta$hatQ_0.975[1:len], 
                         ymin = tas_mean_simulations_delta$hatQ_0.025[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_med), color = "red", alpha = 0.7) +
  ggplot2::geom_errorbar(ymax = tas_mean_simulations_delta$classic_upr[1:len], 
                         ymin = tas_mean_simulations_delta$classic_lo[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =5)) + 
  ggplot2::ylim(min(tas_mean_simulations_delta[1:len,-c(1)]),
                max(tas_mean_simulations_delta[1:len,-c(1)])
  ) +
  ggplot2::ggtitle(paste(
    "Change in Mean Temperature, 2020-2049, RMSE Ratio:", 
    round(rmse(tas_mean_simulations_delta$delta_truth, tas_mean_simulations_delta$hatQ_0.5)/
            rmse(tas_mean_simulations_delta$delta_truth, tas_mean_simulations_delta$classic_med), 2), 
    ", Coverage:", round(coverage(x = tas_mean_simulations_delta$delta_truth, 
                                  lwr = tas_mean_simulations_delta$hatQ_0.025, 
                                  upr = tas_mean_simulations_delta$hatQ_0.975), 2))) + 
  ggplot2::ylab("TemperatureChange (K)") +
  ggplot2::xlab("Watershed")




pal <- colorNumeric(palette = "Spectral", domain = range(tas_mean_simulations_delta_sf$hatQ_0.5), reverse = TRUE)

tas_mean_simulations_delta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(hatQ_0.5), fillOpacity = 0.8, weight = 0.5, color='black') %>%
  addLegend(pal = pal, values = ~ hatQ_0.5)


