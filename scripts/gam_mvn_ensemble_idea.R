library(dplyr)
library(sf)
library(tidyverse)
library(purrr)
library(mgcv)

cmip5 <- readRDS("cmip5_dat.rds")
hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/")

hu4_centroids <- sf::st_centroid(hu4) %>%
  select(NAME, geometry=Shape) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

## use NorESM1-M as artificial holdout
cmip5$historical_tas_all %>%
  dplyr::select(gcm, ic) %>% 
  dplyr::distinct() %>%
  as.data.frame()

# training: 1960-1981
# test: 1982-2005
hist_tas_train <- cmip5$historical_tas_all %>% 
  dplyr::filter(Year >= 1960 & Year <= 1982) %>%
  dplyr::filter(gcm != "NorESM1-M")

hist_tas_test <- cmip5$historical_tas_all %>% 
  dplyr::filter(Year >= 1983 & Year <= 2005) %>%
  dplyr::filter(gcm != "NorESM1-M")

hist_pr_train <- cmip5$historical_pr_all %>% 
  dplyr::filter(Year >= 1960 & Year <= 1982) %>%
  dplyr::filter(gcm != "NorESM1-M") %>%
  dplyr::mutate(climate_variable = sqrt(climate_variable))

hist_pr_test <- cmip5$historical_pr_all %>% 
  dplyr::filter(Year >= 1983 & Year <= 2005) %>%
  dplyr::filter(gcm != "NorESM1-M") %>%
  dplyr::mutate(climate_variable = sqrt(climate_variable))

# synthetic truth for now
hist_tas_train_truth <- cmip5$historical_tas_all %>% 
  dplyr::filter(Year >= 1960 & Year <= 1982) %>%
  dplyr::filter(gcm == "NorESM1-M")

hist_tas_test_truth <- cmip5$historical_tas_all %>% 
  dplyr::filter(Year >= 1983 & Year <= 2005) %>%
  dplyr::filter(gcm == "NorESM1-M")

hist_pr_train_truth <- cmip5$historical_pr_all %>% 
  dplyr::filter(Year >= 1960 & Year <= 1982) %>%
  dplyr::filter(gcm == "NorESM1-M") %>%
  dplyr::mutate(climate_variable = sqrt(climate_variable))

hist_pr_test_truth <- cmip5$historical_pr_all %>% 
  dplyr::filter(Year >= 1983 & Year <= 2005) %>%
  dplyr::filter(gcm == "NorESM1-M") %>%
  dplyr::mutate(climate_variable = sqrt(climate_variable))




# CREATE DATASETS ----------------------------------------------------------

hist_train <- hist_tas_train %>%
  dplyr::rename(tas = climate_variable) %>%
  full_join(hist_pr_train %>% 
              dplyr::rename(pr = climate_variable), 
            by = c("RegionName", "gcm", "ic", "Year", "Month")) %>%
  mutate(gcm_ic = paste(gcm, ic, sep='_')) %>%
  group_by(RegionName, gcm, ic, Month) %>%
  mutate(q = rev(rank(tas)))  %>%
  mutate(p = q/ length(tas)) %>%
  ungroup()


hist_train_truth <- hist_tas_train_truth %>%
  dplyr::rename(tas = climate_variable) %>%
  full_join(hist_pr_train_truth %>% 
              dplyr::rename(pr = climate_variable), 
            by = c("RegionName", "gcm", "ic", "Year", "Month")) %>% 
  mutate(gcm_ic = paste(gcm, ic, sep='_')) %>% 
  group_by(RegionName, gcm, ic, Month) %>%
  mutate(q = rev(rank(tas)))  %>%
  mutate(p = q/ length(tas)) %>%
  ungroup()


hist_test <- hist_tas_test %>%
  dplyr::rename(tas = climate_variable) %>%
  full_join(hist_pr_test %>% 
              dplyr::rename(pr = climate_variable), 
            by = c("RegionName", "gcm", "ic", "Year", "Month")) %>%
  mutate(gcm_ic = paste(gcm, ic, sep='_')) %>%
  group_by(RegionName, gcm, ic, Month) %>%
  mutate(q = rev(rank(tas)))  %>%
  mutate(p = q/ length(tas)) %>%
  ungroup()


hist_train_truth <- hist_tas_train_truth %>%
  dplyr::rename(tas = climate_variable) %>%
  full_join(hist_pr_train_truth %>% 
              dplyr::rename(pr = climate_variable), 
            by = c("RegionName", "gcm", "ic", "Year", "Month")) %>% 
  mutate(gcm_ic = paste(gcm, ic, sep='_')) %>% 
  group_by(RegionName, gcm, ic, Month) %>%
  mutate(q = rev(rank(tas)))  %>%
  mutate(p = q/ length(tas)) %>%
  ungroup()


hist_test_truth <- hist_tas_test_truth %>%
  dplyr::rename(tas = climate_variable) %>%
  full_join(hist_pr_test_truth %>% 
              dplyr::rename(pr = climate_variable), 
            by = c("RegionName", "gcm", "ic", "Year", "Month")) %>% 
  mutate(gcm_ic = paste(gcm, ic, sep='_')) %>% 
  group_by(RegionName, gcm, ic, Month) %>%
  mutate(q = rev(rank(tas)))  %>%
  mutate(p = q/ length(tas)) %>%
  ungroup()



GCM_Errors <- function(yhat, y){
  yhat %>% full_join(y, by = c("RegionName", "Month", "q", "p")) %>%
    mutate(pr_error = pr.x - pr.y, 
           tas_error = tas.x - tas.y) %>%
    select(RegionName, Month, q, p, 
           tas_error, pr_error, 
           gcm = gcm.x, 
           ic = ic.x, 
           gcm_ic = gcm_ic.x, 
           tas_mod = tas.x, 
           pr_mod = pr.x, 
           tas_obs = tas.y, 
           pr_obs = pr.y)
}

# 
# tst <- hist_train %>%
#   split(x = ., f = hist_train$gcm_ic) %>% .[[1]]
# 
# 
# tst_errors <- GCM_Errors(tst, hist_train_truth)
# hist(tst_errors$tas_error[tst_errors$Month == 6])
# 
# tst_errors %>%
# ggplot2::ggplot(aes(x = ))

errors_df <- hist_train %>%
  split(x = ., f = hist_train$gcm_ic) %>%
  purrr::map(.x = ., .f = ~ GCM_Errors(.x, hist_train_truth)) %>% 
  reduce(rbind) %>%
  full_join(hu4_centroids %>% mutate(NAME = as.character(NAME)), by = c("RegionName" = "NAME"))#  %>%
#full_join(hist_train, by = c("RegionName", "gcm", "ic", "p", "q", "Month"))

# couple quick summaries
errors_df %>%
  group_by(gcm) %>%
  summarise(mean(tas_error, na.rm = T), mean(pr_error, na.rm=TRUE)) %>%
  as.data.frame()


# MODELING ----------------------------------------------------------------

modeling_df <- na.omit(errors_df)

modeling_df_climatology <- modeling_df %>%
  group_by(RegionName, lon, lat, gcm, ic, gcm_ic) %>%
  summarise(tas_error = mean(tas_error, na.rm = TRUE), 
            pr_error = mean(pr_error, na.rm = TRUE), 
            tas_mod = mean(tas_mod, na.rm = TRUE), 
            pr_mod = mean(pr_mod, na.rm = TRUE), 
            tas_obs = mean(tas_obs, na.rm = TRUE), 
            pr_obs = mean(pr_obs, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(gcm_ic = as.factor(gcm_ic))#, 
# Month = as.factor(Month))

test_df_climatology <- hist_test %>%
  group_by(RegionName, gcm, ic, gcm_ic) %>%
  summarise( tas_mod = mean(tas, na.rm = TRUE), 
             pr_mod = mean(pr, na.rm = TRUE))%>% 
  ungroup() %>%
  full_join(hu4_centroids %>% mutate(NAME = as.character(NAME)), by = c("RegionName" = "NAME")) %>%
  mutate(gcm_ic = as.factor(gcm_ic))


#hist_test_truth

# 
# modeling_df_tst <- modeling_df %>%
#   filter(gcm == "MPI-ESM-LR") %>% 
#   mutate(Month = as.factor(Month))
# 
# lmod <- lm(tas_error ~ lon+ lat+ p+ Month, data = modeling_df_tst)
# summary(lmod)


# t1 <- Sys.time()
# mod <- mgcv::gam(list(
#   tas_error ~ as.factor(Month) + s(lat, lon, p,  bs = "gp", m = 2), # + s(gcm_ic, bs="re"), 
#   pr_error ~ as.factor(Month) + s(lat, lon, p, bs = "gp", m = 2)),#  + s(gcm_ic, bs="re")),
#   family=mvn(d=2), data = modeling_df_tst)
# Sys.time() - t1


modeling_df_climatology_list <- split(modeling_df_climatology, f = modeling_df_climatology$gcm_ic)


gcms <- as.character(modeling_df_climatology$gcm %>% unique()) %>%
  .[-length(.)]
gcm_ics <- as.character(modeling_df_climatology$gcm_ic %>% unique())

j=length(gcms)

t1 <- Sys.time()
mod <- mgcv::gam(list(
  tas_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re"), 
  pr_error ~ s(lat, lon, bs = "gp", by = gcm, m = 2) + s(gcm_ic, bs="re")),
  family=mvn(d=2), discrete = TRUE,control = gam.control(trace = TRUE),
  data = modeling_df_climatology  %>%
    mutate(gcm = as.factor(gcm)) %>%
   filter(gcm %in% gcms[1:j]))
Sys.time() - t1

runGam <- function(df){
  mod <- mgcv::gam(list(
    tas_error ~ s(lat, lon, bs = "gp", m = 2),#:s(gcm_ic, bs="re"), 
    pr_error ~ s(lat, lon, bs = "gp", m = 2)),# + s(gcm_ic, bs="re")),
    family=mvn(d=2), data = df )
  return(mod)
}

# mods <- purrr::map(.x = modeling_df_climatology_list, .f = ~runGam(.x))
# 
# yhat_list <- purrr::map(.x =mods, .f = ~ predict(.x, se.fit = TRUE) )
# 
# yhat <- list()
# yhat$fit <- purrr::map(.x = yhat_list, .f = ~ .x$fit) %>%
#   reduce(rbind)
# yhat$se.fit <- purrr::map(.x = yhat_list, .f = ~ .x$se.fit) %>%
#   reduce(rbind)

yhat <- predict(mod, se.fit = TRUE)

plot(yhat$fit[,1], modeling_df_climatology$tas_error)
lines(x = c(-100:100), y=c(-100:100))
plot(yhat$fit[,2]  + 3*yhat$se.fit[,2], modeling_df_climatology$pr_error)
lines(x = c(-100:100), y=c(-100:100))

tas_error_error_distribution <- mod$fitted.values[,1] - modeling_df_climatology  %>%
  mutate(gcm = as.factor(gcm)) %>%
  filter(gcm %in% gcms[1:j]) %>%
  .$tas_error
qqnorm(tas_error_error_distribution) 
qqline(tas_error_error_distribution)



pr_error_error_distribution  <- mod$fitted.values[,2] - modeling_df_climatology  %>%
  mutate(gcm = as.factor(gcm)) %>%
  filter(gcm %in% gcms[1:j]) %>%
  .$pr_error

qqnorm(pr_error_error_distribution)
qqline(pr_error_error_distribution)




# "PREDICT" MODEL ON NEW INPUTS ---------------------------------------------

# first join fitted params back onto modeling climatology

holdout_df <- modeling_df_climatology %>% 
  mutate(gcm = as.factor(gcm)) %>%
  filter(gcm %in% gcms[1:j]) %>%
  mutate(gcm_ic = as.factor(gcm_ic)) %>%
  dplyr::select(-tas_mod, -pr_mod, -tas_obs, -pr_obs) %>% 
  bind_cols(tibble(tas_structural_error = yhat$fit[,1], 
                   pr_structural_error = yhat$fit[,2], 
                   tas_structural_error_se = yhat$se.fit[,1],
                   pr_structural_error_se = yhat$se.fit[,2]
                   )) %>%
  left_join(test_df_climatology, by = c("RegionName", "lon", "lat", "gcm", "ic", "gcm_ic")) %>% 
  na.omit() %>% 
  # bias adjusted 
  dplyr::mutate(bias_adjusted_tas = tas_mod - tas_structural_error, 
                bias_adjusted_pr = pr_mod - pr_structural_error)

### bootstrap/oos a set of errors

norm_density <- function(x){
  d <- density(x)
  d$y <- d$y/sum(d$y)
  d
}

tas_e_e_density_normalized <- norm_density(tas_error_error_distribution) 
pr_e_e_density_normalized <- norm_density(pr_error_error_distribution)


# future tas - structural error (bias)

# now bias adjustment + random error component.

# no relationship -- and there shouldn't be!
# plot(holdout_df$tas_structural_error, tas_error_error_distribution)
# plot(holdout_df$pr_structural_error, pr_error_error_distribution)



# BOOTSTRAP: 10k ---------------------------------------------------------------
boostrapped_structural_error_tas <- purrr::map2(.x = holdout_df$tas_structural_error, 
            .y = holdout_df$tas_structural_error_se, 
            .f = ~ rnorm(10000, mean = .x, sd = .y))

bootstrapped_sampling_error_tas <- sample(x = tas_e_e_density_normalized$x,
                                          replace = TRUE, 
                                          size = length(boostrapped_structural_error_tas),
                                          prob =tas_e_e_density_normalized$y )

total_bootstrap <- purrr::map2(.x = boostrapped_structural_error_tas, 
            .y = bootstrapped_sampling_error_tas, 
            .f = ~ .y - .x)

full_uq_spread_tas <- purrr::map2(.x = total_bootstrap, 
                                  .y = holdout_df$tas_mod, 
                                  .f = ~ .x + .y)

full_uq_statistics_tas <- purrr::map(.x = full_uq_spread_tas, 
                                     .f = ~ quantile(x = .x, 
                                                     probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))) %>%
  purrr::reduce(rbind) %>%
  as_tibble(.)

colnames(full_uq_statistics_tas) <- c("tas_0000", 
                                      "tas_0025", "tas_0250", "tas_0500", "tas_0750", "tas_0975", 
                                      "tas_1000")

holdout_df_uq <- holdout_df %>% 
  bind_cols(full_uq_statistics_tas)


final_holdout_prediction_tas <- holdout_df_uq %>% 
  group_by(RegionName) %>%
  dplyr::summarise(lwr_bound = min(tas_0000), 
                   med = median(tas_0500), 
                   upr_bound = max(tas_1000), 
                   classic_lwr = min(tas_mod),
                   classic_mean = median(tas_mod), 
                   classic_upr = max(tas_mod)) %>%
  ungroup() %>%
  full_join(y = hist_tas_test_truth %>%
              group_by(RegionName) %>%
              summarise(tas_true = mean(climate_variable)) %>%
              ungroup())


cor(final_holdout_prediction_tas[,-1] )

len <- 20
final_holdout_prediction_tas[1:len,] %>%

  ggplot2::ggplot(aes(x = RegionName, y = tas_true)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = med), color = "blue") +
  ggplot2::geom_errorbar(ymax = final_holdout_prediction_tas$upr_bound[1:len], 
                         ymin = final_holdout_prediction_tas$lwr_bound[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_mean), color = "red") +
  ggplot2::geom_errorbar(ymax = final_holdout_prediction_tas$classic_upr[1:len], 
                         ymin = final_holdout_prediction_tas$classic_lwr[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =8)) + 
  ggplot2::ylim(min(final_holdout_prediction_tas[1:len,-1]),
                max(final_holdout_prediction_tas[1:len,-1])
                )
  

cor(final_holdout_prediction_tas[,-1])



# SAME BUT FOR PRECIP -----------------------------------------------------

boostrapped_structural_error_pr <- purrr::map2(.x = holdout_df$pr_structural_error, 
                                                .y = holdout_df$pr_structural_error_se, 
                                                .f = ~ rnorm(10000, mean = .x, sd = .y))

bootstrapped_sampling_error_pr <- sample(x = pr_e_e_density_normalized$x,
                                          replace = TRUE, 
                                          size = length(boostrapped_structural_error_pr),
                                          prob =pr_e_e_density_normalized$y )

total_bootstrap <- purrr::map2(.x = boostrapped_structural_error_pr, 
                               .y = bootstrapped_sampling_error_pr, 
                               .f = ~ .y - .x)

full_uq_spread_pr <- purrr::map2(.x = total_bootstrap, 
                                  .y = holdout_df$pr_mod, 
                                  .f = ~ .x + .y)

full_uq_statistics_pr <- purrr::map(.x = full_uq_spread_pr, 
                                     .f = ~ quantile(x = .x, 
                                                     probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))) %>%
  purrr::reduce(rbind) %>%
  as_tibble(.)

colnames(full_uq_statistics_pr) <- c("pr_0000", 
                                      "pr_0025", "pr_0250", "pr_0500", "pr_0750", "pr_0975", 
                                      "pr_1000")

holdout_df_uq <- holdout_df %>% 
  bind_cols(full_uq_statistics_pr)


final_holdout_prediction_pr <- holdout_df_uq %>% 
  group_by(RegionName) %>%
  dplyr::summarise(lwr_bound = min(pr_0000), 
                   med = median(pr_0500), 
                   upr_bound = max(pr_1000), 
                   classic_lwr = min(pr_mod),
                   classic_mean = median(pr_mod), 
                   classic_upr = max(pr_mod)) %>%
  ungroup() %>%
  full_join(y = hist_pr_test_truth %>%
              group_by(RegionName) %>%
              summarise(pr_true = mean(climate_variable)) %>%
              ungroup())


cor(final_holdout_prediction_pr[,-1] )

len <- 20
final_holdout_prediction_pr[1:len,] %>%
  
  ggplot2::ggplot(aes(x = RegionName, y = pr_true)) +
  ggplot2::geom_point() + 
  ggplot2::geom_point(aes(y = med), color = "blue") +
  ggplot2::geom_errorbar(ymax = final_holdout_prediction_pr$upr_bound[1:len], 
                         ymin = final_holdout_prediction_pr$lwr_bound[1:len], color="blue") + 
  ggplot2::geom_point(aes(y = classic_mean), color = "red") +
  ggplot2::geom_errorbar(ymax = final_holdout_prediction_pr$classic_upr[1:len], 
                         ymin = final_holdout_prediction_pr$classic_lwr[1:len], color="red", alpha = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =8)) + 
  ggplot2::ylim(min(final_holdout_prediction_pr[1:len,-1]),
                max(final_holdout_prediction_pr[1:len,-1])
  )

cor(final_holdout_prediction_pr[,-1])



