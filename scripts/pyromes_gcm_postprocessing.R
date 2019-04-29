source("lib/gcm_preprocessing.R")
source("lib/climate_metrics.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/preprocessed_gcm_data/pyromes"


# helper
replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}

formTibble <- function(obj){
  obj$ts_df$gcm <- obj$gcm
  obj$ts_df$ic <- obj$ic
  obj$ts_df %>% 
    dplyr::mutate(Year = lubridate::year(Date), 
                  Month = lubridate::month(Date)) %>%
    dplyr::select(-Date) 
}


# CMIP5 DAILY MAXIMA PRECIP -----------------------------------------------

source("lib/gcm_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"

s3_cmip5_historical_daily_pr_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_historical_pr",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_rcp85_daily_pr_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_rcp85_pr",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_rcp45_daily_pr_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_rcp45_pr",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_historical_daily_tas_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_historical_tas",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_rcp85_daily_tas_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_rcp85_tas",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 

s3_cmip5_rcp45_daily_tas_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/pyromes/cmip5_daily_rcp45_tas",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


# PROCESS PR --------------------------------------------------------------

# daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[2])

tmp <- list()
for(i in 1:nrow(s3_cmip5_historical_daily_pr_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[i])
}

daily_pr_statistics_historical <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_pr_statistics_historical, "cmip5_daily_pr_statistics_historical_pyromes.rds")


tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp85_daily_pr_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_pr_postprocessing(s3_obj = s3_cmip5_rcp85_daily_pr_preprocessed$Key[i])
}

daily_pr_statistics_rcp85 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_pr_statistics_rcp85, "cmip5_daily_pr_statistics_rcp85_pyromes.rds")


tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp45_daily_pr_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_pr_postprocessing(s3_obj = s3_cmip5_rcp45_daily_pr_preprocessed$Key[i])
}

daily_pr_statistics_rcp45 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_pr_statistics_rcp45, "cmip5_daily_pr_statistics_rcp45_pyromes.rds")


# SAME BUT TAS ------------------------------------------------------------


tmp <- list()
for(i in 1:nrow(s3_cmip5_historical_daily_tas_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_tas_postprocessing(s3_obj = s3_cmip5_historical_daily_tas_preprocessed$Key[i])
}

daily_tas_statistics_historical <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_tas_statistics_historical, "cmip5_daily_tas_statistics_historical_pyromes.rds")


tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp85_daily_tas_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_tas_postprocessing(s3_obj = s3_cmip5_rcp85_daily_tas_preprocessed$Key[i], 
                                       pr_df = daily_pr_statistics_rcp85)
}

daily_tas_statistics_rcp85 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_tas_statistics_rcp85, "cmip5_daily_tas_statistics_rcp85_pyromes.rds")

tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp45_daily_tas_preprocessed)){
  print(i)
  tmp[[i]] <- gcm_daily_tas_postprocessing(s3_obj = s3_cmip5_rcp45_daily_tas_preprocessed$Key[i], 
                                           pr_df = daily_pr_statistics_rcp45)
}

daily_tas_statistics_rcp45 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_tas_statistics_rcp45, "cmip5_daily_tas_statistics_rcp45_pyromes.rds")


