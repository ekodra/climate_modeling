source("lib/gcm_preprocessing.R")
source("lib/climate_metrics.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"


# helper
replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}

# MONTHLY -----------------------------------------------------------------


cmip5_monthly_historical_tas_hu4 <- aws.s3::s3readRDS(object = "cmip5_monthly_historical_tas_hu4.rds", 
                  bucket = export_bucket)

cmip5_monthly_historical_pr_hu4 <- aws.s3::s3readRDS(object = "cmip5_monthly_historical_pr_hu4.rds", 
                                                      bucket = export_bucket)

cmip5_monthly_rcp85_tas_hu4 <- aws.s3::s3readRDS(object = "cmip5_monthly_rcp85_tas_hu4.rds", 
                                                      bucket = export_bucket)

cmip5_monthly_rcp85_pr_hu4 <- aws.s3::s3readRDS(object = "cmip5_monthly_rcp85_pr_hu4.rds", 
                                                     bucket = export_bucket)



# CREATE DF FOR ANALYSIS --------------------------------------------------

historical_tas_meta <- dplyr::bind_cols(
  gcm = (purrr::map(.x = cmip5_monthly_historical_tas_hu4, .f= ~ .x$gcm) %>%
  unlist()), 
  ic = (purrr::map(.x = cmip5_monthly_historical_tas_hu4, .f= ~ .x$ic) %>%
    unlist()), 
  year_min = unlist((purrr::map(.x = cmip5_monthly_historical_tas_hu4, 
                         .f= ~ min(lubridate::year(.x$ts_df$Date)) %>%
                unlist() %>% unlist()))), 
  year_max = unlist((purrr::map(.x = cmip5_monthly_historical_tas_hu4, 
                         .f= ~ max(lubridate::year(.x$ts_df$Date)) %>%
                           unlist() %>%c ())))
  ) %>%
  dplyr::group_by(gcm, ic) %>% 
  dplyr::summarise(year_min = min(year_min), year_max = max(year_max)) %>%
  dplyr::ungroup()

# overall years to keep
lwr_yr_historical <- max(historical_tas_meta$year_min)
upr_yr_historical <- min(historical_tas_meta$year_max)
med_yr_historical <- median(lwr_yr:upr_yr)



formTibble <- function(obj){
  obj$ts_df$gcm <- obj$gcm
  obj$ts_df$ic <- obj$ic
  obj$ts_df %>% 
    dplyr::mutate(Year = lubridate::year(Date), 
                  Month = lubridate::month(Date)) %>%
    dplyr::select(-Date) 
}
       

historical_tas_all <- purrr::map(.x = cmip5_monthly_historical_tas_hu4, 
           .f = ~ formTibble(.x)) %>%
  purrr::reduce(rbind)

historical_pr_all <- purrr::map(.x = cmip5_monthly_historical_pr_hu4, 
                                 .f = ~ formTibble(.x)) %>%
  purrr::reduce(rbind)

rcp85_tas_all <- purrr::map(.x = cmip5_monthly_rcp85_tas_hu4, 
                                 .f = ~ formTibble(.x)) %>%
  purrr::reduce(rbind)

rcp85_pr_all <- purrr::map(.x = cmip5_monthly_rcp85_pr_hu4, 
                                .f = ~ formTibble(.x)) %>%
  purrr::reduce(rbind)



tmp <- list(historical_tas_all = historical_tas_all, 
            historical_pr_all = historical_pr_all, 
            rcp85_tas_all = rcp85_tas_all, 
            rcp85_pr_all = rcp85_pr_all 
            )

saveRDS(tmp, "cmip5_monthly_dat.rds") # put on flash drive.



# CMIP5 DAILY MAXIMA PRECIP -----------------------------------------------

source("lib/gcm_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"

s3_cmip5_historical_daily_pr_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/usgs_wbd_hu4/cmip5_daily_historical_pr",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_rcp85_daily_pr_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/usgs_wbd_hu4/cmip5_daily_rcp85_pr",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_historical_daily_tas_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/usgs_wbd_hu4/cmip5_daily_historical_tas",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


s3_cmip5_rcp85_daily_tas_preprocessed <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "preprocessed_gcm_data/usgs_wbd_hu4/cmip5_daily_rcp85_tas",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 



# PROCESS PR --------------------------------------------------------------
daily_pr_postprocessing <- function(s3_obj){
  dat <- aws.s3::s3readRDS(object = s3_obj, 
                           bucket = export_bucket)
  df <- dat$ts_df %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) %>% # in case not date type
    dplyr::mutate( Year  = lubridate::year(Date), 
                   Month = lubridate::month(Date)) %>% 
    dplyr::mutate(pr_mm = 86400*climate_variable) %>%
    dplyr::group_by (RegionName, Year, Month) %>%
    dplyr::summarise(pr_mm_max = max(pr_mm ), # to mm/day
                     Date_max = Date[which.max(pr_mm)], 
                     wet_days_percent = sum(pr_mm > 1) / dplyr::n(), 
                     cdd = consec_dry_days(pr_mm, th = 1), 
                     cwd = consec_wet_days(pr_mm, th = 1)
    ) %>%
    ungroup() %>%
    dplyr::mutate(gcm = dat$gcm, ic = dat$ic) %>%
    dplyr::mutate(gcm_ic = paste(gcm, ic, sep='_')) %>%
    dplyr::mutate(cdd = replace_inf(cdd), 
                  cwd = replace_inf(cwd))
  
  return(df)
}


# daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[2])

tmp <- list()
for(i in 1:nrow(s3_cmip5_historical_daily_pr_preprocessed)){
  print(i)
  tmp[[i]] <- daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[i])
}

daily_pr_statistics_historical <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_pr_statistics_historical, "cmip5_daily_pr_statistics_historical.rds")


tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp85_daily_pr_preprocessed)){
  print(i)
  tmp[[i]] <- daily_pr_postprocessing(s3_obj = s3_cmip5_rcp85_daily_pr_preprocessed$Key[i])
}

daily_pr_statistics_rcp85 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_pr_statistics_rcp85, "cmip5_daily_pr_statistics_rcp85.rds")


# SAME BUT TAS ------------------------------------------------------------
# include: cdd, hdd, deviation from mean temp during extreme rainfall, mean temp
daily_tas_postprocessing <- function(s3_obj = s3_cmip5_historical_daily_tas_preprocessed$Key[1], 
                                     pr_df = daily_pr_statistics_historical){
  
  
  dat <- aws.s3::s3readRDS(object = s3_obj, 
                           bucket = export_bucket)
  
  df_pr_relation <- dat$ts_df %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) # in case not date type
  
  
  # the piece where you get mean tas on the day of precip event
  tas_pr_df <- pr_df %>%
    dplyr::filter(gcm == dat$gcm & ic == dat$ic) %>%
    dplyr::select(RegionName, Year, Month, Date_max) %>%
    full_join(df_pr_relation, by = c("RegionName", "Date_max" = "Date")) %>% 
    dplyr::rename(tas_prMaxima = climate_variable) 
  
  df <- dat$ts_df %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) %>% # in case not date type
    dplyr::mutate( Year  = lubridate::year(Date), 
                   Month = lubridate::month(Date)) %>% 
    dplyr::group_by (RegionName, Year, Month) %>%
    dplyr::summarise(tas_mean = mean(climate_variable), # to mm/day
                     heating_degree_days = Heating_degree_days(climate_variable), 
                     cooling_degree_days = Cooling_degree_days(climate_variable)
    ) %>%
    ungroup() %>%
    dplyr::mutate(gcm = dat$gcm, ic = dat$ic) %>%
    dplyr::mutate(gcm_ic = paste(gcm, ic, sep='_')) %>%
    inner_join(  tas_pr_df, by = c("RegionName", "Year", "Month")) %>% 
    na.omit() %>%
    dplyr::mutate(pr_maxima_tas_anomaly = tas_prMaxima - tas_mean)
  
  df %>% group_by(Month) %>% summarise(mean(pr_maxima_tas_anomaly))
  
  return(df)
}


tmp <- list()
for(i in 1:nrow(s3_cmip5_historical_daily_tas_preprocessed)){
  print(i)
  tmp[[i]] <- daily_tas_postprocessing(s3_obj = s3_cmip5_historical_daily_tas_preprocessed$Key[i])
}

daily_tas_statistics_historical <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_tas_statistics_historical, "cmip5_daily_tas_statistics_historical.rds")


tmp <- list()
for(i in 1:nrow(s3_cmip5_rcp85_daily_tas_preprocessed)){
  print(i)
  tmp[[i]] <- daily_tas_postprocessing(s3_obj = s3_cmip5_rcp85_daily_tas_preprocessed$Key[i])
}

daily_tas_statistics_rcp85 <- purrr::reduce(.x = tmp, .f = rbind)

saveRDS(daily_tas_statistics_rcp85, "cmip5_daily_tas_statistics_rcp85.rds")


