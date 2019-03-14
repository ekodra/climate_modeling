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


daily_pr_postprocessing <- function(s3_obj){
  dat <- aws.s3::s3readRDS(object = s3_obj, 
                           bucket = export_bucket)
  df <- dat$ts_df %>%
    na.omit() %>%
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


daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[2])

# 
# for(i in 1:nrow(s3_cmip5_historical_daily_pr_preprocessed)){
#   print(i)
#   tmp <- daily_pr_postprocessing(s3_obj = s3_cmip5_historical_daily_pr_preprocessed$Key[i])
# }

daily_pr_statistics_historical <- purrr::map(.x = s3_cmip5_historical_daily_pr_preprocessed$Key, 
           .f = ~ daily_pr_postprocessing(s3_obj = .x)) %>% 
  purrr::reduce(.f = rbind)





