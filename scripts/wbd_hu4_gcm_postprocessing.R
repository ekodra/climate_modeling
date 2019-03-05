source("lib/gcm_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"



# MONTHLY -----------------------------------------------------------------

# READ IN OBJECTS ---------------------------------------------------------


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

saveRDS(tmp, "cmip5_dat.rds") # put on flash drive.




# split into hist and future
historical_tas_all_hist <-  historical_tas_all %>%
  dplyr::filter(Year < med_yr) %>% 
  dplyr::group_by()


