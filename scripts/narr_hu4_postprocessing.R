source("lib/gcm_preprocessing.R")
source("lib/climate_metrics.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/preprocessed_narr_data/usgs_wbd_hu4"

narr_historical_air <- aws.s3::s3readRDS(object = "narr.air.2m.hu4.rds", 
                                                      bucket = export_bucket)

narr_historical_apcp <- aws.s3::s3readRDS(object = "narr.apcp.hu4.rds", 
                                         bucket = export_bucket)

narr_historical_rhum <- aws.s3::s3readRDS(object = "narr.rhum.hu4.rds", 
                                          bucket = export_bucket)

# helpers
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



daily_apcp_postprocessing <- function(dat = narr_historical_apcp){

  df <- dat %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) %>% # in case not date type
    dplyr::mutate( Year  = lubridate::year(Date), 
                   Month = lubridate::month(Date)) %>% 
    dplyr::group_by (RegionName, Year, Month) %>%
    dplyr::summarise(pr_mean = mean(climate_variable), 
                     pr_max = max(climate_variable), # to mm/day
                     Date_max = Date[which.max(climate_variable)], 
                     wet_days_percent = sum(climate_variable > 1) / dplyr::n(), 
                     cdd = consec_dry_days(climate_variable, th = 1), 
                     cwd = consec_wet_days(climate_variable, th = 1)
    ) %>%
    ungroup() %>%
    dplyr::mutate(cdd = replace_inf(cdd), 
                 cwd = replace_inf(cwd))
  
  return(df)
}

daily_air_postprocessing <- function(dat){
  
  df <- dat %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) %>% # in case not date type
    dplyr::mutate( Year  = lubridate::year(Date), 
                   Month = lubridate::month(Date)) %>% 
    dplyr::group_by (RegionName, Year, Month) %>%
    dplyr::summarise(tas_mean = mean(climate_variable), # to mm/day
                     heating_degree_days = Heating_degree_days(climate_variable), 
                     cooling_degree_days = Cooling_degree_days(climate_variable)
    ) %>%
    ungroup() 

  return(df)
}

daily_rhum_postprocessing <- function(dat){
  
  df <- dat %>%
    na.omit() %>%
    dplyr::mutate( Date = as.Date(Date) ) %>% # in case not date type
    dplyr::mutate( Year  = lubridate::year(Date), 
                   Month = lubridate::month(Date)) %>% 
    dplyr::group_by (RegionName, Year, Month) %>%
    dplyr::summarise(rhum_mean = mean(climate_variable)
    ) %>%
    ungroup() 
  
  return(df)
}

narr_air_statistics <- daily_air_postprocessing(narr_historical_tas)
narr_apcp_statistics <- daily_apcp_postprocessing(narr_historical_apcp)
narr_rhum_statistics <- daily_rhum_postprocessing(narr_historical_apcp)

narr_statistics <- narr_air_statistics %>% 
  inner_join(narr_apcp_statistics) %>%
  inner_join(narr_rhum_statistics)

saveRDS(narr_statistics, "narr_statistics.rds")

