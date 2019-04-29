source("lib/gcm_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"
# 
# s3_cmip5_historical_monthly_tas <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/historical",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/tas_", Key))

s3_cmip5_historical_daily_tas <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/historical",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/tas_", Key))


# s3_cmip5_historical_monthly_pr <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/historical",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/pr_", Key))

s3_cmip5_historical_daily_pr <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/historical",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/pr_", Key))

# 
# s3_cmip5_rcp45_monthly_tas <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/rcp45",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/tas_", Key))
# 
# s3_cmip5_rcp45_monthly_pr <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/rcp45",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/pr_", Key))

# s3_cmip5_rcp85_monthly_tas <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/rcp85",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/tas_", Key))
# 
# s3_cmip5_rcp85_monthly_pr <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/CMIP5/monthly/rcp85",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(grepl(pattern = "\\/pr_", Key))




s3_cmip5_rcp85_daily_tas <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/rcp85",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/tas_", Key))

s3_cmip5_rcp85_daily_pr <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/rcp85",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/pr_", Key))


s3_cmip5_rcp45_daily_tas <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/rcp45",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/tas_", Key))
 
s3_cmip5_rcp45_daily_pr <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/daily/rcp45",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/pr_", Key))



# s3_wbd <- aws.s3::get_bucket(
#   bucket = export_bucket,
#   prefix = "raw_data/USGS_WBD",
#   max = Inf
# ) %>%
#   purrr::map(.f = ~`class<-`(.x, "list")) %>%
#   dplyr::bind_rows() 


# pyromes <- aws.s3::s3read_using(FUN = sf::st_read, 
#                             bucket = file.path(export_bucket, "raw_data/USGS_WBD"),
#                             object = "wbdpyromes_a_us_september2018.gdb/")


pyromes <- sf::st_read("Pyrome_20150605/Pyrome_20150605.shp") %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

# # all historical temp ------
# dat <- purrr::map(.x = s3_cmip5_historical_monthly_tas$Key, 
#                   .f = ~ preprocess_gcm(polygon_sf = pyromes, 
#                                          nc_key = .x, 
#                                          resolution = "monthly", 
#                                          scenario = "historical", 
#                                          climate_variable = "tas", 
#                                          spatial_defn_field = "NAME"))
# 
# dat <- list()
# 
# for(i in 1:length(s3_cmip5_historical_monthly_tas$Key)){
#   print(i)
#   dat[[i]] <- preprocess_gcm(polygon_sf = pyromes, 
#                              nc_key = s3_cmip5_historical_monthly_tas$Key[i], 
#                              resolution = "monthly", 
#                              scenario = "historical", 
#                              climate_variable = "tas", 
#                              spatial_defn_field = "NAME")
# }
# 
# aws.s3::s3saveRDS(dat, 
#                   object = "cmip5_monthly_historical_tas_pyromes.rds", 
#                   bucket = "risqinc/preprocessed_gcm_data/pyromes"
# )
# 
# 
# dat <- list()
# 
# for(i in 1:length(s3_cmip5_historical_monthly_pr$Key)){
#   print(i)
#   dat[[i]] <- preprocess_gcm(polygon_sf = pyromes, 
#                              nc_key = s3_cmip5_historical_monthly_pr$Key[i], 
#                              resolution = "monthly", 
#                              scenario = "historical", 
#                              climate_variable = "pr", 
#                              spatial_defn_field = "NAME")
# }
# 
# aws.s3::s3saveRDS(dat, 
#                   object = "cmip5_monthly_historical_pr_pyromes.rds", 
#                   bucket = "risqinc/preprocessed_gcm_data/pyromes"
# )
# 
# 
# 
# dat <- list()
# 
# for(i in 1:length(s3_cmip5_rcp85_monthly_tas$Key)){
#   print(i)
#   dat[[i]] <- preprocess_gcm(polygon_sf = pyromes, 
#                              nc_key = s3_cmip5_rcp85_monthly_tas$Key[i], 
#                              resolution = "monthly", 
#                              scenario = "rcp85", 
#                              climate_variable = "tas", 
#                              spatial_defn_field = "NAME")
# }
# 
# aws.s3::s3saveRDS(dat, 
#                   object = "cmip5_monthly_rcp85_tas_pyromes.rds", 
#                   bucket = "risqinc/preprocessed_gcm_data/pyromes"
# )
# 
# 
# 
# 
# dat <- list()
# 
# for(i in 1:length(s3_cmip5_rcp85_monthly_pr$Key)){
#   print(i)
#   dat[[i]] <- preprocess_gcm(polygon_sf = pyromes, 
#                              nc_key = s3_cmip5_rcp85_monthly_pr$Key[i], 
#                              resolution = "monthly", 
#                              scenario = "rcp85", 
#                              climate_variable = "pr", 
#                              spatial_defn_field = "NAME")
# }
# 
# aws.s3::s3saveRDS(dat, 
#                   object = "cmip5_monthly_rcp85_pr_pyromes.rds", 
#                   bucket = "risqinc/preprocessed_gcm_data/pyromes"
# )
# 
# 


# DAILY -------------------------------------------------------------------

# historical tas
for(i in 1:length(s3_cmip5_historical_daily_tas$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_historical_daily_tas$Key[i], 
                        resolution = "daily", 
                        scenario = "historical", 
                        climate_variable = "tas", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_historical_tas_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}


# historical pr
for(i in 1:length(s3_cmip5_historical_daily_pr$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_historical_daily_pr$Key[i], 
                        resolution = "daily", 
                        scenario = "historical", 
                        climate_variable = "pr", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_historical_pr_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}





# rcp85 tas
for(i in 1:length(s3_cmip5_rcp85_daily_tas$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_rcp85_daily_tas$Key[i], 
                        resolution = "daily", 
                        scenario = "rcp85", 
                        climate_variable = "tas", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_rcp85_tas_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}


# rcp85 pr
for(i in 1:length(s3_cmip5_rcp85_daily_pr$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_rcp85_daily_pr$Key[i], 
                        resolution = "daily", 
                        scenario = "rcp85", 
                        climate_variable = "pr", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_rcp85_pr_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}




# rcp45 tas
for(i in 1:length(s3_cmip5_rcp45_daily_tas$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_rcp45_daily_tas$Key[i], 
                        resolution = "daily", 
                        scenario = "rcp45", 
                        climate_variable = "tas", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_rcp45_tas_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}


# rcp45 pr
for(i in 1:length(s3_cmip5_rcp45_daily_pr$Key)){
  print(i)
  dat <- preprocess_gcm(polygon_sf = pyromes, 
                        nc_key = s3_cmip5_rcp45_daily_pr$Key[i], 
                        resolution = "daily", 
                        scenario = "rcp45", 
                        climate_variable = "pr", 
                        spatial_defn_field = "FPU_CODE")
  
  gcm <- dat$gcm 
  ic <- dat$ic
  begin <- min(dat$ts_df$Date)
  end <- max(dat$ts_df$Date)
  obj_name <- paste0(paste("cmip5_daily_rcp45_pr_pyromes", gcm, ic, begin, end, sep='_'),  ".rds")
  aws.s3::s3saveRDS(dat, 
                    object = obj_name, 
                    bucket = "risqinc/preprocessed_gcm_data/pyromes")
  
}



