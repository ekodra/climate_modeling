source("lib/gcm_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"

s3_cmip5_historical_monthly_temperature <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/monthly/historical",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/tas_", Key))

s3_cmip5_historical_monthly_pr <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/monthly/historical",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/pr_", Key))

# 
# s3_cmip5_rcp45_monthly_temperature <- aws.s3::get_bucket(
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

s3_cmip5_rcp85_monthly_temperature <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/monthly/rcp85",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/tas_", Key))




s3_cmip5_rcp85_monthly_pr <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/CMIP5/monthly/rcp85",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/pr_", Key))



s3_wbd <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/USGS_WBD",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() 


# hu4 <- aws.s3::s3read_using(FUN = sf::st_read, 
#                             bucket = file.path(export_bucket, "raw_data/USGS_WBD"),
#                             object = "wbdhu4_a_us_september2018.gdb/")

hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/")

# # all historical temp ------
# dat <- purrr::map(.x = s3_cmip5_historical_monthly_temperature$Key, 
#                   .f = ~ preprocess_gcm(polygon_sf = hu4, 
#                                          nc_key = .x, 
#                                          resolution = "monthly", 
#                                          scenario = "historical", 
#                                          climate_variable = "tas", 
#                                          spatial_defn_field = "NAME"))

dat <- list()

for(i in 1:length(s3_cmip5_historical_monthly_temperature$Key)){
  print(i)
  dat[[i]] <- safely_preprocess_gcm(polygon_sf = hu4, 
                            nc_key = s3_cmip5_historical_monthly_temperature$Key[i], 
                            resolution = "monthly", 
                            scenario = "historical", 
                            climate_variable = "tas", 
                            spatial_defn_field = "NAME")
}

aws.s3::s3saveRDS(dat, 
                  object = "cmip5_monthly_historical_tas_hu4.rds", 
                  bucket = "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"
                  )


dat <- list()

for(i in 1:length(s3_cmip5_historical_monthly_pr$Key)){
  print(i)
  dat[[i]] <- safely_preprocess_gcm(polygon_sf = hu4, 
                                    nc_key = s3_cmip5_historical_monthly_pr$Key[i], 
                                    resolution = "monthly", 
                                    scenario = "historical", 
                                    climate_variable = "pr", 
                                    spatial_defn_field = "NAME")
}

aws.s3::s3saveRDS(dat, 
                  object = "cmip5_monthly_historical_pr_hu4.rds", 
                  bucket = "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"
)








dat <- list()

for(i in 1:length(s3_cmip5_rcp85_monthly_temperature$Key)){
  print(i)
  dat[[i]] <- safely_preprocess_gcm(polygon_sf = hu4, 
                                    nc_key = s3_cmip5_rcp85_monthly_temperature$Key[i], 
                                    resolution = "monthly", 
                                    scenario = "rcp85", 
                                    climate_variable = "tas", 
                                    spatial_defn_field = "NAME")
}

aws.s3::s3saveRDS(dat, 
                  object = "cmip5_monthly_rcp85_tas_hu4.rds", 
                  bucket = "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"
)




dat <- list()

for(i in 1:length(s3_cmip5_rcp85_monthly_pr$Key)){
  print(i)
  dat[[i]] <- safely_preprocess_gcm(polygon_sf = hu4, 
                                    nc_key = s3_cmip5_rcp85_monthly_pr$Key[i], 
                                    resolution = "monthly", 
                                    scenario = "rcp85", 
                                    climate_variable = "pr", 
                                    spatial_defn_field = "NAME")
}

aws.s3::s3saveRDS(dat, 
                  object = "cmip5_monthly_rcp85_pr_hu4.rds", 
                  bucket = "risqinc/preprocessed_gcm_data/usgs_wbd_hu4"
)






