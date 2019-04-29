source("lib/narr_preprocessing.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"


s3_narr_daily_air.2m <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/NARR",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/air.2m", Key))

s3_narr_daily_apcp <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/NARR",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/apcp", Key))

s3_narr_daily_rhum <- aws.s3::get_bucket(
  bucket = export_bucket,
  prefix = "raw_data/NARR",
  max = Inf
) %>%
  purrr::map(.f = ~`class<-`(.x, "list")) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(grepl(pattern = "\\/rhum", Key))


pyromes <- sf::st_read("Pyrome_20150605/Pyrome_20150605.shp") %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")

Years <- 1979:2018


air_list <- list()

for(i in 1:length(s3_narr_daily_air.2m$Key)){
  print(i)
  air_list[[i]] <- preprocess_narr_file(narr_key = s3_narr_daily_air.2m$Key[i], 
                       polygon_sf = pyromes,
                       climate_variable = "air", 
                       Year = Years[i], 
                       spatial_defn_field = "FPU_CODE")
}

air_df <- purrr::reduce(air_list, rbind)

aws.s3::s3saveRDS(x = air_df, 
                  object = "narr.air.2m.pyromes.rds", 
                  bucket = "risqinc/preprocessed_narr_data/pyromes")



apcp_list <- list()

for(i in 1:length(s3_narr_daily_apcp$Key)){
  print(i)
  apcp_list[[i]] <- preprocess_narr_file(narr_key = s3_narr_daily_apcp$Key[i], 
                                        polygon_sf = pyromes,
                                        climate_variable = "apcp", 
                                        Year = Years[i], 
                                        spatial_defn_field = "FPU_CODE")
}

apcp_df <- purrr::reduce(apcp_list, rbind)

aws.s3::s3saveRDS(x = apcp_df, 
                  object = "narr.apcp.pyromes.rds", 
                  bucket = "risqinc/preprocessed_narr_data/pyromes")



rhum_list <- list()

for(i in 1:length(s3_narr_daily_rhum$Key)){
  print(i)
  rhum_list[[i]] <- preprocess_narr_file(narr_key = s3_narr_daily_rhum$Key[i], 
                                        polygon_sf = pyromes,
                                        climate_variable = "rhum", 
                                        Year = Years[i], 
                                        spatial_defn_field = "FPU_CODE")
}

rhum_df <- purrr::reduce(rhum_list, rbind)

aws.s3::s3saveRDS(x = rhum_df, 
                  object = "narr.rhum.pyromes.rds", 
                  bucket = "risqinc/preprocessed_narr_data/pyromes")

