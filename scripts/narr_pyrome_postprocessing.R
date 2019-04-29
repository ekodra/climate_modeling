source("lib/gcm_preprocessing.R")
source("lib/narr_preprocessing.R")
source("lib/climate_metrics.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/preprocessed_narr_data/pyromes"

narr_historical_air <- aws.s3::s3readRDS(object = "narr.air.2m.pyromes.rds", 
                                         bucket = export_bucket)

narr_historical_apcp <- aws.s3::s3readRDS(object = "narr.apcp.pyromes.rds", 
                                          bucket = export_bucket)

narr_historical_rhum <- aws.s3::s3readRDS(object = "narr.rhum.pyromes.rds", 
                                          bucket = export_bucket)

# helpers
replace_inf <- function(x){
  x[x == -Inf] <- 0
  x
}

narr_air_statistics <- daily_air_postprocessing(narr_historical_air)
narr_apcp_statistics <- daily_apcp_postprocessing(narr_historical_apcp)
narr_rhum_statistics <- daily_rhum_postprocessing(narr_historical_rhum)

narr_statistics <- narr_air_statistics %>% 
  inner_join(narr_apcp_statistics) %>%
  inner_join(narr_rhum_statistics)

saveRDS(narr_statistics, "narr_statistics_pyromes.rds")

