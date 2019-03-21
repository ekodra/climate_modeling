source("lib/gcm_preprocessing.R")
source("lib/climate_metrics.R")


# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc/raw_data/NARR"

url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/"


years <- 1979:2018

# temperature
air.2m_files <- paste0(url_base, "air.2m.", years, ".nc")
# daily accumulated precip
apcp_files <- paste0(url_base, "apcp.", years, ".nc")
# 2m relative humidity
rhum.2m_files <- paste0(url_base, "rhum.2m.", years, ".nc")

dl_write <- function(f= air.2m_files[1]){
  rf <- gsub(pattern = url_base, x = f, replacement = "")
  download.file(url = f, destfile = rf)
  # nco <- nc_open(rf)
  # risqGrids::connect_s3()
  # aws.s3::s3saveRDS(x = nco, object = paste0(rf, ".rds"), bucket = export_bucket)
  # nc_close(nco)
  # file.remove(rf)
}

for(i in 1:length(air.2m_files)){
  dl_write(f= air.2m_files[i])
}

for(i in 1:length(apcp_files)){
  dl_write(f= apcp_files[i])
}

for(i in 1:length(rhum.2m_files)){
  dl_write(f= rhum.2m_files[i])
}

