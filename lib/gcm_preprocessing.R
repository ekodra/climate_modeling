library(risqGrids)
library(sf)
library(sp)
library(ncdf4)
library(dplyr)
library(leaflet)
library(mapview)
library(purrr)
library(tidyr)



# Set up environment ------------------------------------------------------

risqGrids::connect_s3()
export_bucket <- "risqinc"

# test
# hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/")


# MAIN FUNCTIONS ------------------------------------------------------
preprocess_gcm <- function(polygon_sf = hu4, 
                           nc_key = s3_cmip5_historical_monthly_temperature$Key[1], 
                           resolution = "monthly", 
                           scenario = "historical", 
                           climate_variable = "tas", 
                           spatial_defn_field = "NAME") {
  # boundary_sf: should be a polygon that provides the boundaries of interest
  # nc_key: AWS S3 key linked to the gcm nc file of interest
  # resolution defaults to monthly. choices are "daily" and "monthly"
  # scenario is "historical", "rcp45", or "rcp85"
  # climate_variable is "tas" or "pr" (temperature and precipitation)
  # spatial_defn_field is the field you want to get from polygon_sf so you can link this back to a polygon
  
  risqGrids::connect_s3()
  export_bucket <- "risqinc"  
  
  ncf <- aws.s3::s3read_using(FUN = ncdf4::nc_open, 
                              bucket = export_bucket, 
                              object = nc_key)
  
  lon <- ncvar_get(nc = ncf, varid = "lon")
  lon[lon > 180] <- lon[lon > 180] - 360
  lat <- ncvar_get(nc = ncf, varid = "lat")
  time <- ncvar_get(nc = ncf, varid = "time")
  
  Yr_bounds <- nc_key %>%
    stringr::str_split(pattern = "/") %>% 
    unlist() %>%
    .[length(.)] %>%
    stringr::str_split(pattern = "_") %>%
    unlist() %>%
    .[length(.)] %>%
    stringr::str_split(pattern = "-") %>%
    unlist() %>%
    purrr::map(.f = ~ substr(x = .x, 1, 4)) %>%
    unlist() %>% 
    as.numeric()
  
  if(resolution == "monthly"){
    Year <- purrr::map(.x = Yr_bounds[1]:Yr_bounds[2], .f = ~ rep(.x, 12)) %>% 
      unlist() %>%
      c()
    
    Month <- rep(1:12, length(unique(Year)))
  }
  
  # for each polygon, execute a cropping.
  ncCrop <- function(single_poly = polygon_sf[152,]){
    
    box <- sf::st_bbox(sf::st_buffer(single_poly, 1)) 
      
    lon_ind <- which(lon >= box[1] & lon <= box[3])
    lat_ind <- which(lat >= box[2] & lat <= box[4])
    
    lon_lat_grid <- expand.grid(lon[lon_ind], lat[lat_ind]) %>%
      st_as_sf(coords = c("Var1", "Var2"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    
    index <- sf::st_intersects(x = single_poly, lon_lat_grid) %>% 
      unlist()
    
    lon_lat <- expand.grid(lon[lon_ind], lat[lat_ind])
    
    if(length(index) > 0){
      keep <- lon_lat[index,] 
    }
    
    if(length(index) == 0 & nrow(lon_lat) > 0){
      keep <- lon_lat
    }
    
    if(length(index) == 0 & nrow(lon_lat) <= 0){
      cat("no data!!!!\n") # debugger
      return(rep(NA, length(time)))
    }
    
  
    pairs <- keep
    for(i in 1:nrow(keep)){
      pairs[i,1] <- which(lon == keep[i,1])
      pairs[i,2] <- which(lat == keep[i,2])
    }
    
    vari_data <- ncdf4::ncvar_get(ncf, 
                                 varid = climate_variable, 
                                 start = c(lon_ind[1], lat_ind[1], 1), 
                                 count = c(length(lon_ind), length(lat_ind), length(time))
    )
    
    if(length(dim(vari_data))==1){
      
      return(vari_data)
    }
    
    if((length(lon_ind) == 1) & (length(lat_ind) > 1)){
      blankArray <- array(NA, dim = c(1, length(lat_ind), length(time)))
      blankArray[1,,] <- vari_data
      vari_data <- blankArray
    }
    
    if((length(lon_ind) > 1) & (length(lat_ind) == 1)){
      blankArray <- array(NA, dim = c(length(lon_ind), 1, length(time)))
      blankArray[,1,] <- vari_data
      vari_data <- blankArray
    }
    
    final_ind <- pairs
    for(i in 1:nrow(pairs)){
      final_ind[i,] <- c(which(lon_ind == pairs[i,1]), 
                         which(lat_ind == pairs[i,2]))
    }

    vari_mean <- purrr::map2(.x = final_ind[,1], .y = final_ind[,2], .f = ~ vari_data[.x,.y,]) %>% 
      purrr::reduce(cbind) %>%
      t() %>%
      t() %>%
      rowMeans()
    
    vari_mean
    
  }
  
  ts_list <- purrr::map(.x = 1:nrow(polygon_sf),
             .f = ~ ncCrop(polygon_sf[.x,]))

  
  ts_mat <- as_tibble(purrr::reduce(.x = ts_list, .f = cbind))
  
  ts_mat <- bind_cols(tibble(Year=Year, Month=Month), ts_mat)
  
  colnames(ts_mat)[3:ncol(ts_mat)] <- as.character(polygon_sf[[spatial_defn_field]])
  
  ts_df <- tidyr::gather(ts_mat, key = "RegionName", value = climate_variable, 3:ncol(ts_mat))
  
  
  meta_fields <- nc_key %>%
    stringr::str_split(pattern = "/") %>% 
    unlist() %>%
    .[length(.)] %>%
    stringr::str_split(pattern = "_") %>%
    unlist()
  
  gcm <- meta_fields[3]
  ic <- meta_fields[5]
  
  output <- list(gcm = gcm, ic = ic, resolution = resolution, ts_df = ts_df)
  
  return(output)
}






