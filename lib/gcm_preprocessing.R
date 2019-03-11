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

# hu4 <- sf::st_read("~/Downloads/wbdhu4_a_us_september2018.gdb/")



# SOME HANDY LEAP YEAR HANDLING DATA ------------------------------------------------
leap_years <- 
  c(1704, 1708, 1712, 1716, 1720, 1724, 1728, 
  1732, 1736, 1740, 1744, 1748, 1752, 1756, 
  1760, 1764, 1768, 1772, 1776, 1780, 1784, 
  1788, 1792, 1796, 1804, 1808, 1812, 1816, 
  1820, 1824, 1828, 1832, 1836, 1840, 1844, 
  1848, 1852, 1856, 1860, 1864, 1868, 1872, 
  1876, 1880, 1884, 1888, 1892, 1896, 1904, 
  1908, 1912, 1916, 1920, 1924, 1928, 1932, 
  1936, 1940, 1944, 1948, 1952, 1956, 1960, 
  1964, 1968, 1972, 1976, 1980, 1984, 1988, 
  1992, 1996, 2000, 2004, 2008, 2012, 2016, 
  2020, 2024, 2028, 2032, 2036, 2040, 2044, 
  2048, 2052, 2056, 2060, 2064, 2068, 2072, 
  2076, 2080, 2084, 2088, 2092, 2096, 2104, 
  2108, 2112, 2116, 2120, 2124, 2128, 2132, 
  2136, 2140, 2144, 2148, 2152, 2156, 2160, 
  2164, 2168, 2172, 2176, 2180, 2184, 2188, 
  2192, 2196)

month_df <- tibble(Month = 1:12, 
                   Days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

leap_month_df <- tibble(Month = 1:12, 
                        Days = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

regular_month_seq <- purrr::map2(.x = month_df$Month, .y = month_df$Days,
                                 .f = ~ rep(.x, .y)) %>% 
  unlist() %>%
  c()

leap_month_seq <- purrr::map2(.x = leap_month_df$Month, .y = leap_month_df$Days,
                              .f = ~ rep(.x, .y)) %>% 
  unlist() %>%
  c()


# MAIN FUNCTIIONS ------
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
  
  meta_fields <- nc_key %>%
    stringr::str_split(pattern = "/") %>% 
    unlist() %>%
    .[length(.)] %>%
    stringr::str_split(pattern = "_") %>%
    unlist()
  
  gcm <- meta_fields[3]
  ic <- meta_fields[5]
 
 if(resolution == "monthly"){
   time_bounds <- nc_key %>%
     stringr::str_split(pattern = "/") %>% 
     unlist() %>%
     .[length(.)] %>%
     stringr::str_split(pattern = "_") %>%
     unlist() %>%
     .[length(.)] %>%
     stringr::str_split(pattern = "-") %>%
     unlist() %>%
     purrr::map(.f = ~ substr(x = .x, 1, 6)) %>%
     unlist() %>% 
     as.numeric()
   
   begin <- lubridate::date(paste0(substr(time_bounds[1], 1, 4), "-", 
                                   substr(time_bounds[1], 5, 6) , 
                                   "-01"))
   
   end <- lubridate::date(paste0(substr(time_bounds[2], 1, 4), "-", 
                                 substr(time_bounds[2], 5, 6) , 
                                 "-01"))
   dates <- seq(lubridate::ymd(begin), lubridate::ymd(end), by = "month")
 }
  
  if(resolution == "daily"){
    
    time_bounds <- nc_key %>%
      stringr::str_split(pattern = "/") %>% 
      unlist() %>%
      .[length(.)] %>%
      stringr::str_split(pattern = "_") %>%
      unlist() %>%
      .[length(.)] %>%
      stringr::str_split(pattern = "-") %>%
      unlist() %>%
      purrr::map(.f = ~ substr(x = .x, 1, 8)) %>%
      unlist() %>% 
      as.numeric()
    
    begin <- lubridate::date(paste0(substr(time_bounds[1], 1, 4), "-", 
                                    substr(time_bounds[1], 5, 6) , "-", 
                                    substr(time_bounds[1], 7, 8)))
    
    end <- lubridate::date(paste0(substr(time_bounds[2], 1, 4), "-", 
                                    substr(time_bounds[2], 5, 6) , "-", 
                                    substr(time_bounds[2], 7, 8)))
    
    dates <- seq(lubridate::ymd(begin), lubridate::ymd(end), by = "day")
    
    yr_df <- tibble(Year = Yr_bounds[1]:Yr_bounds[2], Len = 365)
    
    if(ncf$dim$time$calendar == "proleptic_gregorian"){
      ## leap years -- need to handle
    }
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

    if(length(time) > 1){
      # almost all cases
      vari_mean <- purrr::map2(.x = final_ind[,1], .y = final_ind[,2], .f = ~ vari_data[.x,.y,]) %>% 
        purrr::reduce(cbind) %>%
        t() %>%
        t() %>%
        rowMeans()
      
      return(vari_mean)
    }

    if(length(time) == 1){
      return(mean(vari_data))
    }  
    
  }
  
  ts_list <- purrr::map(.x = 1:nrow(polygon_sf),
             .f = ~ ncCrop(polygon_sf[.x,]))

  
  ts_mat <- as_tibble(purrr::reduce(.x = ts_list, .f = cbind))
  
  # if(gcm %in% c("HadGEM2-CC", "HadGEM-ES")){
  #   ts_mat <- ts_mat[-1,]
  # }
  # 
  ts_mat <- dplyr::bind_cols(tibble(Date = dates), ts_mat)
  
  colnames(ts_mat)[2:ncol(ts_mat)] <- as.character(polygon_sf[[spatial_defn_field]])
  
  ts_df <- tidyr::gather(ts_mat, key = "RegionName", value = climate_variable, 2:ncol(ts_mat))
  
  output <- list(gcm = gcm, ic = ic, resolution = resolution, ts_df = ts_df)
  
  return(output)
}

safely_preprocess_gcm <- purrr::safely(preprocess_gcm)




