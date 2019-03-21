library(risqGrids)
library(sf)
library(sp)
library(ncdf4)
library(dplyr)
library(leaflet)
library(mapview)
library(purrr)
library(tidyr)

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



preprocess_narr_file <- function(narr_key = s3_narr_daily_air.2m$Key[1], 
                                 polygon_sf = hu4,
                                 climate_variable = "air", 
                                 Year = 2000, 
                                 spatial_defn_field = "NAME"){
  nco <- aws.s3::s3read_using( FUN = ncdf4::nc_open, 
                               object = narr_key,
                               bucket = export_bucket )
  
  lon <- ncdf4::ncvar_get(nco, "lon")
  lat <- ncdf4::ncvar_get(nco, "lat")
  x <- ncdf4::ncvar_get(nco, "x")
  y <- ncdf4::ncvar_get(nco, "y")
  time <- ncdf4::ncvar_get(nco, "time")
  dat <- ncdf4::ncvar_get(nco, climate_variable)
  
  ncCrop <- function(single_poly = polygon_sf[152,]){
    
    box <- sf::st_bbox(sf::st_buffer(single_poly, 1)) 
    
    # indicator matrices of belonging
    xy_ind <- (lon >= box[1] & lon <= box[3]) & (lat >= box[2] & lat <= box[4])
    dat_mean <- rep(NA, length(time))
    for(i in 1:length(time)){
      dat_mean[i] <- mean(dat[,,i][xy_ind == TRUE], na.rm = TRUE) 
    }
    
    return(dat_mean)
  }
  
  ts_list <- purrr::map(.x = 1:nrow(polygon_sf),
                        .f = ~ ncCrop(polygon_sf[.x,]))
  
  ts_mat <- as_tibble(purrr::reduce(.x = ts_list, .f = cbind))
  
  begin <- paste0(Year, "-01-01")
  end <- paste0(Year, "-12-31")
  dates <- seq(lubridate::ymd(begin), lubridate::ymd(end), by = "day")
  
  ts_mat <- dplyr::bind_cols(tibble(Date = dates), ts_mat)
  
  colnames(ts_mat)[2:ncol(ts_mat)] <- as.character(polygon_sf[[spatial_defn_field]])
  
  ts_df <- tidyr::gather(ts_mat, key = "RegionName", value = climate_variable, 2:ncol(ts_mat))
  
  ts_df$dataset <- "NARR"
  
  return(ts_df)
  
}
