consec_dry_days <- function(x, th = 0.2){
  # x is a time series of precip in mm/24hr here
  y <- sign(x < th)
  lens <- rle(y)$lengths[rle(y)$values == 1]
  return(max(lens))
}

consec_wet_days <- function(x, th = 0.2){
  # x is a time series of precip in mm/24hr here
  y <- sign(x > th)
  lens <- rle(y)$lengths[rle(y)$values == 1]
  return(max(lens))
}
