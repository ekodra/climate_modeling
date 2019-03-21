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

kelvin_2_fareinheit <- function(x){
  ((x - 273.15) * 9/5) + 32
}

Cooling_degree_days <- function(x){
  y <- kelvin_2_fareinheit(x) - 65
  y[y <= 0] <- 0
  return(sum(y))
}

Heating_degree_days <- function(x){
  y <- kelvin_2_fareinheit(x) - 65
  y[y >= 0] <- 0
  return(sum(abs(y)))
}

rmse <- function(x, y){
  sqrt(mean((x-y)^2))
}

coverage <- function(x, lwr, upr){
  mean(x <= upr & x >= lwr)
}
