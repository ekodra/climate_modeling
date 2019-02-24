library(mgcv)

# these are the variables we want to create

J <-  

  
# make lat long, and order statistic a smooth process term -- either a Markov Random field ("mrf"), 
  # or a Gaussian Process ("gp")
# Why?: b/c we dont expect error distributions to see white noise over space. We wanna model
  # the non random error from a given GCM.
# At the end what you have is something pretty interesting: you've accounted for the 
  # systematic statistical errors of a 
  
mod <- mgcv::gam(list(
  errors_from_observed_gcm_1 ~ s(q, lat, long, bs = "gp", m = 2), 
  errors_from_observed_gcm_2 ~ s(q, lat, long, bs = "gp", m = 2),
  ..., 
  errors_from_observed_gcm_J ~ s(q, lat, long, bs = "gp", m = 2),
  family=mvn(d=J), 
  data = big_huge_dataset)


  x <- rnorm(1000) 
  ID <- rep(1:200,each=5)
  y <- x 
  for(i in 1:200) y[which(ID==i)] <- y[which(ID==i)] + rnorm(1)
  y <- y + rnorm(1000)
  ID <- as.factor(ID)
  m1 <- gam(y ~ x + s(ID,bs="re"))
  m2 <- gamm(y ~ x, random=list(ID=~1) )
  mean( (fitted(m1)-fitted(m2$gam))^2 ) 