library(dplyr)
library(sf)
library(tidyverse)
library(purrr)
library(mgcv) # key lib that has the modeling functions
library(rmapshaper)


norm_density <- function(x){
  d <- density(x)
  d$y <- d$y/sum(d$y)
  d
}

climate_simulator <- function(model = mod_means, 
                              response = "pr_mean", 
                              response_index = 2,
                              training_df = final_train,
                              prediction_df = final_test, 
                              N = 10000, 
                              grouping_vars = c("RegionName", "Month")){
  # gives 10k bias adjusted simulations per GCM-IC
  response_error_str <- paste0(response, "_error")
  
  yhat <- predict(model, newdata = training_df, se.fit = TRUE)
  ee <- yhat$fit[,response_index] - training_df[[response_error_str]]
  
  ee_density <- norm_density(ee)
  
  boostrapped_structural_error <- purrr::map2(.x = yhat$fit[,response_index], 
                                              .y = yhat$se.fit[,response_index], 
                                              .f = ~ rnorm(N, mean = .x, sd = .y))
  
  bootstrapped_sampling_error <- sample(x = ee_density$x,
                                        replace = TRUE, 
                                        size = length(boostrapped_structural_error),
                                        prob = ee_density$y )
  
  total_bootstrap <- purrr::map2(.x = boostrapped_structural_error, 
                                 .y = bootstrapped_sampling_error, 
                                 .f = ~ .y - .x)
  
  full_uq_spread <- purrr::map2(.x = total_bootstrap, 
                                .y = prediction_df[[response]], 
                                .f = ~ .x + .y)
  
  prediction_df$UQ <- full_uq_spread
  
  # interal, goes from lists of lots of simulations to succinct quantiles
  ComputeUQStats <- function(df){
    df$UQ %>% purrr::reduce(c) %>% 
      quantile(x = ., 
               probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
  }
  
  ComputeAllUQStats <- function(df = prediction_df,
                                Grouping_Vars = grouping_vars){

    df$Group <- apply(FUN = paste0, collapse = "_", X = df[,Grouping_Vars], MARGIN = 1)
    df %>%
      split(., f = .$Group) %>%
      purrr::map(.f = ~ ComputeUQStats(.x)) %>%
      purrr::reduce(rbind)
  }
  
  uq_stats <- as_tibble(ComputeAllUQStats())
  colnames(uq_stats) <- paste0("hatQ_", c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
  
  prediction_df$Group <- apply(FUN = paste0, collapse = "__", X = prediction_df[,grouping_vars], MARGIN = 1)
  
  prediction_df_summary <- prediction_df %>%
    group_by(Group) %>%
    summarise(truth = try(mean(eval(parse(text = paste0(response, "_truth")))), silent = T), 
              classic_lo = min(eval(parse(text = paste0(response)))), 
              classic_med = median(eval(parse(text = paste0(response)))), 
              classic_upr = max(eval(parse(text = paste0(response))))
    ) %>%
    tidyr::separate(data = ., col = Group, into = grouping_vars, sep = "__") %>% 
    ungroup()
  
  
  training_df$Group <- apply(FUN = paste0, collapse = "__", X = training_df[,grouping_vars], MARGIN = 1)
  
  previous_truth_df <- training_df %>%
    group_by(Group) %>%
    summarise(previous_truth = mean(eval(parse(text = paste0(response, "_truth"))))) %>%
    tidyr::separate(data = ., col = Group, into = grouping_vars, sep = "__") %>% 
    ungroup()
  
  out <- prediction_df_summary %>% 
    bind_cols(uq_stats) %>% 
    inner_join(previous_truth_df, by = c("RegionName", "Season"))
  
  return(out)
  
}




