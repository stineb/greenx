calc_anom_ndvi <- function(df, mod, prob = 0.05){
  
  ## get mean seasonality
  df_meandoy <- df %>% 
    mutate(doy = lubridate::yday(time)) %>% 
    group_by(doy) %>% 
    summarise(ndvi_mean = mean(MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI, na.rm = TRUE))
  
  ## get anomaly relative to mean seasonal cycle
  df <- df %>% 
    rename(ndvi = MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI) %>% 
    mutate(doy = lubridate::yday(time)) %>% 
    left_join(df_meandoy, by = "doy") %>% 
    mutate(ndvi_anom = ndvi - ndvi_mean)
  
  ## get threshold
  vec_ndvi_anom <- df %>% pull(ndvi_anom)
  sd_anom <- sd(vec_ndvi_anom, na.rm = TRUE)
  
  ## z score of anomaly
  df <- df %>% 
    mutate(zscore = ndvi_anom / sd_anom)
  
  ## determine the return periods, given deficit (=CWD) and mod (=the fitted extreme value distribution)
  location <- mod$results$par[1]
  scale <- mod$results$par[2]
  shape <- 0  # for Gumbel  mod$results$par[3]
  
  ## calculate probability of X <= x
  df <- df %>% 
    mutate(p_exceedance = exp(-exp((deficit - location)/scale))) %>% 
    mutate(return_period = 1/p_exceedance)
  
  ## visualise
  df %>% 
    ggplot(aes(x = ndvi_anom, y = ..count..)) + 
    geom_histogram() +
    geom_vline(xintercept = thresh, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -sd_anom, linetype = "dashed", color = "royalblue") +
    geom_vline(xintercept = 0, linetype = "dotted")
  
  ## ndvi anomaly vs. return period
  df %>% 
    ggplot(aes(x = return_period, y = zscore)) + 
    geom_point()

  ## LOOP OVER THRESHOLDS -- not yet implemented
  ## quantile-threshold
  thresh <- quantile(
    vec_ndvi_anom,
    probs = prob
  )
  
  ## binary classification: above/below threshold
  df <- df %>% 
    mutate(isevent = ifelse(ndvi_anom < thresh, TRUE, FALSE))
  
  ## determine extreme events with more than one consecutive extreme anomaly
  df <- get_iinst(df, leng_threshold = 1)
  
  ## retain only data of NDVI anomaly threshold crossing date (instt = 1)
  df <- df %>% 
    dplyr::select(time, deficit, return_period, ndvi, ndvi_anom, zscore, iinst, instt) %>% 
    dplyr::filter(instt == 1) %>% 
    mutate(thresh_quantile = prob)
  
  # # xxx try
  # x <- df$deficit[1]  
  # p_exceedance <- 1 - pevd(-x, loc = location, scale = scale, type = "Gumbel")  # doesn't make sense!
  # p_exceedance <- exp(-exp((x - location)/scale))
  
  return(df)

}