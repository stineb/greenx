calc_anom_ndvi <- function(df, mod, prob = 0.05){
  
  ## record largest CWD events of each year
  biginstances <- df %>% 
    mutate(year = lubridate::year(time)) %>% 
    group_by(year) %>% 
    dplyr::filter(cwd == max(cwd)) %>% 
    pull(iinst_cwd)
  
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
  
  ## z score of anomaly
  vec_ndvi_anom <- df %>% pull(ndvi_anom)
  sd_anom <- sd(vec_ndvi_anom, na.rm = TRUE)
  df <- df %>% 
    mutate(zscore = ndvi_anom / sd_anom)
  
  ## determine the return periods, given cwd (=CWD) and mod (=the fitted extreme value distribution)
  location <- mod$results$par[1]
  scale <- mod$results$par[2]
  shape <- 0  # for Gumbel  mod$results$par[3]
  
  ## calculate probability of X <= x
  df <- df %>% 
    mutate(p_exceedance = exp(-exp((cwd - location)/scale))) %>% 
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
    dplyr::filter(iinst_cwd %in% biginstances) %>% 
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
  
  ## retain only data of NDVI anomaly threshold crossing date (instt = 1) and happening during annual maximum CWD event
  df <- df %>% 
    dplyr::filter(instt == 1 & iinst_cwd %in% biginstances) %>% 
    dplyr::select(time, cwd, return_period, ndvi, ndvi_anom, zscore) %>% 
    mutate(thresh_quantile = prob)
  
  # # xxx try
  # x <- df$cwd[1]  
  # p_exceedance <- 1 - pevd(-x, loc = location, scale = scale, type = "Gumbel")  # doesn't make sense!
  # p_exceedance <- exp(-exp((x - location)/scale))
  
  return(df)

}