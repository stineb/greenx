calc_anom_ndvi <- function(df, mod, prob = 0.05){
  
  # ## monthly CWD maxima vs. NDVI
  # df %>% 
  #   mutate(month = month(time), year = year(time)) %>% 
  #   group_by(month, year) %>% 
  #   summarise(cwd = max(cwd, na.rm = TRUE), ndvi_anom = mean(ndvi_anom, na.rm = TRUE)) %>% 
  #   ggplot(aes(x = cwd, y = ndvi_anom)) +
  #   geom_point()
  
  continue <- TRUE
  if (sum(!is.na(df$MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI))<3) continue <- FALSE
  if (sum(!is.na(df$cwd))<3) continue <- FALSE
  
  if (continue){
    ## get mean seasonality
    df_meandoy <- df %>% 
      mutate(doy = lubridate::yday(time)) %>% 
      group_by(doy) %>% 
      summarise(ndvi_mean = mean(MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI, na.rm = TRUE)) %>% 
      ungroup()
    
    ## get anomaly relative to mean seasonal cycle
    df <- df %>% 
      rename(ndvi = MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI) %>% 
      mutate(doy = lubridate::yday(time)) %>% 
      left_join(df_meandoy, by = "doy") %>% 
      mutate(ndvi_anom = ndvi - ndvi_mean)
    
    ## record largest CWD events of each year
    df_annmax <- df %>% 
      mutate(year = lubridate::year(time)) %>% 
      group_by(year) %>% 
      dplyr::filter(cwd == max(cwd)) %>% 
      mutate(year = lubridate::year(time), month = lubridate::month(time)) %>% 
      dplyr::select(-ndvi_anom) %>% 
      left_join(df %>% 
                  mutate(year = lubridate::year(time), month = lubridate::month(time)) %>% 
                  dplyr::select(year, month, ndvi_anom) %>% 
                  dplyr::filter(!is.na(ndvi_anom)),
                by = c("month", "year")) %>% 
      ungroup()
    
    ggplot() +
      geom_point(data = df, aes(cwd, ndvi_anom)) +
      geom_point(data = df_annmax, aes(cwd, ndvi_anom), color = "red")
    
    ## z score of anomaly
    vec_ndvi_anom <- df_annmax %>% pull(ndvi_anom)
    sd_anom <- sd(vec_ndvi_anom, na.rm = TRUE)
    df <- df %>% 
      mutate(zscore = ndvi_anom / sd_anom)
    
    ## determine the return periods, given cwd (=CWD) and mod (=the fitted extreme value distribution)
    location <- mod$results$par[1]
    scale <- mod$results$par[2]
    shape <- 0  # for Gumbel  mod$results$par[3]
    
    ## calculate probability of X <= x
    # df <- df %>% 
    #   mutate(p_exceedance = 1 - pevd(cwd, loc = location, scale = scale, type = "Gumbel")) %>% 
    #   # mutate(p_exceedance = exp(-exp(-(cwd - location)/scale))) %>% 
    #   mutate(return_period = 1/p_exceedance)
    
    df_annmax <- df_annmax %>% 
      mutate(p_exceedance = 1 - pevd(cwd, loc = location, scale = scale, type = "Gumbel")) %>% 
      # mutate(p_exceedance = exp(-exp(-(cwd - location)/scale))) %>% 
      mutate(return_period = 1/p_exceedance)
    
    # ggplot(data = df_annmax, aes(log(return_period), ndvi_anom)) +
    #   # geom_point(data = df, aes((return_period), ndvi_anom)) +
    #   geom_point(color = "red") +
    #   geom_smooth(method = "lm")
    
    # df_annmax %>% 
    #   mutate(log_rp = log(return_period)) %>% 
    #   analyse_modobs2("ndvi_anom", "log_rp")
    
    # df_annmax %>% 
    #   mutate(week = month(time)) %>% 
    #   ggplot(aes(week, ..count..)) +
    #   geom_histogram()
    
    # ## visualise ndvi anomaly vs. cwd, ndvi anomaly vs. return period
    # df %>% 
    #   dplyr::filter(iinst_cwd %in% biginstances) %>% 
    #   ggplot(aes(x = return_period, y = zscore)) + 
    #   geom_point()
    
    # ## LOOP OVER THRESHOLDS -- not yet implemented
    # ## quantile-threshold
    # thresh <- quantile(
    #   vec_ndvi_anom,
    #   probs = prob,
    #   na.rm = TRUE
    # )
    
    # ## visualise distribution of NDVI anomalies
    # df %>% 
    #   ggplot(aes(x = ndvi_anom, y = ..count..)) + 
    #   geom_histogram() +
    #   geom_vline(xintercept = thresh, linetype = "dashed", color = "red") +
    #   geom_vline(xintercept = -sd_anom, linetype = "dashed", color = "royalblue") +
    #   geom_vline(xintercept = 0, linetype = "dotted")
    
    # ## binary classification: above/below threshold
    # df <- df %>% 
    #   mutate(isevent = ifelse(ndvi_anom < thresh, TRUE, FALSE))
    # 
    # ## determine extreme events with more than one consecutive extreme anomaly
    # df <- get_iinst(df, leng_threshold = 1)
    # 
    # ## retain only data of NDVI anomaly threshold crossing date (instt = 1) and happening during annual maximum CWD event
    # df <- df %>% 
    #   # dplyr::filter(instt == 1 & iinst_cwd %in% biginstances) %>% 
    #   dplyr::select(time, cwd, return_period, ndvi, ndvi_anom, zscore) %>% 
    #   mutate(thresh_quantile = prob)
    # 
    # # xxx try
    # x <- df$cwd[[9]]
    # p_exceedance <- 1 - pevd(x, loc = location, scale = scale, type = "Gumbel")  # doesn't make sense!
    # p_exceedance <- exp(-exp(-(x - location)/scale))
    
    df_annmax <- df_annmax %>% 
      dplyr::select(time, cwd, ndvi_anom, p_exceedance, return_period)
    
  } else {
    df_annmax <- tibble(time = NA, cwd = NA, ndvi_anom = NA, p_exceedance = NA, return_period = NA)
  }
  

  return(df_annmax)

}