calc_rp_cwd <- function(df, mod, prob = 0.05){
  
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
    
    ## determine the return periods, given cwd (=CWD) and mod (=the fitted extreme value distribution)
    location <- mod$results$par[1]
    scale <- mod$results$par[2]
    shape <- 0  # for Gumbel  mod$results$par[3]
    
    ## retain data of month where largest negative NDVI anomaly happens
    mdf <- df %>% 
      mutate(year = lubridate::year(time), month = lubridate::month(time)) %>% 
      group_by(year, month) %>% 
      summarise(cwd = max(cwd, na.rm = TRUE), ndvi_anom = mean(ndvi_anom, na.rm = TRUE))
    
    df_ndvix <- mdf %>% 
      ungroup() %>% 
      dplyr::filter(ndvi_anom == min(ndvi_anom))
    
    if (!is.infinite(df_ndvix$cwd)){
      
      df_ndvix <- df_ndvix %>% 
        mutate(p_exceedance = sum(mdf$cwd >= df_ndvix$cwd) / (nrow(mdf) + 1)) %>% 
        mutate(return_period = 1/p_exceedance)
      
    } else {
      df_ndvix <- df_ndvix %>% 
        mutate(p_exceedance = NA) %>% 
        mutate(return_period = NA)
      
    }

    df_ndvix <- df_ndvix %>% 
      dplyr::select(year, month, cwd, ndvi_anom, p_exceedance, return_period)
    
  } else {
    df_ndvix <- tibble(year = NA, month = NA, cwd = NA, ndvi_anom = NA, p_exceedance = NA, return_period = NA)
  }
  

  return(df_ndvix)

}