calc_anom_ndvi_byilon <- function(ilon){

  source("R/calc_anom_ndvi.R")
  source("R/get_iinst.R")
  
  
  ## load NDVI data
  load(paste0("~/data/modis_ndvi_MOD13C2_006/data_tidy/scrubbed.MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI._ilon_", ilon, ".RData"))
  df_ndvi <- df %>% 
    mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3))
  rm("df")
  
  ## load CWDX data
  load(paste0("~/mct/data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
  df <- df %>% 
    mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3))
  
  if ((df$lon %>% unique()) != (df_ndvi$lon %>% unique())){
    rlang::abort("calc_anom_ndvi_byilon(): longitude values not identical")
  }
    
  tmp <- df %>% 
    
    ## extract to get nice format
    dplyr::select(-lon) %>% 
    inner_join(df_ndvi, by = c("lat")) %>% 
    mutate(mct = purrr::map(out_mct, "mct")) %>% 
    mutate(df_mct = purrr::map(mct, "df")) %>% 
    mutate(mod = purrr::map(out_mct, "mod")) %>% 
    dplyr::select(-out_mct, -mct) %>% 
    mutate(notavl_mct = purrr::map_lgl(df_mct, ~is.null(.))) %>% 
    dplyr::filter(!notavl_mct) %>% 
    dplyr::select(-notavl_mct) %>% 
    mutate(df_mct = purrr::map(df_mct, ~dplyr::select(., time, cwd = deficit, iinst_cwd = iinst))) %>% 
    
    ## merge time series
    mutate(df_mct = purrr::map2(df_mct, data, ~full_join(.x, .y, by = "time"))) %>% 
    
    ## remove non-modis dates
    mutate(df_mct = purrr::map(df_mct, ~dplyr::filter(., !is.na(MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI)))) %>% 
    
    dplyr::select(-data) %>% 
    rename(df_tseries = df_mct) %>% 
    
    ## get EVI anomalies and extremes, and determine return periods of EVI anomalies
    mutate(df_tseries = purrr::map2(df_tseries, mod, ~calc_anom_ndvi(.x, .y)))
  
}