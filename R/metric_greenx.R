metric_greenx <- function(df, lat){
  
  print(lat)
  
  cont <- TRUE
  if (sum(!is.na(df$return_period))<3) cont <- FALSE
  if (sum(!is.na(df$ndvi_anom))<3) cont <- FALSE

  if (cont){
    out <- df %>% 
      mutate(log_rp = log(return_period)) %>% 
      analyse_modobs2("log_rp", "ndvi_anom")
  } else {
    out <- NA
  }
  return(out)
}