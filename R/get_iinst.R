#' Aligns data by events
#'
#' Uses a vectory specifying whether data falls into an event to reshape data, aligning by the onset of the event
#' 
#' @param df A data frame containing all data continuously along time. The data frame must contain one column of type logical
#' named \code{"isevent"}, specifying whether respective dates satisfy a user-defined condition that is used to define events. 
#' Events are determined by the function based on consecutive dates, where this condition is satisfied (minimum length for 
#' defining an event is given by \code{leng_threshold}).
#' @param leng_threshold A vector of character strings specifying which columns of \code{df} to re-arrange.

#' @return An aligned data frame
#' @export
#'
#' @examples df_alg <- align_events( df, before=30, after=300 )
#' 
get_iinst <- function(df, leng_threshold){

  ##--------------------------------------------------------
  ## Identify events ()
  ##--------------------------------------------------------
  if (!("isevent" %in% names(df))){
    rlang::abort("align_events(): Column named isevent is missing in data frame df.")
  }
  events <- get_consecutive( 
    df$isevent, 
    leng_threshold = leng_threshold, 
    do_merge       = FALSE
  )

  ##--------------------------------------------------------
  ## Re-arrange data, aligning by beginning of events
  ## Creates data frame where not all rows are retained from df
  ## and columns added for 'dday' (number of day relative to onset of event)
  ## and 'iinst' number of event to which row belongs.
  ##--------------------------------------------------------
  df <- df %>% 
    mutate(iinst = NA, instt = NA)

  if (nrow(events)>1){
    for ( iinst in 1:nrow(events) ){
      idx_start <- events$idx_start[iinst]
      idx_end <- events$idx_start[iinst] + events$len[iinst] -1
      df$iinst[idx_start:idx_end] <- iinst
      df$instt[idx_start:idx_end] <- 1:events$len[iinst]
    }

  }

  return( df )

}


get_consecutive <- function( dry, leng_threshold=5, anom=NULL, do_merge=FALSE ){
  ##------------------------------------
  ## Returns a dataframe that contains information about events (starting index and length) 
  ## of consecutive conditions (TRUE) in a boolean vector ('dry' - naming is a legacy).
  ##------------------------------------

  ## replace NAs with FALSE (no drought). This is needed because of NAs at head or tail
  dry[ which(is.na(dry)) ] <- FALSE

  ## identifies periods where 'dry' true for consecutive days of length>leng_threshold and 
  ## creates data frame holding each instance's info: start of drought by index in 'dry' and length (number of days thereafter)
  instances <- data.frame( idx_start=c(), len=c() )
  consecutive_dry <- rep( NA, length( dry ) )
  ndry  <- 0
  ninst <- 0
  for ( idx in 1:length( dry ) ){
    if (dry[idx]){ 
      ndry <- ndry + 1 
    } else {
      if (ndry>=leng_threshold) { 
        ## create instance
        ninst <- ninst + 1
        addrow <- data.frame( idx_start=idx-(ndry), len=ndry )
        instances <- rbind( instances, addrow )
      }
      ndry <- 0
    }
    consecutive_dry[idx] <- ndry
  }
  if (ndry>leng_threshold){
    ## create a last instance if the last dry period extends to the end of the time series
    ninst <- ninst + 1
    addrow <- data.frame( idx_start=idx-(ndry), len=ndry )
    instances <- rbind( instances, addrow )
  }


  if (nrow(instances)>0){

    ## Get cumulative deficit per instance (deficit w.r.t. 1, where 'anom' is a vector with values 0-1)
    if (!is.null(anom)){
      instances$deficit <- rep( NA, nrow(instances) )
      for ( idx in 1:nrow(instances) ){
        instances$deficit[idx] <- sum( anom[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ] )
      }
    }

    ## merge events interrupted by short non-drought periods
    ## if in-between non-drought period is shorter than both of the drought periods
    ## before and after non-drought period
    if (do_merge){
      
      print("dimensions of instances before merging short periods")
      print(dim(instances))

      ninst_save <- nrow( instances ) + 1
      ninst      <- nrow( instances )

      while (ninst < ninst_save){

        ninst_save <- nrow( instances )

        instances_merged <- data.frame( idx_start=c(), len=c() )

        idx <- 0
        while (idx<(nrow(instances)-1)){
          idx <- idx + 1

          len_betweendrought <- instances$idx_start[idx+1] - (instances$idx_start[idx] + instances$len[idx] + 1)
          
          if (len_betweendrought<instances$len[idx] && len_betweendrought<instances$len[idx+1]){
            addrow <- data.frame( idx_start=instances$idx_start[idx], len=(instances$idx_start[idx+1] + instances$len[idx+1]) - instances$idx_start[idx] )
            instances_merged <- rbind( instances_merged, addrow )
            idx <- idx + 1
          } else {
            instances_merged <- rbind( instances_merged, instances[idx,] )
            if (idx==(nrow(instances)-1)){
              instances_merged <- rbind( instances_merged, instances[idx+1,] )
            }
          }
        }

        instances <- instances_merged    

        ninst <- nrow( instances )

        print( "dimensions of instances after merging short periods" )
        print( dim( instances ) )

      }

    }

  }

  return( instances )
}


q33 <- function( vec, ... ){
  quantile( vec, 0.33, ...)
}

q66 <- function( vec, ... ){
  quantile( vec, 0.66, ...)
}


  

