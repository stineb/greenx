#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

args <- c(5000, 7200)

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(magrittr)
library(tidync)
library(rbeni)

##------------------------------------------------------------------------
## Extract point data and construct separate nested time series data frame
## for each longitde slice
##------------------------------------------------------------------------
dir <- "~/data/modis_ndvi_MOD13C2_006/netcdf/"
fileprefix <- "scrubbed.MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI."
nclist <- paste0(dir, list.files(dir, pattern = paste0(fileprefix, ".*.nc"), recursive = TRUE))
outdir <- "~/data/modis_ndvi_MOD13C2_006/data_tidy/"
varnam <- "MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI"
lonnam <- "lon"
latnam <- "lat"
timenam <- "time"
timedimnam <- "time"

# not necessary anymore after files have been combined into annual files 
fgetdate_modis <- function(filnam){
  filnam <- basename(filnam)
  year <- stringr::str_sub(filnam, 48, 51)
  moy <- stringr::str_sub(filnam, 52, 53)
  date <- lubridate::ymd(paste0(year, "-", moy, "-01"))
  return(date)
}

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nlon <- 7200
nrows_chunk <- ceiling(nlon/nchunk)
ilat <- seq(1:nlon)
irow_chunk <- split(ilat, ceiling(seq_along(ilat)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
nclist_to_df(
  nclist, 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = irow_chunk[[as.integer(args[1])]],
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = timenam, 
	timedimnam = timedimnam, 
	ncores = 1, #"all", 
  fgetdate = fgetdate_modis
	)
