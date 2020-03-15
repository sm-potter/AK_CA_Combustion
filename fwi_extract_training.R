# rm(list=ls())
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(lubridate)
library(doSNOW)
library(parallel)
library(doParallel)
library(plyr)

# cores <- detectCores()
# cl <-makeCluster(cores)
# cl <- makeCluster(5, type = "SOCK")
# 
# registerDoSNOW(cl)
# rm(list=ls())

rasterOptions(tmpdir = "/mnt/data1/boreal/spotter/temp_dir")

#---------------------------------extract fwi information for the modis samples
pts <- read_sf("/mnt/data1/boreal/spotter/combustion/final_files/raw/for_extraction.shp")

pts <- pts %>% filter(is.na(Date) == FALSE)

#inputs are the pts to extract, the raster_path, variables for extraction, 
#and time period through which to aggregate over, t1 and t2 both at 0 means only the day of ignition
extract_fwi <- function(pts, raster_path, variables, t1, t2){
  
  #a final dataframe to return the results
  final_df <- list()
  
  #get the unique burn_dates
  all_dates <- unique(pts$Date)
  
  #loop through the unique dates
  for (bd in all_dates){
    
    print (as.character(lubridate::as_date(bd)))
    
    for (var in variables){
      
      #an if else statement for the variable prefix
      prefix <- ifelse(var %in% c( 'MERRA2_t', 'MERRA2_rh', 'MERRA2_wdSpd', 'MERRA2_snowDepth', 'VPD'), "Wx.MERRA2.Daily.Default.", "FWI.MERRA2.Daily.Default.") 
      
      #an empty raster stack to get the means of the time steps
      for_mean <- stack()
      
      for (date in (seq(bd - t1, bd+t2, 1))){
        
        date <- as.character(lubridate::as_date(date))
        
        year <- substr(date, 1, 4)
        month <- substr(date, 6, 7)
        day <- substr(date, 9, 10)
        
        # #the first time step raster 
        in_raster <- raster(file.path(raster_path,var, year, paste0(prefix, year, month, sprintf("%02d", as.numeric(day)), '.tif')))
        
        #append each to "for_mean"
        for_mean <- stack(for_mean, in_raster)
      }
      #get the mean
      mean_ras <- stackApply(for_mean, indices = rep(1, nlayers(for_mean)), fun = "mean", na.rm = T)
      
      sub_frame <- pts %>% dplyr::filter(Date == bd) 
      
      #extract the pts
      extract <- as_tibble(raster::extract(mean_ras, sub_frame, df = T, method = 'simple')) 
      
      #rename the value with appropriate variable
      names(extract) <- c('id', 'value')
      
      #make sure only one id
      extract$Variable <- var
      
      #rename with appropriate id from sub_frame
      extract$id <- sub_frame$id
      extract$Date <- as.character(lubridate::as_date(bd))
      extract <- extract %>% distinct(id, .keep_all = TRUE)
      
      extract <- extract %>% dplyr::select(id, Variable, value)
      
      #append the file to "final_df"
      final_df[[length(final_df) + 1]] <- extract
    }
  }
  return (spread(bind_rows(final_df), key = Variable, value = value))
}

#variables to extract
variables <- variables <- c('MERRA2_DC','MERRA2_DMC','MERRA2_FFMC','MERRA2_ISI','MERRA2_BUI','MERRA2_FWI','MERRA2_DSR',
                            'MERRA2_t', 'MERRA2_rh', 'MERRA2_wdSpd', 'MERRA2_snowDepth', 'VPD')

all_extracted <- extract_fwi(pts, "/mnt/data1/boreal/raw/GFWED/v2.5", variables, 0, 0)

#join the FWI's back to the original csv_file and save out
out <- "/mnt/data1/boreal/spotter/combustion/final_files/raw"
dir.create(out, recursive = T)

in_csv <- read_csv(file.path(out, 'for_extraction.csv')) %>% dplyr::select(id)

in_csv <- left_join(in_csv, all_extracted , by = 'id')

write_csv(in_csv, file.path(out, 'FWI_training.csv'))

removeTmpFiles(h=24)
