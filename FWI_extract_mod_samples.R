# rm(list=ls())
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(lubridate)
# rm(list=ls())

#---------------------------------extract fwi information for the modis samples
pts <-  read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/DOB.shp")

# pts <- pts %>% mutate(Date = as.character(lubridate::as_date(Date)))


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
      
      sub_frame <- st_as_sf(pts) %>% dplyr::filter(Date == bd) 
      
      #extract the pts
      extract <- as_tibble(raster::extract(mean_ras, sub_frame, df = T, method = 'simple')) 
      names(extract) <- c('ID', 'value')
      extract <- extract %>% distinct(id, .keep_all = TRUE)
      extract$Variable <- var
      extract$ID2 <- sub_frame$ID2
      extract$Burn_Date <- as.character(lubridate::as_date(bd))

      extract <- extract %>% dplyr::select(ID2, Variable, Burn_Date, value)
     
      #append the file to "final_df"
      final_df[[length(final_df) + 1]] <- extract
      
    }
  }
  return (spread(bind_rows(final_df), key = Variable, value = value))
}

variables <- c('MERRA2_DC','MERRA2_DMC','MERRA2_FFMC','MERRA2_ISI','MERRA2_BUI','MERRA2_FWI','MERRA2_DSR',
               'MERRA2_t', 'MERRA2_rh', 'MERRA2_wdSpd', 'MERRA2_snowDepth', 'VPD')

all_extracted <- extract_fwi(as(pts, 'Spatial'), "/mnt/data1/boreal/raw/GFWED/v2.5", variables, 0, 0)


# df <- bind_rows(final_df)
# df <-  df %>% group_by(ID2, Burn_Date) %>% mutate(idn = row_number()) %>% spread(Variable, value) %>% summarize_all(mean, na.rm = T) %>% drop_na() %>% ungroup()
# df <- df  %>% dplyr::select(-idn)


out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors"
dir.create(out, recursive = T)

write_csv(all_extracted, file.path(out, 'FWI.csv'))
# write_csv(df, file.path(out, 'FWI.csv'))

#------------------------------------get landsat FWI by merging into modis
# out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
# dir.create(out, recursive = T)
# 
# mod_fwi <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/FWI.csv")
# 
# #read in the landsat pixels - ID2 here is from MODIS pixels burn year and running row id
# shape <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp") 
# 
# shape <- as_tibble(data.frame(shape)) %>% dplyr::select(-geometry)
# 
# #attach the DOB from MODIS to all landsat which overlay - join on ID2
# land_fwi <- left_join(as_tibble(shape), as_tibble(mod_fwi), by = 'ID2') 
# 
# write_csv(land_fwi, file.path(out, 'FWI.csv'))

# df2 <-  read_csv('/Users/spotter/Documents/combustion/intro_files/text_files/Combustion_SynthesisData_05042018_XJW.csv')
# df2 <- df2 %>% dplyr::select(-DMC.y, -BUI.y, -Relative.humidity, -Wind.speed, -FFMC.y, -FWI.y, -DC.y, -ISI.y, -DSR, -Temperature)
# names(df) <- c("id", "Burn_Date", 'BUI.y', 'DC.y', 'DMC.y', 'DSR', 'FFMC.y', 'FWI.y', 'ISI.y', 'Relative.humidity', 'snowDepth', 'Temperature', 'Wind.speed', 'VPD')
# 
# df <- df %>% dplyr::select(-snowDepth, -Burn_Date)
# 
# df <- left_join(df, df2, on = id)
# 
# df2 <-  read_csv('/Users/spotter/Documents/combustion/intro_files/text_files/Combustion_SynthesisData_05042018_XJW.csv')
# 
# df2 <- df2 %>% filter(!id %in% unique(df$id))
# df2 <- df2 %>% mutate(VPD = -999) %>% na_if(-999)
# df <- df[names(df2)]
# 
# df <- bind_rows(df, df2)
# 
# df2 <-  read_csv('/Users/spotter/Documents/combustion/intro_files/text_files/Combustion_SynthesisData_05042018_XJW.csv')
# 
# # df <- df %>% ungroup() %>% mutate(id = as.factor(id, levels = list(unique(df2$id))))
# write_csv(df, '/Users/spotter/Documents/combustion/intro_files/text_files/Combustion_Data_051019_DOB_SMP.csv')


