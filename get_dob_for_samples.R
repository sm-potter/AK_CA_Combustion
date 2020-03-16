library(raster)
library(tidyverse)
library(lubridate)
library(doSNOW)
library(parallel)
library(sf)


#------------------------get the dob for each pixel for modis pixels- to be used as a predictor and get the FWI data
out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors"
dir.create(out, recursive = T)
#pathway to samples
samples <- list.files("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/tifs", pattern = ".tif$", full.names = F)

#pathway to original data
path <- "/mnt/data1/boreal/scooperdock/Combustion/Final/translate"

#pathway to individual shapefiles
shape_path <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes"

cores <- detectCores() - 1
cl <-makeCluster(cores)
registerDoSNOW(cl)

all_df <- c()
for (f in samples){
# foreach (f = samples, .packages=c("raster", 'tidyverse', 'sf')) %dopar% {
    
  #read in shapefile
  df <- read_sf(file.path(shape_path, gsub('.tif', '.shp', f)))
  
  #read in original data. band2 for DOB
  orig <- raster(file.path(path, f), band = 2)
  orig <- st_as_sf(rasterToPolygons(orig, na.rm=T))
  orig <- st_centroid(orig)
  orig <- st_transform(orig, '+proj=longlat +datum=WGS84 +no_defs')
  names(orig) <- c('DOB_lst', 'geometry')
  
  #spatial join
  ex <- st_join(df, orig, join = st_intersects)
  ex <- as_tibble(data.frame(ex)) %>% dplyr::select(ID1, burn_yr, ID2, DOB_lst)
  
  all_df[[length(all_df) + 1]] <- ex
}

final <- bind_rows(all_df)

#convert DOB and burn_yr to date
final <-  final %>% mutate(Date = as.Date(strptime(paste(burn_yr, DOB_lst), "%Y %j"))) 
  
#merge to shapefile and write out
shape <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years.shp")

t <- left_join(shape, final %>% dplyr::select(ID2, DOB_lst, Date), on = 'ID2')
write_sf(t, file.path(out, 'DOB.shp'))


t <- as_tibble(data.frame(t)) %>% dplyr::select(-geometry)
write_csv(t, file.path(out, 'DOB.csv'))

#------------------------get the dob for each pixel for landsat - to be used as a predictor and get the FWI data
out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
dir.create(out, recursive = T)

#read in the landsat pixels - ID2 here is from MODIS pixels burn year and running row id
shape <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp") 

#read in the modis DOB
mod_dob <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/DOB.csv") %>% dplyr::select(ID2, DOB_lst)
mod_dob <- as_tibble(data.frame(mod_dob)) 
#attach the DOB from MODIS to all landsat which overlay - join on ID2
land_dob <- left_join(as_tibble(shape), as_tibble(mod_dob), by = 'ID2') 

write_csv(land_dob, file.path(out, 'DOB.csv'))

