#--------read in all sols burn area products, sample 1000 pixels from 2004, 2006, 2012, 2014, 2015 
#--------maask the correct quality flags and convert to polygon to intersect with landsat pixels in EE

library(raster)
library(sf)
library(tidyverse)
library(doSNOW)
library(parallel)

#set temp dir
temp_dir <- '/mnt/data1/boreal/spotter/temp_dir'
dir.create(temp_dir, recursive = T)
#rasterOptions()
# dirname(rasterTmpFile())
rasterOptions(tmpdir=temp_dir)

# 
cores <- detectCores() - 1
cl <-makeCluster(cores)

#-------get the correct flags
years <- c(2004, 2006, 2012, 2014, 2015)
path <- "/mnt/data1//boreal/scooperdock/Combustion/Final/translate"


out_path <- "/mnt/data1//boreal/spotter/combustion/burned_area/scaling/modis_pixels/tifs"
dir.create(out_path, recursive = T)

registerDoSNOW(cl)

for (year in years){
# foreach (year = years, .packages=c("raster", 'tidyverse', 'sf')) %dopar% {
  print(year)
  suffix <- paste0('translated_', as.character(year), '.tif')
  ras <- raster(file.path(path, suffix), band = 3)
  ras1 <- raster(file.path(path, suffix), band = 1)
  ras1[ras1 < 1] = NA
  ras <- mask(ras, ras1)


  ras[ras > 8] = NA

  ras[ras %in% c(1,2,3,5,6,7)] <- NA


  writeRaster(ras, file.path(out_path, suffix), overwrite = T)
}

stopCluster(cl)

#-------convert to shapefile

path <- "/mnt/data1//boreal/spotter/combustion/burned_area/scaling/modis_pixels/tifs"

above = read_sf("/mnt/data1//boreal/spotter/Features/ABoVE_reference_grid_v2_1527/ABoVE_reference_grid_v2_1527/data/ABoVE_Study_Domain/ABoVE_Study_Domain/ABoVE_Study_Domain.shp")
above = st_transform(above, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

out_path <- "/mnt/data1//boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes"
dir.create(out_path, recursive = T)

# years <- c(2004, 2014, 2015)
years <- c(2004, 2006, 2012, 2014, 2015)

#all shapefiles
ak <- read_sf("/mnt/data1//boreal/spotter/auxillary/Features/Alaska/Boundaries/alaska.shp")
nwt <- read_sf("/mnt/data1//boreal/spotter/auxillary/Features/Canada/Provinces/NWT_boundary.shp")
sk <- read_sf("/mnt/data1//boreal/spotter/auxillary/Features/Canada/Provinces/SK_boundary.shp")

cores <- detectCores() - 1
cl <-makeCluster(cores)

registerDoSNOW(cl)

# for (year in years){
foreach (year = years, .packages=c("raster", 'tidyverse', 'sf')) %dopar% {
  print(year)
  # if (year == 2004){
    
    suffix <- paste0('translated_', as.character(year), '.tif')
    suffix2 <- paste0('translated_', as.character(year), '.shp')
  
    ras <- raster(file.path(path, suffix))
    ras <- rasterToPolygons(ras, na.rm = T)
    ras <- st_as_sf(ras)
    ras <- st_transform(ras, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    # ak <-  st_transform(ak, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
    
    #need to check the line below and make sure we stay in the ABoVE zone
    ras <- st_join(ras, above, join = st_intersects)
    ras <- ras %>% filter(Region %in% c('Core Region', 'Extended Region'))
    
    ras <- sample_n(ras, 200, replace = FALSE)
    ras <- ras %>% mutate(ID1 = row_number(), burn_year = year)
    ras <- ras %>% mutate(ID2 = paste0(burn_year, '_', ID1))
    
    write_sf(ras,  file.path(out_path, suffix2))
  
    print(year)

}

stopCluster(cl)


#-------combine and get latitude and longitudes in wgs84

path <-"/mnt/data1//boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes"


out_path <- "/mnt/data1//boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes"
dir.create(out_path, recursive = T)

all_df <- c()

for (year in years){

  suffix <- paste0('translated_', as.character(year), '.shp')

  shp <- read_sf(file.path(path, suffix))
  shp <- st_transform(shp, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

  shp <- st_centroid(shp)
  #get latitude and longitude
  coords <- data.frame(st_coordinates(shp))

  all_x <- coords$X
  all_y <- coords$Y

  shp <- shp %>% mutate(lat = all_y, lon = all_x)
  to_drop <- names(shp)[1]
  shp <- as_tibble(data.frame(shp)) %>% dplyr::select(-geometry, -to_drop)
  all_df[[length(all_df) + 1]] <- shp
}

all_df <- bind_rows(all_df) %>% mutate(ID1 = row_number()) %>% dplyr::select(ID1, burn_yr, ID2, lat, lon)

out_path <- "/mnt/data1//boreal/spotter/combustion/burned_area/scaling/climateNA/input"
dir.create(out_path, recursive = T)

write_csv(all_df, file.path(out_path, 'modis_climatena_all.csv'))

all_df <- all_df %>% dplyr::select(ID1, ID2, lat, lon)
#
write_csv(all_df, file.path(out_path, 'modis_climatena_input.csv'))


