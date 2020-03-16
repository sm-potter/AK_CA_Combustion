library(raster)
library(sf)
library(tidyverse)
library(doSNOW)
library(parallel)

#-----------------------determine which landsat scenes intersect which ABoVE scenes
#-----------------------use these to download landsat 8 necessary scenes and
#-----------------------to determine which ABoVE scenes to intersect with jwang data

#landsat descending pass
land <- read_sf("/mnt/data1/boreal/spotter/Features/WRS2_descending_0/WRS2_descending.shp")

#modis pixels
modis <- list.files("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes", pattern = ".shp$", full.names = T)

above = read_sf("/mnt/data1/boreal/spotter/Features/ABoVE_reference_grid_v2_1527/ABoVE_reference_grid_v2_1527/data/ABoVE_240m_30m_5m_grid_tiles/ABoVE_240m_30m_5m_grid_tiles/ABoVE_240m_30m_5m_grid_tiles.shp")
above = above %>% select(bh, bv, geometry)
above <- st_transform(above, '+proj=longlat +datum=WGS84 +no_defs')



final <- c()
# 
# cores <- detectCores() - 1
# cl <-makeCluster(cores)
# registerDoSNOW(cl)

#loop through and intersect with landsat
for (f in modis){
# foreach (f = modis, .packages=c("raster", 'tidyverse', 'sf')) %dopar% {
  
  year <- substr(f, 89, 92)
  df <- read_sf(f)
  df <- st_transform(df, '+proj=longlat +datum=WGS84 +no_defs')
  #intersect landsat and modis
  int <- st_join(df, above, join = st_intersects) %>% select(bh, bv)
  int <- int %>% mutate(Year = year)
  
  #intersect landsat with ABOVE
  # int = st_join(int, above, join = st_intersects)
  # 
  int = int %>% filter(bh != 0 & bv != 0)

  int <- as_tibble(data.frame(int)) %>% select(-geometry)
  int = int %>% mutate(burn_yr = year)
  final[[length(final) + 1]] <- int
  
}

final <- bind_rows(final)
# 
final2 <- unique(final[c("bh", "bv", "burn_yr")])
# stopCluster(cl)
# 
write_csv(final2, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/needed_above_scenes.csv")
# 
