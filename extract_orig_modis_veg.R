library(raster)
library(tidyverse)
library(sf)


ex <- function(pts, out){

  pts <- read_sf(pts)
  dir.create(out, recursive = T)
    
  jp <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_JP.tif")
  bs  <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_BS.tif")
  dec <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_DB.tif")
  grsh <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_GrassShrub.tif")
  nv <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_NonVeg.tif")
  ocon <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_OtherCon.tif")
  ws <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_WS.tif")
  
  
  #veg variables
  jp_ex <-  as_tibble(raster::extract(jp, pts, df = T), na.rm = T)
  names(jp_ex) <- c('ID', 'JP')
  jp_ex <- jp_ex %>% dplyr::select(-ID)
  jp_ex$ID2 <- pts$ID2
  
  bs_ex <-  as_tibble(raster::extract(bs, pts, df = T), na.rm = T)
  names(bs_ex) <- c('ID', 'BS')
  bs_ex <- bs_ex %>% dplyr::select(-ID)
  bs_ex$ID2 <- pts$ID2
  
  dec_ex <-  as_tibble(raster::extract(dec, pts, df = T), na.rm = T)
  names(dec_ex) <- c('ID', 'DEC')
  dec_ex <- dec_ex %>% dplyr::select(-ID)
  dec_ex$ID2 <- pts$ID2
  
  grsh_ex <-  as_tibble(raster::extract(grsh, pts, df = T), na.rm = T)
  names(grsh_ex) <- c('ID', 'GRSH')
  grsh_ex <- grsh_ex %>% dplyr::select(-ID)
  grsh_ex$ID2 <- pts$ID2
  
  nv_ex <-  as_tibble(raster::extract(nv, pts, df = T), na.rm = T)
  names(nv_ex) <- c('ID', 'NV')
  nv_ex <- nv_ex %>% dplyr::select(-ID)
  nv_ex$ID2 <- pts$ID2
  
  ocon_ex <-  as_tibble(raster::extract(ocon, pts, df = T), na.rm = T)
  names(ocon_ex) <- c('ID', 'OCON')
  ocon_ex <- ocon_ex %>% dplyr::select(-ID)
  ocon_ex$ID2 <- pts$ID2
  
  ws_ex <-  as_tibble(raster::extract(ws, pts, df = T), na.rm = T)
  names(ws_ex) <- c('ID', 'WS')
  ws_ex <- ws_ex %>% dplyr::select(-ID)
  ws_ex$ID2 <- pts$ID2
  
  all_join <- plyr::join_all(list(jp_ex, bs_ex, dec_ex, grsh_ex, nv_ex, ocon_ex, ws_ex), by = 'ID2')
  
  # all_join <- as_tibble(data.frame(all_join)) %>% dplyr::select(-geometry)
  all_join <- all_join %>% mutate_all(funs(replace(., . < 0, 0)))
                                           
  write_csv(all_join, file.path(out, 'veg.csv'))
}

ex(pts <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years_pts.shp",
   out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels")

ex(pts <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/pure_fire_mixed_veg/modis_pixels/all_years_pts.shp",
   out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/pure_fire_mixed_veg/modis_pixels")

ex(pts <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_mixed_veg/modis_pixels/all_years_pts.shp",
   out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_mixed_veg/modis_pixels")

ex(pts <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_pure_veg/modis_pixels/all_years_pts.shp",
   out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_pure_veg/modis_pixels")


ex <- function(pts, out){
  
  pts <- read_sf(pts)
  dir.create(out, recursive = T)
  
  jp <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_JP.tif")
  bs  <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_BS.tif")
  dec <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_DB.tif")
  grsh <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_GrassShrub.tif")
  nv <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_NonVeg.tif")
  ocon <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_OtherCon.tif")
  ws <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_WS.tif")
  
  
  #veg variables
  jp_ex <-  as_tibble(raster::extract(jp, pts, df = T), na.rm = T)
  names(jp_ex) <- c('ID', 'JP')
  jp_ex <- jp_ex %>% dplyr::select(-ID)
  jp_ex$ID1 <- pts$ID1
  
  bs_ex <-  as_tibble(raster::extract(bs, pts, df = T), na.rm = T)
  names(bs_ex) <- c('ID', 'BS')
  bs_ex <- bs_ex %>% dplyr::select(-ID)
  bs_ex$ID1 <- pts$ID1
  
  dec_ex <-  as_tibble(raster::extract(dec, pts, df = T), na.rm = T)
  names(dec_ex) <- c('ID', 'DEC')
  dec_ex <- dec_ex %>% dplyr::select(-ID)
  dec_ex$ID1 <- pts$ID1
  
  grsh_ex <-  as_tibble(raster::extract(grsh, pts, df = T), na.rm = T)
  names(grsh_ex) <- c('ID', 'GRSH')
  grsh_ex <- grsh_ex %>% dplyr::select(-ID)
  grsh_ex$ID1 <- pts$ID1
  
  nv_ex <-  as_tibble(raster::extract(nv, pts, df = T), na.rm = T)
  names(nv_ex) <- c('ID', 'NV')
  nv_ex <- nv_ex %>% dplyr::select(-ID)
  nv_ex$ID1 <- pts$ID1
  
  ocon_ex <-  as_tibble(raster::extract(ocon, pts, df = T), na.rm = T)
  names(ocon_ex) <- c('ID', 'OCON')
  ocon_ex <- ocon_ex %>% dplyr::select(-ID)
  ocon_ex$ID1 <- pts$ID1
  
  ws_ex <-  as_tibble(raster::extract(ws, pts, df = T), na.rm = T)
  names(ws_ex) <- c('ID', 'WS')
  ws_ex <- ws_ex %>% dplyr::select(-ID)
  ws_ex$ID1 <- pts$ID1
  
  all_join <- plyr::join_all(list(jp_ex, bs_ex, dec_ex, grsh_ex, nv_ex, ocon_ex, ws_ex), by = 'ID1')
  
  # all_join <- as_tibble(data.frame(all_join)) %>% dplyr::select(-geometry)
  all_join <- all_join %>% mutate_all(funs(replace(., . < 0, 0)))
  
  write_csv(all_join, file.path(out, 'veg.csv'))
}

ex(pts <- "/mnt/data1/boreal/spotter/combustion/final_files/scaling/all_training/modis_pixels/all_years_pts.shp",
   out <- "/mnt/data1/boreal/spotter/combustion/final_files/scaling/all_training/modis_pixels")


