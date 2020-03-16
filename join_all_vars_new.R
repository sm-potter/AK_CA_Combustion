library(tidyverse)
library(sf)

#create a function to join all variables
join_all_modis <- function(cna, dob, spatial, fwi){
  
  cna <- cna %>% dplyr::rename(ID1 = id1, ID2 = id2) %>% dplyr::select(-ID1)
  cna <- cna %>% dplyr::rename_all(function(x) paste0('CNA_', x)) %>% dplyr::rename(ID2 = CNA_ID2)
  dob <- dob %>% dplyr::select(-burn_yr, -Date, -ID1)
  spatial <- spatial %>% dplyr::select(-'system:index', -.geo, -burn_yr, -ID1, -key) %>% dplyr::rename(Tree.cover = tree_canopy_cover)
  fwi <- fwi %>% dplyr::select(-Burn_Date, -MERRA2_snowDepth)
  fwi <- fwi %>% dplyr::rename(Wind.speed = MERRA2_wdSpd, Temperature = MERRA2_t, Relative.humidity = MERRA2_rh, 
                        DC = MERRA2_DC, ISI = MERRA2_ISI, BUI = MERRA2_BUI, DMC = MERRA2_DMC,
                        DSR = MERRA2_DSR, FFMC = MERRA2_FFMC, FWI = MERRA2_FWI)
  
  joined <- plyr::join_all(list(spatial, dob, cna, fwi), by = 'ID2', type = 'left')
  
  return(joined)

}


#----this is how landsat all pixels joins into modis pixels, by ID2
# land <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp") %>% drop_na()
# mod <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years.shp") %>% drop_na()
# 
# join <- left_join(land, mod, by = 'ID2')

#this is how landsat cna joins into modis cna, here land id1 == land2 land_ID1, in land2 ID2 is the modis pixels
# land <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/land_final.csv") %>% drop_na() %>% dplyr::rename(ID1 = id2)
# land2 <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp") %>% drop_na() %>% dplyr::select(-ID1) %>% dplyr::rename(ID1 = land_ID1)
# 
# #combine land and land2, now by ID1
# land <- left_join(land2, land, by = 'ID1') 
# 
# mod <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/mod_final.csv") %>% drop_na() %>% dplyr::select(-id1) %>% dplyr::rename(ID2 = id2)
# 
# #join the land to modis, where ID2 is modis ids
# join <- left_join(mod, land, by = 'ID2')


#----------------------join all variables for modis native

cna <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/mod_cna.csv")
dob <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/DOB.csv")
spatial <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/modis_native.csv")
fwi <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/FWI.csv")

mod_native <-  join_all_modis(cna, dob, spatial, fwi) 
mod_native <- mod_native %>% dplyr::rename(CNA_PPT_5_8 = CNA_PPT_sm)
mod_native <- mod_native %>% dplyr::rename(CNA_Tmax_5_8 = CNA_Tmax_sm)
mod_native <- mod_native %>% dplyr::rename(stand.age = stand_age)

out = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors"
dir.create(out, recursive = T)

write_csv(mod_native, file.path(out, 'all_modis_predictors.csv'))

#----------------------join all variables for landsat native (which is how models were trained)
#file which specifies which landsat ids go to which modis ids
land_orig <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp")
land_orig <- as_tibble(data.frame(land_orig)) %>% dplyr::select(ID2, land_ID1)

#join each to land_orig
cna <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/land_cna.csv")
cna <- cna %>% dplyr::rename(ID2 = id2)
cna <- cna %>% dplyr::rename_all(function(x) paste0('CNA_', x)) %>% dplyr::rename(ID2 = CNA_ID2)

dob <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/DOB.csv") %>% dplyr::select(-burn_yr, -Date, -ID1)

#read in vi's from landsat resamples, land_spatial and mod_spatial join by ID1, land_ID1 for land_spatial is the unique landsat ID's
land_spatial <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/landsat_native_landsat.csv") %>% dplyr::select(-'system:index', -.geo, -burn_yr, -ID1, -key)
mod_spatial <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/landsat_native_rest.csv") %>% dplyr::select(-'system:index', -.geo, -burn_yr, -ID1, -key)

#join the 
spatial <- left_join(land_spatial, mod_spatial, by = 'ID2') %>% dplyr::rename(Tree.cover = tree_canopy_cover) %>% drop_na()

#join spatial to CNA, here ID2 in cna is land_ID1 in spatial
cna <- cna %>% dplyr::rename(land_ID1 = ID2)
spatial <- left_join(spatial, cna, by = 'land_ID1')

#modify somw fwi names
fwi <- fwi %>% dplyr::select(-Burn_Date, -MERRA2_snowDepth)
fwi <- fwi %>% dplyr::rename(Wind.speed = MERRA2_wdSpd, Temperature = MERRA2_t, Relative.humidity = MERRA2_rh, 
                      DC = MERRA2_DC, ISI = MERRA2_ISI, BUI = MERRA2_BUI, DMC = MERRA2_DMC,
                      DSR = MERRA2_DSR, FFMC = MERRA2_FFMC, FWI = MERRA2_FWI)


out = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
dir.create(out, recursive = T)

#join it all together and save
land_to_mod <- plyr::join_all(list(spatial, dob, fwi), by = 'ID2', type = 'left')
land_to_mod <- land_to_mod %>% dplyr::rename(CNA_PPT_5_8 = CNA_PPT_sm)
land_to_mod <- land_to_mod %>% dplyr::rename(CNA_Tmax_5_8 = CNA_Tmax_sm)
land_to_mod <- land_to_mod %>% dplyr::rename(stand.age = stand_age)

write_csv(land_to_mod, file.path(out, 'all_landsat_native_predictors.csv'))

# out = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
# dir.create(out, recursive = T)
# 
# write_csv(land_to_mod, file.path(out, 'all_landsat_resampled_predictors.csv'))

#----------------------join all variables for landsat resampled

out = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
dir.create(out, recursive = T)

#read in the landsat predictors which were resampled to modis resolution
land_spatial <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/landsat_resample.csv") %>% dplyr::select(-'system:index', -.geo, -burn_yr, -ID1) %>%
  dplyr::rename(Tree.cover = tree_canopy_cover)

#read in all the modis native variables to be joined to the land spatial variabled
mod_spatial <-  read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/modis_native.csv") %>% dplyr::select(-'system:index', -.geo, -key, -NDII,
                                                                                                                                                -NDVI, -TWI, -aspect, -brightness, -dNBR, -elevation,
                                                                                                                                                -greenness, -rbr, -rdnbr, -slope, -tree_canopy_cover, -wetness)
                                                                                                                                                                                                                                                                        

#join in land spatial to mod spatial by ID2
spatial <- left_join(mod_spatial, land_spatial, by = "ID2") %>% dplyr::select(-ID1)


#join each to land_orig
cna <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/mod_cna.csv")
cna <- cna %>% dplyr::rename(ID1 = id1, ID2 = id2) %>% dplyr::select(-ID1)
cna <- cna %>% dplyr::rename_all(function(x) paste0('CNA_', x)) %>% dplyr::rename(ID2 = CNA_ID2)

dob <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/DOB.csv") %>% dplyr::select(-burn_yr, -Date, -ID1)
fwi <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/FWI.csv")
fwi <- fwi %>% dplyr::select(-Burn_Date, -MERRA2_snowDepth)
fwi <- fwi %>% dplyr::rename(Wind.speed = MERRA2_wdSpd, Temperature = MERRA2_t, Relative.humidity = MERRA2_rh,
                      DC = MERRA2_DC, ISI = MERRA2_ISI, BUI = MERRA2_BUI, DMC = MERRA2_DMC,
                      DSR = MERRA2_DSR, FFMC = MERRA2_FFMC, FWI = MERRA2_FWI)

dob_fwi <-  plyr::join_all(list(cna, dob, fwi), by = 'ID2', type = 'left')

land_native <- left_join(spatial, dob_fwi)

colnames(land_native)[duplicated(colnames(land_native))]
land_native <- land_native %>% dplyr::rename(CNA_PPT_5_8 = CNA_PPT_sm)
land_native <- land_native %>% dplyr::rename(CNA_Tmax_5_8 = CNA_Tmax_sm)
land_native <- land_native %>% dplyr::rename(stand.age = stand_age)

out = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors"
dir.create(out, recursive = T)

write_csv(land_native, file.path(out, 'all_landsat_resampled_predictors.csv'))
