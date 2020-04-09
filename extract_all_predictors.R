library(tidyverse)
library(sf)
library(rgdal)
library(parallel)
library(doSNOW)
library(plyr)
library(raster)

#this outputs has FWI and has stand age
#the missing is has FWI and no stand age

out <- "/mnt/data1/boreal/spotter/combustion/final_files/final_for_prediction/final_csv/all_data"
dir.create(out, recursive = T)

miss_out <- "/mnt/data1/boreal/spotter/combustion/final_files/missing_for_predictions/final_csv/all_data"
dir.create(miss_out, recursive = T)

#this code will extract all the covariates for all burned pixels as defined by sol, and then merge the CNA and FWI data from the csvs

######--------------------------set up all the pathways to predictors
stand.age <- "/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu_w_ak_lfdb"
dob <-  "/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/DOB"
bd <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/BD_30.tif")
silt <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/Silt_30.tif")
sand <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/Sand_30.tif")
clay <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/Clay_30.tif")
ph <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/pH_30.tif")
soc <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/250/SOC_30.tif")
jp <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_JP.tif")
bs  <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_BS.tif")
dec <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_DB.tif")
grsh <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_GrassShrub.tif")
nv <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_NonVeg.tif")
ocon <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_OtherCon.tif")
ws <- raster("/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg/ABoVE_VegMap_msin_500m_V1_WS.tif")
pfi <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/1000/PFI.tif")
ruggedness <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/1000/Ruggedness.tif")
tc <- "/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/tree_cover"
twi <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/topo/TWI.tif")
elevation <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/topo/elevation.tif")
aspect <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/topo/aspect.tif")
slope <- raster("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/topo/slope.tif")
vi <- "/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/modis_VI2"


#burn pts needed to extract from all the rasters
pts <-read_sf("/mnt/data1//boreal/spotter/combustion/burned_area/final_predictions/cna/modis_shps/all_modis_pts.shp") 

#read in all the fwis
fwi <-  list.files("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/FWI_ee", pattern = "*.csv", full.names = T) %>%
          map_df(~read_csv(.)) %>% 
          dplyr::select(-"system:index", -".geo") %>%
          dplyr::rename(Wind.speed = wspeed, Relative.humidity = rh, Temperature = t, VPD = vpd)
colSums(is.na(fwi))


#95% of the points we have

cna <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/cna/final_cna.csv")#this is the correct number

#missing fwi points to extract with no fwi next
missing_fwi <- cna %>% filter(!ID1 %in% fwi$ID1)
write_csv(missing_fwi, file.path(miss_out, "missing_fwi_for_pred.csv"))

#the cna points are actually clipped to the above domain
pts <- pts %>% filter(ID1 %in% cna$ID1)

#select missing fwi pts
pts <- pts %>% filter(ID1 %in% fwi$ID1)
cna <- cna %>% filter(ID1 %in% fwi$ID1)

# we have 96% of the points with FWI's

#join FWI to CNA for later
cna <- left_join(fwi, cna %>% dplyr::select(-ID2), by = 'ID1')

#----------------------------------end read in pathways

#all the years we need to extract data for
years <- seq(2001, 2017, 1)
years <- seq(2004, 2017, 1)

#set up cluster - requires doSNOW, and parallel packages
cores <- detectCores() - 3
cl <-makeCluster(cores)
registerDoSNOW(cl)

#loop through the years
# foreach (year = years, .packages=c("raster", "sf", "tidyverse", "rgdal", "lubridate", "doParallel",  'plyr', "doSNOW")) %dopar% {
for (year in years){
  print(year)
  #subset the pts by the year
  sub_pts <- pts %>% filter(ID2 == as.character(year))
  
  #year suffix
  year_suf <- paste0(as.character(year), '.tif')
 
  #read in variables associated with year
  stand_ras <- raster(file.path(stand.age, year_suf))
  tc_ras <- raster(file.path(vi, as.character(year), 'Tree.cover.tif'))
  rbr_ras <- raster(file.path(vi, as.character(year), 'rbr.tif'))
  rdbr_ras <- raster(file.path(vi, as.character(year), 'rdnbr.tif'))
  bright_ras <- raster(file.path(vi, as.character(year), 'brightness.tif'))
  dnbr_ras <- raster(file.path(vi, as.character(year), 'dNBR.tif'))
  green_ras <- raster(file.path(vi, as.character(year), 'greenness.tif'))
  ndii_ras <- raster(file.path(vi, as.character(year), 'NDII.tif'))
  ndvi_ras <- raster(file.path(vi, as.character(year), 'NDVI.tif'))
  wet_ras <- raster(file.path(vi, as.character(year), 'wetness.tif'))
  
  
  #extract all variables
  
  #topo variables
  aspect_ex <-  as_tibble(raster::extract(aspect, sub_pts, df = T), na.rm = T)
  slope_ex <-  as_tibble(raster::extract(slope, sub_pts, df = T), na.rm = T)
  elevation_ex <-  as_tibble(raster::extract(elevation, sub_pts, df = T), na.rm = T)
  twi_ex <-  as_tibble(raster::extract(twi, sub_pts, df = T), na.rm = T)
  
  #veg variables
  jp_ex <-  as_tibble(raster::extract(jp, sub_pts, df = T), na.rm = T)
  names(jp_ex) <- c('ID', 'JP')
  
  bs_ex <-  as_tibble(raster::extract(bs, sub_pts, df = T), na.rm = T)
  names(bs_ex) <- c('ID', 'BS')
  
  dec_ex <-  as_tibble(raster::extract(dec, sub_pts, df = T), na.rm = T)
  names(dec_ex) <- c('ID', 'DEC')
  
  grsh_ex <-  as_tibble(raster::extract(grsh, sub_pts, df = T), na.rm = T)
  names(grsh_ex) <- c('ID', 'GRSH')
  
  nv_ex <-  as_tibble(raster::extract(nv, sub_pts, df = T), na.rm = T)
  names(nv_ex) <- c('ID', 'NV')
  
  ocon_ex <-  as_tibble(raster::extract(ocon, sub_pts, df = T), na.rm = T)
  names(ocon_ex) <- c('ID', 'OCON')
  
  ws_ex <-  as_tibble(raster::extract(ws, sub_pts, df = T), na.rm = T)
  names(ws_ex) <- c('ID', 'WS')
  
  
  #pfi and ruggedness
  pfi_ex <-  as_tibble(raster::extract(pfi, sub_pts, df = T), na.rm = T)
  rug_ex <-  as_tibble(raster::extract(ruggedness, sub_pts, df = T), na.rm = T)
  
  
  #soil variables
  silt_ex <-  as_tibble(raster::extract(silt, sub_pts, df = T), na.rm = T)
  sand_ex <-  as_tibble(raster::extract(sand, sub_pts, df = T), na.rm = T)
  clay_ex <-  as_tibble(raster::extract(clay, sub_pts, df = T), na.rm = T)
  ph_ex <-  as_tibble(raster::extract(ph, sub_pts, df = T), na.rm = T)
  soc_ex <-  as_tibble(raster::extract(soc, sub_pts, df = T), na.rm = T)
  bd_ex <-  as_tibble(raster::extract(bd, sub_pts, df = T), na.rm = T)
  
  
  #all vi and stand age
  stand_ex <-  as_tibble(raster::extract(stand_ras, sub_pts, df = T), na.rm = T)
  names(stand_ex) <- c('ID', 'stand.age')
  
  tc_ex <-  as_tibble(raster::extract(tc_ras, sub_pts, df = T), na.rm = T)
  names(tc_ex) <- c('ID', 'Tree.cover')
  
  rbr_ex <-  as_tibble(raster::extract(rbr_ras, sub_pts, df = T), na.rm = T)
  rdnbr_ex <-  as_tibble(raster::extract(rdbr_ras, sub_pts, df = T), na.rm = T)
  bright_ex <-  as_tibble(raster::extract(bright_ras, sub_pts, df = T), na.rm = T)
  dnbr_ex <-  as_tibble(raster::extract(dnbr_ras, sub_pts, df = T), na.rm = T)
  green_ex <-  as_tibble(raster::extract(green_ras, sub_pts, df = T), na.rm = T)
  ndii_ex <-  as_tibble(raster::extract(ndii_ras, sub_pts, df = T), na.rm = T)
  ndvi_ex <-  as_tibble(raster::extract(ndvi_ras, sub_pts, df = T), na.rm = T)
  wet_ex <-  as_tibble(raster::extract(wet_ras, sub_pts, df = T), na.rm = T)
  
  all <- list(aspect_ex, slope_ex, elevation_ex, twi_ex, jp_ex, bs_ex, dec_ex, grsh_ex, nv_ex, ocon_ex, ws_ex, pfi_ex, 
              rug_ex, silt_ex, sand_ex, clay_ex, ph_ex, soc_ex, stand_ex, tc_ex, bright_ex, dnbr_ex, green_ex, ndii_ex, 
              ndvi_ex, wet_ex, rbr_ex, rdnbr_ex, bd_ex)
  
  final <- plyr::join_all(all, by = 'ID', type = "left")
  
  final$ID1 <- sub_pts$ID1
  final$ID2 <- sub_pts$ID2
  
  final <- left_join(final, cna, by = 'ID1')

  #make missing permafrost 0, it is in south of domain
  final <- final %>% mutate(PFI = ifelse(is.na(PFI), 0, PFI))
  
  #fill missing soil with mean of soil and ruggedness with mean
  final <- final %>% mutate(Clay_30 = ifelse(is.na(Clay_30), mean(Clay_30, na.rm = T), Clay_30))
  final <- final %>% mutate(Sand_30 = ifelse(is.na(Sand_30), mean(Sand_30, na.rm = T), Sand_30))
  final <- final %>% mutate(Silt_30 = ifelse(is.na(Silt_30), mean(Silt_30, na.rm = T), Silt_30))
  final <- final %>% mutate(BD_30 = ifelse(is.na(BD_30), mean(BD_30, na.rm = T), BD_30))
  final <- final %>% mutate(pH_30 = ifelse(is.na(pH_30), mean(pH_30, na.rm = T), pH_30))
  final <- final %>% mutate(SOC_30 = ifelse(is.na(SOC_30), mean(SOC_30, na.rm = T), SOC_30))
  final <- final %>% mutate(Ruggedness = ifelse(is.na(Ruggedness), mean(Ruggedness, na.rm = T), Ruggedness))
  final <- final %>% mutate(Tree.cover = ifelse(is.na(Tree.cover), mean(Tree.cover, na.rm = T), Tree.cover))
  final <- final %>% mutate(TWI = ifelse(is.na(TWI), mean(TWI, na.rm = T), TWI))
  final <- final %>% mutate(aspect = ifelse(is.na(aspect), mean(aspect, na.rm = T), aspect))
  final <- final %>% mutate(slope = ifelse(is.na(slope), mean(slope, na.rm = T), slope))
  final <- final %>% mutate(elevation = ifelse(is.na(elevation), mean(elevation, na.rm = T), elevation))
  
  final <- final %>% mutate(NV= ifelse(is.na(NV), 0, NV))
  final <- final %>% mutate(JP = ifelse(is.na(JP), 0, JP))
  final <- final %>% mutate(GRSH = ifelse(is.na(GRSH), 0, GRSH))
  final <- final %>% mutate(DEC = ifelse(is.na(DEC), 0, DEC))
  final <- final %>% mutate(OCON = ifelse(is.na(OCON), 0, OCON))
  final <- final %>% mutate(WS = ifelse(is.na(WS), 0, WS))
  final <- final %>% mutate(BS = ifelse(is.na(BS), 0, BS))
  
  final <- as_tibble(final)
  final$DOB_lst <- sub_pts$DOB_lst
  
  colSums(is.na(final))
  
  #get missing_stand_age
  not_miss <- final %>% dplyr::select(ID1, stand.age) %>% drop_na()

  miss <- final %>% filter(!ID1 %in% not_miss$ID1)
  
  colSums(is.na(final))
  
  final <- final %>% drop_na()
  
  
  write_csv(miss, file.path(miss_out, paste0(as.character(year), '.csv')))
  write_csv(final, file.path(out, paste0(as.character(year), '.csv')))
}

