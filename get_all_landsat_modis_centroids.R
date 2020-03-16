library(raster)
library(sf)
library(tidyverse)
library(doSNOW)
library(parallel)


#----------------------re-project jwang landsat to MODIS, extract to each MODIS shp file
#----------------------then convert MODIS and extracted landsat to pts and export for EE extraction

#set temp dir
temp_dir <- '/mnt/data1/boreal/spotter/temp_dir'
dir.create(temp_dir, recursive = T)
#rasterOptions()
# dirname(rasterTmpFile())
rasterOptions(tmpdir=temp_dir)

#landsat individual out path
land_out = '/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/shps'
dir.create(land_out, recursive = T)

land_csv_out = '/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input'

dir.create(land_out, recursive = T)
#get the pathway with all scenes 
all_path = "/mnt/data1/boreal/raw/landsat_veg/wang/data"
modis_shp_path = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes"
modis_ras_path = "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/tifs"

#read in the needed scenes
good_scenes =  read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/needed_above_scenes.csv")

#pad bh and bv to 2 digits
good_scenes = good_scenes %>% mutate(bh = sprintf("%02d", bh), bv = sprintf("%02d", bv))

good_scenes = good_scenes %>% mutate(ID = paste0('ABoVE_LandCover_Bh', bh, 'v', bv, '.tif'))
years <- c(2004, 2006, 2012, 2014, 2015)

#loop through years and determine which all_scenes to grab from good scenes

cores <- detectCores() - 1
cl <-makeCluster(cores)
registerDoSNOW(cl)
# for (year in years){
foreach (year = years, .packages=c("raster", 'tidyverse', 'sf')) %dopar% {

  year_comb = c()
  sub_good = good_scenes %>% filter(burn_yr == year)

  #read in the modis file from this year
  modis_shp =  read_sf(file.path(modis_shp_path, paste0('translated_', as.character(year), '.shp')))
  # modis_tif = raster(file.path(modis_ras_path, paste0('translated_', as.character(year), '.tif')))

  mod_land = st_transform(modis_shp, ' +proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no')
  #get unique file names to read in
  unique = unique(sub_good$ID)
  # mod_final[[length(mod_final) + 1]] = modis_shp

  for (i in unique){

    if (file.exists(file.path(all_path, i))){
      
      #read in the correct .tif file from jwant
      ras = raster(file.path(all_path, i), band = 1)

      #extract by mask
      ras = raster::mask(ras, as(mod_land, 'Spatial'))

      #convert ras to point
      ras = st_as_sf(rasterToPoints(ras, spatial = T))

      if (nrow(ras) != 0){

        print(i)
        #convert to wgs
        ras = st_transform(ras, '+proj=longlat +datum=WGS84 +no_defs')

        #join pts to modis_shp to get MODIS ids
        ras = st_join(ras, modis_shp, join = st_intersects)

        #add landsat ID
        ras = ras %>% dplyr::mutate(land_ID1 = paste0(dplyr::row_number(), '_', as.character(burn_yr)))
        ras = ras %>% dplyr::select(ID1, ID2, burn_yr, land_ID1, geometry)
        #save
        year_comb[[length(year_comb) + 1]] = ras
      }
    }
  }

  final_year = sf::st_as_sf(data.table::rbindlist(year_comb))

  # print(year)
  # print(head(final_year))
  write_sf(final_year, file.path(land_out, paste0('translated_', as.character(year), '.shp')))
  # land_final[[length(land_final) + 1]] = final_year
}

# stopCluster(cl)


#save out modis final, for this file ID1 = distinct within a year, ID2 = all distinct pixels
mod_path = list.files("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/shapes", pattern = '.shp$', full.names = T) %>% map(read_sf)
mod_final = sf::st_as_sf(data.table::rbindlist(mod_path)) %>% dplyr::select(ID1, ID2, burn_yr, geometry)

write_sf(mod_final, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years.shp")

te <- read_sf("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years.shp")

#turn to points
te <- te %>% st_centroid()
write_sf(te, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/all_years_pts.shp")

#save out landsat final, for this file ID1 = each distinct pixel within a modis year, ID2 = all modis pixels, land_ID1 = landsat_within a year??, x= 
#there are missing scenes in jwang which is why things don't exactly match
land_path = list.files("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/shps", pattern = '.shp$', full.names = T) %>% map(read_sf)
land_final = sf::st_as_sf(data.table::rbindlist(land_path)) %>% dplyr::select(ID1, ID2, land_ID1, burn_yr, geometry)
land_final <- land_final %>% dplyr::mutate(land_ID1 = row_number())

#find missing ID2
miss <- setdiff(mod_final$ID2, land_final$ID2)

#for land_final here ID2 is the modis pixels
#land_ID1 is the landsat pixels

write_sf(land_final, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/all_years.shp")

#save out landsat for climatena
coords = data.frame(st_coordinates(land_final))
land_final$lat = coords$Y
land_final$lon = coords$X

#id1 here is all the modis ids and id2 is all the unique landsat ids
land_final = as_tibble(data.frame(land_final)) %>% dplyr::rename(id1 = ID2, id2 = land_ID1) %>% dplyr::select(id1, id2, lat, lon)
# land_final = land_final %>% dplyr::mutate(ID1 = row_number()) %>% dplyr::rename(ID2 = land_ID2)
# land_final = land_final %>% dplyr::select(ID1, ID2, lat, lon)
write_csv(land_final, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/landsat_climatena_input.csv")


