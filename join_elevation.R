library(tidyverse)
library(sf)


#-----------------landsat join
#original climatena input
df <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/landsat_climatena_input.csv")

#elevation, id2 in df links to land_ID1 in df2
df2 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/landsat_elevation.csv")
df2 <- df2 %>% dplyr::select(land_ID1, elevation) 
df2 <- df2 %>% dplyr::rename(id2 = land_ID1) %>% dplyr::rename(elev = elevation)

#join
df2 <- left_join(df, df2, by = 'id2') %>% drop_na() %>% dplyr::select(id1, id2, lat, lon, elev)

write_csv(df2, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/landsat_final.csv")


#------------------modis join
#original climatena input
df <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/modis_climatena_input.csv")

#elevation, id2 in df links to land_ID1 in df2
df2 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/modis_elevation.csv")
df2 <- df2 %>% dplyr::select(ID2, elevation) 
df2 <- df2 %>% dplyr::rename(elev = elevation)

#join
df2 <- left_join(df, df2, by = 'ID2') %>% drop_na() %>% dplyr::select(ID1, ID2, lat, lon, elev)

write_csv(df2, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/input/modis_final.csv")


