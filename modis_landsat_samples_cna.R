library(tidyverse)

#-----------------------------this will pull together the landsat and modis samples for cna
#-----------------------------to use for predictions

g_vars <- c('Year', 'id1', 'id2', 'PPT_sm',
'Tmax_sm', 'MAT', 'MAP', 'MAR', 'CMD', 'PAS', 
'NFFD', 'TD')

ann_g_vars <- c('Year', 'id1', 'id2', 'MAT', 'MAP', 'MAR', 'CMD', 'PAS', 'NFFD', 'TD')

seas_g_vars <- c('Year', 'id1', 'id2',  'PPT_sm',
                 'Tmax_sm')

#---------------------get all annual landsat
one <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/landsat_final_1981-2010YT.csv") %>% dplyr::select(ann_g_vars, -id1)



#seasonal
two <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/landsat_final_1981-2010S.csv") %>% dplyr::select(seas_g_vars, -id1)


land <- left_join(one, two, by = c('Year', 'id2'))

land <- land %>% group_by(id2) %>% summarize_all(funs(mean), na.rm = T)

write_csv(land %>% dplyr::select(-Year), "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/land_cna.csv")

#---------------------get all modis
one <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/modis_final_1981-2010Y.csv") %>% dplyr::select(ann_g_vars)

two <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/climateNA/output/modis_final_1981-2010S.csv") %>% dplyr::select(seas_g_vars)

mod <- left_join(one, two, by = c('Year', 'id1', 'id2'))

mod <- mod %>% group_by(id1, id2) %>% summarize_all(funs(mean),  na.rm = T)

write_csv(mod %>% dplyr::select(-Year), "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/mod_cna.csv")


