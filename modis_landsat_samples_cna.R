library(tidyverse)

#-----------------------------this will pull together the landsat and modis samples for cna
#-----------------------------to use for predictions

g_vars <- c('Year', 'id1', 'id2', 'PPT_sm',
'Tmax_sm', 'MAT', 'MAP', 'MAR', 'CMD', 'PAS', 
'NFFD', 'TD')

ann_g_vars <- c("Year", "id1", "id2", "MAT", "MWMT", "MCMT", "TD", "MAP", "MSP", "AHM", "SHM", "DD_0", "DD5",      
                 "DD_18", "DD18", "NFFD", "bFFP", "eFFP", "FFP" , "PAS" , "EMT",      
                 "EXT", "Eref", "CMD","MAR", "RH")

seas_g_vars <- c('Year', 'id1', 'id2', "Tmax_sm", "PPT_sm", "Rad_sm", "DD_0_sm", "DD5_sm", "DD_18_sm",
                 "DD18_sm", "NFFD_sm", "PAS_sm", "Eref_sm", "CMD_sm", "RH_sm", "Tave_sm", "Tmin_sm")

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


