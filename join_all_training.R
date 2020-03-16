library(tidyverse)
library(plyr)


#read in the initial files
in_path <- "/mnt/data1/boreal/spotter/combustion/final_files/raw"

#initial vars from brendan and sander
initial <- read_csv(file.path(in_path, 'for_extraction.csv'))

#remove some columns which would be doubled up

initial <- initial %>% dplyr::select(-slope, -aspect, -TWI, -elevation, -VPD)

#fwi 
fwi <- read_csv(file.path(in_path, 'FWI_training.csv'))

#stand ages
stand_age <- read_csv(file.path(in_path, 'stand_age_training.csv')) %>% dplyr::select(-burn_year)

#landsat/topo variables from ee
land_topo <- read_csv(file.path(in_path, 'all_remotely_sensed_extracted.csv')) %>% dplyr::select(-'system:index', -".geo")
land_topo <- land_topo %>% dplyr::rename(Tree.cover = tree_canopy_cover)

#join it all together
final <- plyr::join_all(list(initial, fwi, stand_age, land_topo), by = 'id', type = 'inner')

write_csv(final, file.path(in_path, 'all_predictors.csv'))
