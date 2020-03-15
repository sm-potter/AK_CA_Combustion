library(raster)
library(tidyverse)
library(sf)
library(doSNOW)
library(parallel)
library(gdalUtils)

rasterOptions(tmpdir = "/mnt/data1/boreal/spotter/temp_dir")
ras_path <- "/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu_w_ak_lfdb_domain"


df <- read_sf("/mnt/data1/boreal/spotter/combustion/final_files/raw/for_extraction.shp") %>% dplyr::select(id, burn_year)

#we can only use ages 2001 and over
df <- df %>% filter(burn_year >= 2001)

final <- list()
for(y in unique(df$burn_year)){
  
  #subset year
  sub <- df %>% filter(burn_year == y)
  
  #read in the appropriate raster year
  in_ras <- raster(file.path(ras_path, paste0(as.character(y), '.tif')))
  
  #stract
  ex <- as_tibble(data.frame(raster::extract(in_ras, sub)))
  names(ex) <- c('stand.age')
  
  ex$id <- sub$id
  ex$burn_year <- sub$burn_year
  final[[length(final) + 1]] <- ex
}

final <- bind_rows(final)

write_csv(final, "/mnt/data1/boreal/spotter/combustion/final_files/raw/stand_age_training.csv")

