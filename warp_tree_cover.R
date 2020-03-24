library(raster)
library(doSNOW)
library(parallel)

# cores <- detectCores() 
# cl <-makeCluster(cores)
# registerDoSNOW(cl)

raster_path <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/ee/rest/tree_cover'
out <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/tree_cover'
dir.create(out, recursive = T)

years <- seq(2001, 2017, 1)

# for(year in years){
foreach (year = years, .packages=c("raster")) %dopar% {
  
  rasters <- list.files(file.path(raster_path, as.character(year)), full.names = T, pattern = '.tif$')
  rasters <- lapply(rasters, 'raster')
  
  #mosaic
  rasters$fun <- mean
  mos <- do.call(mosaic, rasters)
  
  # in_ras = raster('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu/2004.tif')
  
  # out_ras <- projectRaster(mos, in_ras, method = 'bilinear')
  writeRaster(mos, file.path(out, paste0(as.character(year), '.tif')), overwrite = T)
}
