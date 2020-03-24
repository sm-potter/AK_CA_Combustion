library(raster)
library(doSNOW)
library(parallel)

cores <- detectCores()
cl <-makeCluster(cores)
registerDoSNOW(cl)

raster_path <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/ee/VI'
out <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_mosaiced/ee/VI'
dir.create(out, recursive = T)

years <- seq(2001, 2017, 1)

bands <- c('dNBR', 'brightness', 'greenness', 'wetness', 'NDVI', 'NDII')
bands2 <- seq(1, 6, 1)

for(year in years){
# foreach (year = years, .packages=c("raster")) %dopar% {
  
  final_out <- file.path(out, paste0(as.character(year)))
  dir.create(final_out, recursive = T)   
  
  rasters <- list.files(file.path(raster_path, as.character(year)), full.names = T, pattern = '.tif$')
  
  for(n in 1:length(bands)){
    
    all_ras <- list()
    
    b1 <- bands[n]
    b2 <- bands2[n]
    
    one_ras <- rasters[1]
    two_ras <- rasters[2]
    
    one_ras <- raster(one_ras, band = b2)
    two_ras <- raster(two_ras, band = b2)
  
    #mosaic
    mos <- mosaic(one_ras, two_ras, fun = mean)
    
    # in_ras = raster('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu/2004.tif')
    # 
    # out_ras <- projectRaster(mos, in_ras, method = 'bilinear')
    writeRaster(mos, file.path(final_out, paste0(b1, '.tif')), overwrite = T)
  }
}
