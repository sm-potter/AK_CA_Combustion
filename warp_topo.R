library(raster)
library(doSNOW)
library(parallel)

cores <- detectCores()
cl <-makeCluster(cores)
registerDoSNOW(cl)

rasterOptions(tmpdir = "/mnt/data1/boreal/spotter/temp_dir")

raster_path <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/ee/rest/topo'
out <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_mosaiced/ee/rest/topo'
dir.create(out, recursive = T)

years <- seq(2001, 2017, 1)

bands <- c('TWI', 'elevation', 'aspect', 'slope')
bands2 <- seq(1, 4, 1)

# for(year in years){
foreach (band_n = bands2, .packages=c("raster")) %dopar% {
  
  rasters <- list.files(raster_path, full.names = T, pattern = '.tif$')
  
  all_ras <- list()
  
  b1 <- bands[band_n]
  b2 <- bands2[band_n]
  
  one_ras <- rasters[1]
  two_ras <- rasters[2]
  # three_ras <- rasters[3]
  
  one_ras <- raster(one_ras, band = b2)
  two_ras <- raster(two_ras, band = b2)
  # three_ras <- raster(three_ras, band = b2)
  
  #mosaic
  mos <- mosaic(one_ras, two_ras, fun = mean)
  
  # in_ras = raster('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu/2004.tif')
  # 
  # out_ras <- projectRaster(mos, in_ras, method = 'bilinear')
  writeRaster(mos, file.path(out, paste0(b1, '.tif')), overwrite = T)
}

removeTmpFiles(h=24)

