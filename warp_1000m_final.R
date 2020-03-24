library(raster)
library(doSNOW)
library(parallel)
library(gdalUtils)

cores <- detectCores()
cl <-makeCluster(cores)
registerDoSNOW(cl)

rasterOptions(tmpdir = "/mnt/data1/boreal/spotter/temp_dir")

raster_path <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_mosaiced/ee/rest/1000'
out <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/rest/1000'
dir.create(out, recursive = T)

years <- seq(2001, 2017, 1)

bands <- c('Ruggedness', 'PFI')
bands2 <- seq(1, 2, 1)

# for(year in years){
foreach (band_n = bands2, .packages=c("raster", "gdalUtils")) %dopar% {
  
  b1 <- bands[band_n]
  b2 <- bands2[band_n]
  
  ras <- raster(file.path(raster_path, paste0(b1, '.tif')), full.names = T, pattern = '.tif$')
  ##Read in file with correct grid
  mask <-  raster("/mnt/data1/boreal/spotter/combustion/update_2011_pan/raw/bor_mask.tif")
  
  #Create empty raster using attributes from above
  e <-  mask@extent
  template <-  raster(e)
  projection(template) <-  mask@crs
  
  #
  # #Write empty raster to file
  writeRaster(template, file = file.path(out, paste0(as.character(b1), '.tif')), format = 'GTiff',overwrite=T)
  
  gdal_translate(src_dataset = file.path(raster_path, paste0(as.character(b1), '.tif')), dst_dataset = file.path(out, paste0(as.character(b1), '.tif')), a_srs = mask@crs)
  
  mos <- raster(file.path(out, paste0(as.character(b1), '.tif')))
  
  in_ras = raster('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu/2004.tif')
  
  out_ras <- projectRaster(mos, in_ras, method = 'bilinear')
  writeRaster(out_ras, file.path(out, paste0(b1, '.tif')), overwrite = T)
}

removeTmpFiles(h=24)

