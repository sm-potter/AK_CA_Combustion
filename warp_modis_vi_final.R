library(raster)
library(doSNOW)
library(parallel)
library(gdalUtils)

cores <- detectCores()
cl <-makeCluster(cores)
# cl <- makeCluster(5, type = 'SOCK')
registerDoSNOW(cl)

raster_path <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_mosaiced/ee/modis_VI'
out <- '/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions_warped/ee/modis_VI'
dir.create(out, recursive = T)

years <- seq(2001, 2017, 1)

bands <- c('Tree.cover', 'dNBR', 'rbr', 'rdnbr', 'NDVI', 'NDII', 'brightness', 'greenness', 'wetness')
bands2 <- seq(1, 9, 1)

# for(year in years){
foreach (year = years, .packages=c("raster", "gdalUtils", 'parallel', 'doSNOW')) %dopar% {
  
  final_out <- file.path(out, paste0(as.character(year)))
  dir.create(final_out, recursive = T)   
  
  # for(n in 1:length(bands)){
  foreach(n  = 1:length(bands), .packages=c("raster", "gdalUtils", 'parallel', 'doSNOW')) %dopar% {
      
    all_ras <- list()
    
    b1 <- bands[n]
    b2 <- bands2[n]
    
    ras <- raster(file.path(raster_path, as.character(year), paste0(b1, '.tif')), full.names = T, pattern = '.tif$')
    
    ##Read in file with correct grid
    mask <-  raster("/mnt/data1/boreal/spotter/combustion/update_2011_pan/raw/bor_mask.tif")
    
    #Create empty raster using attributes from above
    e <-  mask@extent
    template <-  raster(e)
    projection(template) <-  mask@crs
    
    #
    # #Write empty raster to file
    writeRaster(template, file = file.path(final_out,  paste0(b1, '.tif')), format = 'GTiff',overwrite=T)
    
    gdal_translate(src_dataset = file.path(raster_path, as.character(year), paste0(b1, '.tif')), dst_dataset = file.path(final_out, paste0(b1, '.tif')), a_srs = mask@crs)
    
    mos <- raster(file.path(final_out, paste0(b1, '.tif')))
    
    in_ras = raster('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu/2004.tif')
    # 
    out_ras <- projectRaster(mos, in_ras, method = 'bilinear')
    writeRaster(out_ras, file.path(final_out, paste0(b1, '.tif')), overwrite = T)
  }
}
