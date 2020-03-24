library(raster)
library(doSNOW)
library(parallel)

cores <- detectCores() 
cl <-makeCluster(cores)
registerDoSNOW(cl)

# rasters <- list.files('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu', full.names = F, pattern = '.tif$')
# out <- '/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu_ee'
# dir.create(out, recursive = T)
# # for(ras in rasters){
# foreach (ras = rasters, .packages=c("raster")) %dopar% {
#   
#   in_ras = raster(file.path('/mnt/data1/boreal/spotter/combustion/update_2011_pan/adjusted_ages_beu', ras))
#   
#   out_ras <- projectRaster(in_ras, crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs', res = xres(in_ras), method = 'bilinear')
#   writeRaster(out_ras, file.path(out, ras), overwrite = T)
# }


rasters <- list.files('/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg', full.names = F, pattern = '.tif$')
out <- '/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg_ee'
dir.create(out, recursive = T)
foreach (ras = rasters, .packages=c("raster")) %dopar% {
  
  in_ras = raster(file.path('/mnt/data1/boreal/spotter/auxillary/Masks/ABoVE_veg', ras))
  
  out_ras <- projectRaster(in_ras, crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs', res = xres(in_ras), method = 'bilinear')
  writeRaster(out_ras, file.path(out, ras), overwrite = T)
}