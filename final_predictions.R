library(tidyverse)
library(sf)
library(rgdal)
library(parallel)
library(doSNOW)
library(plyr)
library(raster)
library(caret)


#set a random seed
set.seed(170) 

#read in the training data
df <- read_csv("/mnt/data1/boreal/spotter/combustion/final_files/raw/all_predictors.csv")

#shuffle the dataframe
df <- df[sample(1:nrow(df)), ]

#get some columns to remove such as id, latitude etc.
bad_cols <- c('id', 'project.name', 'burn_year', 'latitude', 'longitude')

#remove abovground bad cols
above <- df %>% dplyr::select(-(bad_cols), -below.ground.carbon.combusted) %>% drop_na()  
above$above.carbon.combusted <- above$above.carbon.combusted/ 1000.0

#remove belowground bad cols
below <- df %>% dplyr::select(-(bad_cols), -above.carbon.combusted) %>% drop_na()  
below$below.ground.carbon.combusted <- below$below.ground.carbon.combusted / 1000.0

#read in all the pts to join back as a spatial frame

#burn pts needed to extract from all the rasters
pts <-read_sf("/mnt/data1//boreal/spotter/combustion/burned_area/final_predictions/cna/modis_shps/all_modis_pts.shp") 

# #read in all the fwis
# fwi <-  list.files("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/FWI_ee", pattern = "*.csv", full.names = T) %>%
#   map_df(~read_csv(.)) %>%
#   dplyr::select(-"system:index", -".geo") %>%
#   dplyr::rename(Wind.speed = wspeed, Relative.humidity = rh, Temperature = t, VPD = vpd)
# 
# cna <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/final_predictions/cna/final_cna.csv")#this is the correct number
# 
# #the cna points are actually clipped to the above domain
# pts <- pts %>% filter(ID1 %in% cna$ID1)
# 
# #missing fwi points to extract with no fwi next
# missing_fwi <- cna %>% filter(!ID1 %in% fwi$ID1)
# 
# #the cna points are actually clipped to the above domain
# pts <- pts %>% filter(ID1 %in% cna$ID1)
# 
# #select missing fwi pts
# pts <- pts %>% filter(ID1 %in% missing_fwi$ID1)
# cna <- cna %>% filter(ID1 %in% missing_fwi$ID1)


predict_c <- function(df, in_model, importance, out_path, linear_model, in_pts){

  importance <- read_csv(importance)
  good_cols <- importance$Variables
  
  #set up cluster - requires doSNOW, and parallel packages
  cores <- detectCores() - 3
  cl <-makeCluster(cores)
  registerDoSNOW(cl)

  #create the outpath in case it doesn't exist
  dir.create(out_path, recursive = T)
  
  #an emptry list to store function output
  #read in the final model
  final_model <- readRDS(in_model)
  
  #rename the first column (y) to y
  df <- df %>% dplyr::rename(y = names(df)[1])
  
  #save a copy of non-normalized values
  raw <- df %>% dplyr::select(-y)
  
  #function to normalize if desired
  normalized <- function(x) (x- min(x))/(max(x) - min(x))
  
  #normaize the predictor variables
  df[, 2:length(colnames(df))] <- lapply(df[, 2:length(colnames(df))], normalized)
  
  #select good columns
  good_cols <- names(final_model$trainingData)[2:length(names(final_model$trainingData))]
  df <- df %>% dplyr::select(y, good_cols)
  
  #final model
  model_ranger_final <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = final_model$bestTune$mtry, .splitrule = final_model$bestTune$splitrule, .min.node.size = final_model$bestTune$min.node.size))
  
  
  #read in a file to use as the rasterize information
  for_ras <- raster("/mnt/data1//boreal/scooperdock/Combustion/Final/translate/translated_2004.tif", band = 2)
  
  #read in the linear model
  linearMod <- readRDS(linear_model)
  
  
  #years to predict on
  years <- seq(2001, 2017, 1)
  for(year in years){
  # foreach (year = years, .packages=c("raster", "sf", "tidyverse", "rgdal", "lubridate", "doParallel",  'plyr', "doSNOW")) %dopar% {
      
    # #read in the file to predict on
    # for_predict <- read_csv(file.path("/mnt/data1/boreal/spotter/combustion/final_files/missing_for_predictions/final_csv/all_data",
    #                        paste0(as.character(year), '.csv'))) 
    
    for_predict <- read_csv(file.path("/mnt/data1/boreal/spotter/combustion/final_files/final_for_prediction/final_csv/all_data",
                                      paste0(as.character(year), '.csv'))) 
    
    if(nrow(for_predict) > 0){
      
      # #remove the IDS where we don't have stand age
      # no_age <- read_csv(file.path("/mnt/data1/boreal/spotter/combustion/final_files/missing_for_predictions/final_csv/no_fwi",
      #                        paste0(as.character(year), '.csv')))
      # 
      # for_predict <- for_predict %>% 
      #                filter(!ID1 %in% no_age$ID1)
      
      colSums(is.na(for_predict))
      
      #get sub pts
      sub_pts <- in_pts %>% filter(ID1 %in% for_predict$ID1)
      
      #select appropriate columns
      for_predict <- for_predict %>% 
                     dplyr::rename(CNA_Tmax_5_8 = Tmax_5_8) %>%
                     dplyr::select(importance$Variables)
      
      #normalize
      for_predict[, 1:length(colnames(for_predict))] <- lapply(for_predict[, 1:length(colnames(for_predict))], normalized)
      
      
      preds <- predict(model_ranger_final, newdata = for_predict, cl = cl)
      
      #add the predictions to the sub spatial frame
      sub_pts$Preds <- preds
      
      #adjust the predictions with the scaling model
      # sub_pts <- sub_pts %>% dplyr::mutate(Corrected = linearMod$coefficients[1] + (linearMod$coefficients[2] * Preds))
      sub_pts <- sub_pts %>% dplyr::mutate(Corrected = linearMod$Intercept + (linearMod$Slope * Preds))
      
      write_sf(sub_pts, file.path(out_path, paste0(as.character(year), '.shp')))
      
      #rasterize
      # out_ras <- rasterize(sub_pts, for_ras, field = 'Corrected')
      # writeRaster(out_ras, file.path(out_path, paste0(as.character(year), '.tif')), overwrite = T)
      # 
    }
  }
  return(sub_pts)
  # stopCluster(cl)
  
}


above <- predict_c(df = above,
                   in_model = '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_age/full_model_ranger.rds',
                   importance = '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_age/rfe_importance.csv',
                   out_path = "/mnt/data1/boreal/spotter/combustion/final_files/final_rasters/aboveground/rfe_no_age",
                   linear_model = "/mnt/data1/boreal/spotter/combustion/final_files/scaling/aboveground/no_age/combined_type2/linear_model.rds",
                   in_pts = pts)

below <- predict_c(df = below,
                  in_model = '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_age/full_model_ranger.rds',
                  importance = '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_age/rfe_importance.csv',
                  out_path  = "/mnt/data1/boreal/spotter/combustion/final_files/final_rasters/belowground/rfe_no_age",
                  linear_model = "/mnt/data1/boreal/spotter/combustion/final_files/scaling/belowground/no_age/combined_type2/linear_model.rds",
                  in_pts = pts)



