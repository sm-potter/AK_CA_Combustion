#######################this script will translate my python setup to get variables and grid search combustion#####################
library(tidyverse)
library(caret)
library(doSNOW)
library(ranger)
library(xgboost)
library(caretEnsemble)
library(caTools)
library(doParallel)
library(gridExtra)
library(cowplot)
library(kernlab)
library(lmodel2)


#combined all the data
# mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/pure_fire_mixed_veg/modis_pixels/predictors/all_modis_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_one'))
# land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/pure_fire_mixed_veg/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_one'))
# land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/pure_fire_mixed_veg/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_one'))
# 
# mod_native2 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_mixed_veg/modis_pixels/predictors/all_modis_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_two'))
# land_to_modis2 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_mixed_veg/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_two'))
# land_native2 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/mixed_fire_mixed_veg/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_two'))
# 
# mod_native3 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/all_modis_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_three'))
# land_to_modis3 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_three'))
# land_native3 <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% mutate(ID2 = paste0(ID2, '_three'))
# 
# mod_native <- rbind(mod_native, mod_native2, mod_native3)
# land_to_modis <- rbind(land_to_modis, land_to_modis2, land_to_modis3)
# land_native <- rbind(land_native, land_native2, land_native3)
# 
# write_csv(mod_native, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/modis_pixels/predictors/all_modis_predictors.csv")
# write_csv(land_to_modis, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_resampled_predictors.csv")
# write_csv(land_native, "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_native_predictors.csv")

#--------read in training data

#set a random seed
# set.seed(1896) 
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


scaling <- function(df, in_model, out_path, importance, category){
  
  importance <- read_csv(importance)
  good_cols <- importance$Variables
  
  #set up cluster - requires doSNOW, and parallel packages
  cores <- detectCores() - 4 
  cl <-makeCluster(cores)
  registerDoSNOW(cl)
  
  #set the limit based on above or belowground
  limit1 <- ifelse(category == 'above', 0, 1.5)
  limit2 <- ifelse(category == 'above', 0.75, 4)
  
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
  
  
  #read in the modis native to predict on
  mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/modis_pixels/predictors/all_modis_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
  mod_native <- mod_native %>% dplyr::select(names(raw))
  
  #rescale some values in mod_native
  mod_native <- mod_native %>% mutate(dNBR = dNBR / 1000)
  mod_native <- mod_native %>% mutate(NDVI = NDVI / 1000)
  mod_native <- mod_native %>% mutate(NDII = NDII / 1000)
  mod_native <- mod_native %>% mutate(rdnbr = rdnbr / 1000)
  
  
  #change the min and max of all the appropriate columns
  
  #normalize variables
  mod_native[, 1:length(colnames(mod_native))] <- lapply(mod_native[, 1:length(colnames(mod_native))], normalized)
  
  
  #read in the landsat resampled to predict on
  land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
  
  #rescale some values in mod_native
  land_to_modis <- land_to_modis %>% mutate(dNBR = dNBR / 1000)
  land_to_modis <- land_to_modis %>% mutate(NDVI = NDVI / 1000)
  land_to_modis <- land_to_modis %>% mutate(NDII = NDII / 1000)
  land_to_modis <- land_to_modis %>% mutate(rdnbr = rdnbr / 1000)
  
  land_to_modis[, 1:length(colnames(land_to_modis))] <- lapply(land_to_modis[, 1:length(colnames(land_to_modis))], normalized)
  
  #read in the landsat native to predict on
  land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% dplyr::select(-ID2, -land_ID1) %>% drop_na()
  
  #rescale some values in mod_native
  land_native <- land_native %>% mutate(dNBR = dNBR / 1000)
  land_native <- land_native %>% mutate(NDVI = NDVI / 1000)
  land_native <- land_native %>% mutate(NDII = NDII / 1000)
  land_native <- land_native %>% mutate(rdnbr = rdnbr / 1000)
  
  land_native[, 1:length(colnames(land_native))] <- lapply(land_native[, 1:length(colnames(land_native))], normalized)
  
  mod_native <- mod_native %>% dplyr::select(good_cols)
  land_to_modis <- land_to_modis %>% dplyr::select(good_cols)
  land_native  <- land_native  %>% dplyr::select(good_cols)
  
  
  #predict modis_native
  mod_predict <- predict(model_ranger_final, newdata=mod_native, cl = cl)
  
  #predict land_to_modis
  land_mod_pred <- predict(model_ranger_final, newdata=land_to_modis, cl = cl)
  
  #predict land native
  land_native_pred <- predict(model_ranger_final, newdata=land_native, cl = cl)
  
  land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_native_predictors.csv")
  
  #combined land_native_pred back to original frame and take mean prediction by ID2 (which is what matches modis original)
  land_native_pred <- land_native %>% drop_na() %>% dplyr::select(ID2, land_ID1) %>% dplyr::mutate(land_native = land_native_pred)
  
  #groupby and mean by ID2
  land_native_pred <- land_native_pred %>% group_by(ID2) %>% dplyr::summarize(land_native = mean(land_native, na.rm = T))
  
  #combined mod native and land resam back to original frames
  mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/modis_pixels/predictors/all_modis_predictors.csv")
  mod_predict <- mod_native %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(mod_native = mod_predict)
  
  land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/combined/landsat_pixels/predictors/all_landsat_resampled_predictors.csv")
  land_mod_pred <- land_to_modis %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(land_resam = land_mod_pred)
  
  #get instersection of all ID2's
  all_compare <-  plyr::join_all(list(mod_predict, land_mod_pred, land_native_pred), by = 'ID2', type = 'left') %>% drop_na()
  
  
  
  #-------------start bias correction for landsat native to modis native
  
  #regress Modis Native to Land Native, using landsat as Y and modis as X
  linearMod <- lm(land_native ~ mod_native, data=all_compare)
  
  #save the linear model
  saveRDS(linearMod, file.path(out_path, 'linear_model.rds'))
  
  #save the final formuala
  all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$coefficients[1] + (linearMod$coefficients[2] * mod_native))
  
  #type 2 linear Model
  # linearMod <- lmodel2(land_native ~ mod_native, data=all_compare)$regression.results
  # 
  # linearMod <- as_tibble(linearMod[, 1:3]) %>% dplyr::filter(Method == 'SMA')
  # 
  # 
  # #final formulate type2
  # all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$Intercept + (linearMod$Slope * mod_native))
  
  
  write_csv(all_compare, file.path(out_path, 'ranger.csv'))
  
  p1 <- ggplot(all_compare, aes(land_resam, mod_native)) + geom_point(alpha = 0.05) +
    labs(y = 'Modis Native', x = 'Landsat Resample') +
    theme_bw() +
    theme(text=element_text(size=20)) + xlim(limit1, limit2) + ylim(limit1, limit2) +
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
  p2 <- ggplot(all_compare, aes(land_native, mod_native)) + geom_point(alpha = 0.05) +
    labs(y = 'Modis Native', x = 'Landsat Native') +  
    theme_bw() +
    theme(text=element_text(size=20)) + xlim(limit1, limit2) + ylim(limit1, limit2) +
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
  p3 <- ggplot(all_compare, aes(land_native, land_resam,)) + geom_point(alpha = 0.05) + 
    labs(y = 'Landsat Resample', x = 'Landsat Native') +
    theme_bw() +
    theme(text=element_text(size=20)) + xlim(limit1, limit2) + ylim(limit1, limit2) +
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
  
  p4 <- ggplot(all_compare, aes(land_native, Mod_Corrected)) + geom_point(alpha = 0.05) +
    labs(y = 'Modis Corrected', x = 'Landsat Native') +  
    theme_bw() +
    theme(text=element_text(size=20)) + xlim(limit1, limit2) + ylim(limit1, limit2) +
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
  
  #------finalize plots
  final <- plot_grid(p1, p2, p3, p4, ncol = 2) +
    ggsave(filename = file.path(out_path, 'quantile_corrected.png'), device = 'png', dpi = 150, width = 10, height = 10)
  
  # final
  
  #get the residuals
  resid <- as_tibble(linearMod$residuals) %>% dplyr::rename(Residual = value)
  all_compare$Residual <- resid$Residual
  
  #get the residuals for type 2 regression
  all_compare <- all_compare %>% dplyr::mutate(Residual = land_native - Mod_Corrected)
  
  #plot residuals
  #plot the corrections
  p1 <- ggplot(all_compare, aes(land_native, Residual)) + geom_point() +
    labs(y = 'Landsat Native', x = 'Residuals') +
    theme_bw() +
    theme(text=element_text(size=20)) +
    ggsave(filename = file.path(out_path, 'quantile_residals.png'), device = 'png', dpi = 150, width = 10, height = 10)
  
  stopCluster(cl)
  
  return(all_compare)
}


above_f <- scaling(above, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_fwi/full_model_ranger.rds',
                 "/mnt/data1/boreal/spotter/combustion/final_files/scaling/aboveground/no_fwi/combined",
                 '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_fwi/rfe_importance.csv', 
                 'above')

below_f <- scaling(below, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_fwi/full_model_ranger.rds',
                 "/mnt/data1/boreal/spotter/combustion/final_files/scaling/belowground/no_fwi/combined",
                 '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_fwi/rfe_importance.csv', 
                 'below')


above_f <- scaling(above, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_all/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/aboveground/no_all/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_all/rfe_importance.csv', 
                   'above')

below_f <- scaling(below, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_all/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/belowground/no_all/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_all/rfe_importance.csv', 
                   'below')

above_f <- scaling(above, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_age/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/aboveground/no_age/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe_no_age/rfe_importance.csv', 
                   'above')

below_f <- scaling(below, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_age/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/belowground/no_age/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe_no_age/rfe_importance.csv', 
                   'below')


above_f <- scaling(above, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/aboveground/all/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/rfe/rfe_importance.csv',
                   'above')

below_f <- scaling(below, '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe/full_model_ranger.rds',
                   "/mnt/data1/boreal/spotter/combustion/final_files/scaling/belowground/all/combined",
                   '/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/rfe/rfe_importance.csv',
                   'below')

