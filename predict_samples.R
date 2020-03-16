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

# set.seed(54321) #this is really good for aboveground c
# set.seed(54351) #this one is worse for both #for new perma this one is worse below same above
# set.seed(100) #ok for below good for above
set.seed(1896) #good for both and old perma


#original file
# df <-  read.csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_SynthesisData_05042018_XJW.csv')
# df <-  read.csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_051019_DOB_SMP.csv')
# df <-  read.csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_051019_4day_SMP.csv')
# df <-  read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_053019_DOB_TWI_SMP.csv')
# df <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_061819_DOB_TWI_VI_SMP.csv')
# df <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_062019_DOB_TWI_chavez_VI_SMP.csv')
# df <- read_csv("/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_081419_DOB_TWI_chavez_VI_all_burns_SMP.csv")
# df <- read_csv( '/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_070919_DOB_corrected_TWI_chavez_VI_SMP.csv')
# df <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_070919_DOB_TWI_chavez_VI_obu_SMP.csv')
# df <- read_csv("/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_092319_DOB_TWI_chavez_VI_all_burns_30m_SMP.csv")
df <- read_csv("/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_092419_DOB_TWI_ee_VI_all_burns_30m_SMP.csv")
df <- read_csv( "/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_022019_DOB_TWI_ee_VI_all_burns_30m_bea_age_SMP.csv")

#make sure the exact ecoregions are all dplyr::selected
# df <- df %>% filter(id %in% unique(comb$id))
df <- df %>% filter(!treatment %in% c("Control"))
# 
#swap twi with CDEM/NED
# df2 <-  read.csv("/mnt/data1/boreal/spotter/combustion/twi_swap/twi_swapped.csv")
# df2 <- df2 %>% dplyr::select(id, TWI)
# 
# df <- df %>% filter(id %in% unique(df2$id))
# 
# df <- left_join(df, df2, by = c("id")) %>% dplyr::select(-TWI.x) %>% dplyr::rename(TWI = TWI.y)

#swap hanson tc to sexton
df2 <-  read_csv("/mnt/data1/boreal/spotter/combustion/stand_age/swapped_tc.csv")
df2 <- df2 %>% dplyr::select(id, Tree_cover)
names(df2) <- c('id', 'Tree.cover')
df <- df %>% filter(id %in% unique(df2$id))

df <- df %>% dplyr::select(-Tree.cover)
df <- left_join(df, df2, by = "id")

#remove and dplyr::rename some columns
df <- df %>% dplyr::select (-longitude.y, -slope.x, -aspect.x, -elevation.x) %>% dplyr::rename(slope = slope.y) %>%
       dplyr::rename(longitude = longitude.x) %>% dplyr::rename(aspect = aspect.y) %>% dplyr::rename(elevation = elevation.y)

df <- df %>% dplyr::rename(DMC = DMC.y) %>% dplyr::rename(BUI = BUI.y) %>% dplyr::rename(ISI = ISI.y) %>%
       dplyr::rename(DC = DC.y) %>% dplyr::rename(FFMC = FFMC.y) %>% dplyr::rename(FWI = FWI.y)

#shuffle the dataframe
df <- df[sample(1:nrow(df)), ]

#master column list which will contain all my possible predictors
master_cols <- c('above.carbon.combusted', 'below.ground.carbon.combusted', 'stand.age',
                 'dNBR', 'Tree.cover', 'elevation', 'slope', 'aspect', 'TWI', 
                 'PFI', 'Ruggedness', 'pH_30', 'GRSH', 'DEC', 'BS',
                 'Sand_30', 'Silt_30', 'Clay_30', 'SOC_30', 'NV', 'WS', 'JP',
                 'OCON', 'DOB_lst', 'Temperature', 'Relative.humidity',
                 'Wind.speed', 'DC', 'ISI','BUI', 'DMC', 'DSR', 'FFMC', 'FWI','CNA_PPT_5_8',
                 'CNA_Tmax_5_8', 'CNA_MAT', 'CNA_MAP', 'CNA_CMD', 'CNA_PAS', 
                 'CNA_NFFD', 'CNA_TD', 'VPD', 'project.name', 'id', 'VPD', 'rbr', 'rdnbr', 'greenness', 'brightness', 'wetness',  
                 'NDVI', 'NDII') #'dNIR', 'dRSWIR','postRSWIR',

#'dNIR', 'dRSWIR','postRSWIR', 'greenness', 'brightness', 'wetness',  'NDVI', 'NDII'

#aboveground
above <- df %>% dplyr::select(master_cols, -below.ground.carbon.combusted) %>% drop_na()
above$above.carbon.combusted <- above$above.carbon.combusted / 1000.0

#belowground
below <- df %>% dplyr::select(master_cols, -above.carbon.combusted) %>% drop_na() 
below$below.ground.carbon.combusted <- below$below.ground.carbon.combusted / 1000.0

#function to normalize if desired
normalized <- function(x) (x- min(x))/(max(x) - min(x))

#function to inverse normalize
inv_norm <- function(original, normalized) normalized*(max(original) - min(original)) + min(original)


#get all model names in careyt
modelnames <- paste(names(getModelInfo()), collapse = ', ')
print(modelnames)

#look up parameters which can be tuned
# modelLookup('xgbTree') #gradient boosted
# modelLookup('ranger') #ranger rf
# modelLookup('rf') #standard rf
# modelLookup('qrf') #quantile rf
# modelLookup('lasso') #lasso regression
# modelLookup('ridge') #ridge
# modelLookup('earth') #MARS
# modelLookup('bam') #GAM
# modelLookup('gamboost') #boosted GAM
# modelLookup('svmLinear2') #svm linear
# modelLookup('svmPoly') #svm poly
# modelLookup('svmRadial') #svm radial


# stopCluster(cl)
cores <- detectCores() - 3
cl <-makeCluster(cores)
registerDoSNOW(cl)

#---------------------------------------------------------------above_ground---------------------------------------

for_train = above
indexes <- createDataPartition(for_train$above.carbon.combusted, times = 1, p = 0.8, list = FALSE)

train <- for_train[indexes,]
test <- for_train[-indexes,]

df_raw = test

train <- train %>% dplyr::select(-project.name, -id)
test <- test %>% dplyr::select(-project.name, -id)

#normalize variables
train[, 2:length(colnames(train))] <- lapply(train[, 2:length(colnames(train))], normalized)
test[, 2:length(colnames(test))] <- lapply(test[, 2:length(colnames(test))], normalized)

#for full model R2
for_full <- for_train %>% dplyr::select(-project.name, -id)
for_full[, 2:length(colnames(for_full))] <- lapply(for_full[, 2:length(colnames(for_full))], normalized)

#without stand age
# model_quantile_final <- train(above.carbon.combusted ~., data = for_full, method = 'qrf', tuneGrid=data.frame(.mtry = 22))
# model_quantile_final <- train(above.carbon.combusted ~., data = for_full, method = 'svmRadial', tuneGrid=data.frame(.sigma = 0.0152802, .C = 2))
# model_quantile_final <- train(above.carbon.combusted~., data = for_full, method = 'svmPoly', tuneGrid=data.frame(.degree= 3, .scale = 0.01, .C = 0.5))
# model_quantile_final <- train(above.carbon.combusted ~., data = for_full, method = 'ridge', tuneGrid=data.frame(.lambda = 0.04216965))
model_quantile_final <- train(above.carbon.combusted ~., data = for_full, method = 'ranger', tuneGrid=data.frame(.mtry = 27, .splitrule = 'extratrees', .min.node.size = 5))

#with stand age
model_quantile_final <- train(above.carbon.combusted ~., data = for_full, method = 'ranger', tuneGrid=data.frame(.mtry = 43, .splitrule = 'extratrees', .min.node.size = 5))


c <- predict(model_quantile_final, test)
temp <- tibble(Observed = test$above.carbon.combusted, Predicted = c)
c_p<- ggplot(temp, aes(Observed, Predicted)) + geom_point() +
  labs(x = 'Observed', y = 'Predicted') +
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 2) + ylim(0, 2)
print(c_p)

#read in the modis native to predict on
mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/all_modis_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
#normalize variables
mod_native[, 1:length(colnames(mod_native))] <- lapply(mod_native[, 1:length(colnames(mod_native))], normalized)


#read in the landsat resampled to predict on
land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
land_to_modis[, 1:length(colnames(land_to_modis))] <- lapply(land_to_modis[, 1:length(colnames(land_to_modis))], normalized)

#read in the landsat native to predict on
land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% dplyr::select(-ID2, -land_ID1) %>% drop_na()
land_native[, 1:length(colnames(land_native))] <- lapply(land_native[, 1:length(colnames(land_native))], normalized)

#predict modis_native
mod_predict <- predict(model_quantile_final, newdata=mod_native)

#predict land_to_modis
land_mod_pred <- predict(model_quantile_final, newdata=land_to_modis)

#predict land native
land_native_pred <- predict(model_quantile_final, newdata=land_native)

land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_native_predictors.csv")
#combined land_native_pred back to original frame and take mean prediction by ID2 (which is what matches modis original)

land_native_pred <- land_native %>% drop_na() %>% dplyr::select(ID2, land_ID1) %>% dplyr::mutate(land_native = land_native_pred)

#groupby and mean by ID2
land_native_pred <- land_native_pred %>% group_by(ID2) %>% dplyr::summarize(land_native = mean(land_native, na.rm = T))

#combine mod native and land resam back to original frames
mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/all_modis_predictors.csv")
mod_predict <- mod_native %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(mod_native = mod_predict)

land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_resampled_predictors.csv")
land_mod_pred <- land_to_modis %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(land_resam = land_mod_pred)

#get instersection of all ID2's
all_compare <-  plyr::join_all(list(mod_predict, land_mod_pred, land_native_pred), by = 'ID2', type = 'left') %>% drop_na()

out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/final_predictions/original"
dir.create(out, recursive = T)
write_csv(all_compare, file.path(out, 'quantile_aboveground.csv'))

p1 <- ggplot(all_compare, aes(land_resam, mod_native)) + geom_point(alpha = 0.3) +
                                                         labs(y = 'Modis Native', x = 'Landsat Resample') +
                                                         theme_bw() +
                                                         theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
                                                         geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
p2 <- ggplot(all_compare, aes(land_native, mod_native)) + geom_point(alpha = 0.3) +
                                                          labs(y = 'Modis Native', x = 'Landsat Native') +  
                                                          theme_bw() +
                                                          theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
                                                          geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 
p3 <- ggplot(all_compare, aes(land_native, land_resam,)) + geom_point(alpha = 0.3) + 
                                                          labs(y = 'Landsat Resample', x = 'Landsat Native') +
                                                          theme_bw() +
                                                          theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
                                                          geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

#------finalize plots
final <- plot_grid(p1, p2, p3, ncol = 2) +
  ggsave(filename = file.path(out, 'quantile_above_scatter.png'), device = 'png', dpi = 150, width = 10, height = 10)

final

#-------------start bias correction for landsat native to modis native

#regress Modis Native to Land Native, using landsat as Y and modis as X
# linearMod <- lm(land_native ~ 1 + mod_native + I(mod_native^2), data=all_compare)

#save the final formuala
# all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$coefficients[1] + (linearMod$coefficients[2] * mod_native))


#type 2 linear Model
linearMod <- lmodel2(land_native ~ mod_native, data=all_compare)$regression.results

linearMod <- as_tibble(linearMod[, 1:3]) %>% dplyr::filter(Method == 'SMA')


#final formulate type2
all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$Intercept + (linearMod$Slope * mod_native))



# #plot the corrections
# p1 <- ggplot(all_compare, aes(land_native, mod_native)) + geom_point(alpha = 0.3) +
#   labs(y = 'Modis Native', x = 'Landsat Native') +
#   theme_bw() +
#   theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
#   geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

p4 <- ggplot(all_compare, aes(land_native, Mod_Corrected)) + geom_point(alpha = 0.3) +
  labs(y = 'Modis Corrected', x = 'Landsat Native') +  
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

#------finalize plots
final <- plot_grid(p1, p2, p3, p4, ncol = 2) +
  ggsave(filename = file.path(out, 'quantile_above_corrected.png'), device = 'png', dpi = 150, width = 10, height = 10)

final

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
  ggsave(filename = file.path(out, 'quantile_above_residals_t2.png'), device = 'png', dpi = 150, width = 10, height = 10)

p1



#---------------------------------------------------------------below_ground---------------------------------------

for_train = below
indexes <- createDataPartition(for_train$below.ground.carbon.combusted, times = 1, p = 0.8, list = FALSE)

train <- for_train[indexes,]
test <- for_train[-indexes,]

df_raw = test

train <- train %>% dplyr::select(-project.name, -id)
test <- test %>% dplyr::select(-project.name, -id)

#nomalize variables if you wish (0.13 normalized only predictors, non-normalized 0.17, with dob) #feature selection
#nomalize variables if you wish (0.17 normalized only predictors, non-normalized 0.17, with dob) #all_predictors selection

#nomalize variables if you wish (0.13 normalized only predictors, non-normalized 0.13, with 4day) #all_predictors selection

train[, 2:length(colnames(train))] <- lapply(train[, 2:length(colnames(train))], normalized)
test[, 2:length(colnames(test))] <- lapply(test[, 2:length(colnames(test))], normalized)

#for full model R2
for_full <- for_train %>% dplyr::select(-project.name, -id)
for_full[, 2:length(colnames(for_full))] <- lapply(for_full[, 2:length(colnames(for_full))], normalized)

#no stand age
# model_quantile_final <- train(below.ground.carbon.combusted ~., data = for_full, method = 'qrf', tuneGrid=data.frame(.mtry = 7))
# model_quantile_final <- train(below.ground.carbon.combusted ~., data = for_full, method = 'svmRadial', tuneGrid=data.frame(.sigma = 0.01475131, .C = 2))
# model_quantile_final <- train(below.ground.carbon.combusted~., data = for_full, method = 'svmPoly', tuneGrid=data.frame(.degree= 3, .scale = 0.01, .C = 0.5))
model_quantile_final <- train(below.ground.carbon.combusted ~., data = for_full, method = 'ranger', tuneGrid=data.frame(.mtry = 2, .splitrule = 'extratrees', .min.node.size = 5))

model_quantile_final <- train(below.ground.carbon.combusted ~., data = for_full, method = 'ranger', tuneGrid=data.frame(.mtry = 17, .splitrule = 'extratrees', .min.node.size = 5))


c <- predict(model_quantile_final, test)
temp <- tibble(Observed = test$below.ground.carbon.combusted, Predicted = c)
c_p<- ggplot(temp, aes(Observed, Predicted)) + geom_point() +
  labs(x = 'Observed', y = 'Predicted') +
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 6) + ylim(0, 6)
print(c_p)

#read in the modis native to predict on
mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/all_modis_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
#normalize variables
mod_native[, 1:length(colnames(mod_native))] <- lapply(mod_native[, 1:length(colnames(mod_native))], normalized)


#read in the landsat resampled to predict on
land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_resampled_predictors.csv") %>% dplyr::select(-ID2) %>% drop_na()
land_to_modis[, 1:length(colnames(land_to_modis))] <- lapply(land_to_modis[, 1:length(colnames(land_to_modis))], normalized)

#read in the landsat native to predict on
land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_native_predictors.csv") %>% dplyr::select(-ID2, -land_ID1) %>% drop_na()
land_native[, 1:length(colnames(land_native))] <- lapply(land_native[, 1:length(colnames(land_native))], normalized)

#predict modis_native
mod_predict <- predict(model_quantile_final, newdata=mod_native)

#predict land_to_modis
land_mod_pred <- predict(model_quantile_final, newdata=land_to_modis)

#predict land native
land_native_pred <- predict(model_quantile_final, newdata=land_native)

land_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_native_predictors.csv")
#combined land_native_pred back to original frame and take mean prediction by ID2 (which is what matches modis original)

land_native_pred <- land_native %>% drop_na() %>% dplyr::select(ID2, land_ID1) %>% dplyr::mutate(land_native = land_native_pred)

#groupby and mean by ID2
land_native_pred <- land_native_pred %>% group_by(ID2) %>% dplyr::summarize(land_native = mean(land_native, na.rm = T))

#combine mod native and land resam back to original frames
mod_native <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/modis_pixels/predictors/all_modis_predictors.csv")
mod_predict <- mod_native %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(mod_native = mod_predict)

land_to_modis <- read_csv("/mnt/data1/boreal/spotter/combustion/burned_area/scaling/landsat_pixels/predictors/all_landsat_resampled_predictors.csv")
land_mod_pred <- land_to_modis %>% drop_na() %>% dplyr::select(ID2) %>% dplyr::mutate(land_resam = land_mod_pred)

#get instersection of all ID2's
all_compare <-  plyr::join_all(list(mod_predict, land_mod_pred, land_native_pred), by = 'ID2', type = 'left') %>% drop_na()

out <- "/mnt/data1/boreal/spotter/combustion/burned_area/scaling/final_predictions/original"
dir.create(out, recursive = T)
write_csv(all_compare, file.path(out, 'quantile_belowground.csv'))

p1 <- ggplot(all_compare, aes(land_resam, mod_native)) + geom_point(alpha = 0.3) +
  labs(y = 'Modis Native', x = 'Landsat Resample') +
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 4) + ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

p2 <- ggplot(all_compare, aes(land_native, mod_native)) + geom_point(alpha = 0.3) +
  labs(y = 'Modis Native', x = 'Landsat Native') +  
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 4) + ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

p3 <- ggplot(all_compare, aes(land_native, land_resam)) + geom_point(alpha = 0.3) + 
  labs(y = 'Landsat Resample', x = 'Landsat Native') +
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 4) + ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

#------finalize plots
final <- plot_grid(p1, p2, p3, ncol = 2) +
  ggsave(filename = file.path(out, 'quantile_below_scatter.png'), device = 'png', dpi = 150, width = 10, height = 10)

final


#-------------start bias correction for landsat native to modis native

#regress Modis Native to Land Native, using landsat as Y and modis as X
# linearMod <- lm(land_native ~ 1 + mod_native + I(mod_native^2), data=all_compare)

#save the final formuala
# all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$coefficients[1] + (linearMod$coefficients[2] * mod_native))


#type 2 linear Model
linearMod <- lmodel2(land_native ~ mod_native, data=all_compare)$regression.results

linearMod <- as_tibble(linearMod[, 1:3]) %>% dplyr::filter(Method == 'SMA')


#final formulate type2
all_compare <- all_compare %>% dplyr::mutate(Mod_Corrected = linearMod$Intercept + (linearMod$Slope * mod_native))



# #plot the corrections
# p1 <- ggplot(all_compare, aes(land_native, mod_native)) + geom_point(alpha = 0.3) +
#   labs(y = 'Modis Native', x = 'Landsat Native') +
#   theme_bw() +
#   theme(text=element_text(size=20)) + xlim(0, 0.75) + ylim(0, 0.75) +
#   geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

p4 <- ggplot(all_compare, aes(land_native, Mod_Corrected)) + geom_point(alpha = 0.3) +
  labs(y = 'Modis Corrected', x = 'Landsat Native') +  
  theme_bw() +
  theme(text=element_text(size=20)) + xlim(0, 4) + ylim(0, 4) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) 

#------finalize plots
final <- plot_grid(p1, p2, p3, p4, ncol = 2) +
  ggsave(filename = file.path(out, 'quantile_below_corrected.png'), device = 'png', dpi = 150, width = 10, height = 10)

final

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
  ggsave(filename = file.path(out, 'quantile_below_residals_t2.png'), device = 'png', dpi = 150, width = 10, height = 10)

p1
