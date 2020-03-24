#######################this script will translate my python setup to get variables and grid search combustion#####################
library(tidyverse)
library(caret)
library(doSNOW)
library(ranger)
library(rfPermute)
library(parallel)
library(viridis)

#set a random seed
set.seed(1896) 


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

#function to normalize if desired
normalized <- function(x) (x- min(x))/(max(x) - min(x))

#function to inverse normalize
inv_norm <- function(original, normalized) normalized*(max(original) - min(original)) + min(original)

#function for R2
rsq <- function (x, y) cor(x, y) ^ 2

#function for RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#set up cluster
# cores <- detectCores() - 3
# cl <-makeCluster(cores)
# registerDoSNOW(cl)


#get all model names in caret
modelnames <- paste(names(getModelInfo()), collapse = ', ')

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
# modelLookup('gam') #svm radial


# stopCluster(cl)

#create a function to run the permutations
sig_vars <- function(df, grid_model, out_path, category){
  
  ##arguments are:
  #df = input tibble which has only the x and y variables, very import y is the first column!!
  #out_path = output destination
  #grid_model comes from model_training.R and is the best tuned paramaters file pathway
  ##
  
  #register parallel processing, requires parallel and doSNOW packages
  cores <- detectCores() 
  cl <-makeCluster(cores)
  registerDoSNOW(cl)
  
  #create the outpath if it does not exist
  dir.create(out_path, recursive = T)
  
  #an emptry list to store function output
  for_output <- list()
  
  #rename the first column (y) to y
  df <- df %>% dplyr::rename(y = names(df)[1])
  
  df[, 2:length(colnames(df))] <- lapply(df[, 2:length(colnames(df))], normalized)
  
  #set the fit control parameter for caret, here is 10 fold cv repeated 3 times
  set.seed(100)
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             savePredictions = 'final',
                             index = createResample(df$y, 25),
                             allowParallel = T)
  
  #get the best model parameters with a random grid search
  model_ranger <- train(y ~., data = df, method = 'ranger', tuneLength = 10, metric = 'RMSE', trControl = fitControl)

  #save a list of tuned model comparisons
  #save all the models to a list
  pre_final <- list(model_ranger)
  
  #read in the appropriate ranger file from the model_training file
  train_model <- pre_final[1]
  
  #ranger is the first element
  train_model <- train_model[[1]]
  
  #rf permute
  permute <- rfPermute(y ~.,data = df, nrep = 100, mtry = train_model$bestTune$mtry)
  
  #get the importance
  importance <- rp.importance(permute, scale = TRUE, sort.by = NULL, decreasing = TRUE)
  # plot(importance, alpha = 0.05, sig.only = TRUE)
  
  #save the variables with p <= 0.05 and %IncMSE
  importance <- data.frame(importance)
  importance <- as_tibble(tibble::rownames_to_column(importance, "Variables"))
  importance <- importance %>% filter(X.IncMSE.pval <= 0.05)
  
  write_csv(importance, file.path(out_path, 'permute_importance.csv'))
  
  df <- df %>% dplyr::select(y, importance$Variables)
  #run a 10 fold cv 100 times using the new variables to get a new measure of model performance
  set.seed(200)

  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             savePredictions = 'final',
                             index = createResample(df$y, 25),
                             allowParallel = T)

  # run new parameter tuning
  model_ranger_param <- train(y ~., data = df, method = 'ranger', tuneLength = 10, trControl = fitControl)
  
  #get estimated model performance with a 10 fold cv repeated 100 times
  set.seed(200)
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 100,
                             savePredictions = 'final',  
                             allowParallel = T)
  
  model_ranger_final <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = model_ranger_param$bestTune$mtry, .splitrule = model_ranger_param$bestTune$splitrule, .min.node.size = model_ranger_param$bestTune$min.node.size), trControl = fitControl)
  
  #set the limit based on above or belowground
  limit <- ifelse(category == 'above', 2, 11)

  compare_ranger <- as_tibble(model_ranger_final$pred)
  
  # Get density of points in 2 dimensions.
  # @param x A numeric vector.
  # @param y A numeric vector.
  # @param n Create a square n by n grid to compute density.
  # @return The density within each square.
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  compare_ranger$density <- get_density(compare_ranger$obs, compare_ranger$pred, n = 100)
  compare_ranger$density2 <- get_density(compare_ranger$obs, compare_ranger$Resids, n = 100)
  
  #save the ranger plot
  p <- ggplot(compare_ranger)  +
    geom_point(aes(obs, pred, color = density)) + scale_color_viridis() +
    labs(x = Observed ~ (kg ~C/m^2), y = Predicted ~ (kg ~C/m^2)) +  
    xlim(0, limit) + ylim(0, limit) + 
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_ranger_heat_map.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  #save the residual plots
  p3 <- ggplot(compare_ranger)  +
    geom_point(aes(obs, Resids, color = density2)) + scale_color_viridis() +
    labs(x = Observed ~ (kg ~C/m^2), y = Residual ~ (kg ~C/m^2)) +  
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_ranger_residuals.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  #get the quantiles
  quantile_ranger <- compare_ranger %>% mutate(Quartiles = ntile(pred, 10))
  
  # #get the mean Residual per quartile
  mean_ranger_quantile_resid <- quantile_ranger %>% group_by(Quartiles) %>% dplyr::summarize(Resids = sd(Resids), Preds = max(pred))
  
  #get the loess function
  ranger_loess <- loess(Resids ~ Quartiles, data = mean_ranger_quantile_resid)
  ranger_loess <- predict(ranger_loess, mean_ranger_quantile_resid %>% dplyr::select(Quartiles))
  mean_ranger_quantile_resid <- mean_ranger_quantile_resid %>% mutate(Loess = ranger_loess)
  write_csv(mean_ranger_quantile_resid, file.path(out_path, paste0(category,'_ranger_resid_smoothed.csv')))
  
  bins <- ggplot(mean_ranger_quantile_resid, aes(x = Quartiles, y = Resids)) + 
    geom_line() + 
    geom_point() + 
    geom_smooth(method = 'loess') + 
    ylab('SD of Residuals') +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_ranger_smoothed.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  
  #save all the r2
  all_r2 <- model_ranger_final$resample$Rsquared
  med <- median(all_r2, na.rm = T)
  all_r22 <- tibble(R2 = all_r2,
                    Median = med)
  
  write_csv(all_r22, file.path(out_path, 'all_r2.csv'))
  
  final_ranger <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = model_ranger_param$bestTune$mtry, .splitrule = model_ranger_param$bestTune$splitrule, .min.node.size = model_ranger_param$bestTune$min.node.size))
  
  #save the final model
  saveRDS(final_ranger, file=file.path(out_path, 'full_model_ranger.rds'))
  
  #get full model predictions and rsq
  all_ranger_predicteds <- predict(final_ranger, newdata=df %>% select(-y))

  full_ranger_rsq = rsq(df$y, all_ranger_predicteds)

  #get obs and preds
  ranger_compare <- tibble(Obs = df$y, 
                           Pred = all_ranger_predicteds)
 
  #plot the full model
  p <- ggplot(ranger_compare, aes(x = Obs, y =  Pred)) + 
    geom_point() +
    labs(x = Observed ~ (kg ~C/m^2), y = Predicted ~ (kg ~C/m^2)) +  
    xlim(0, limit) + ylim(0, limit) + 
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_ranger_full_model_ob_pred.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  for_output[1] <- importance

  return(for_output)
  stopCluster(cl)
  
}


s_age_above <- sig_vars(above,
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/no_sig/ranger.rds",
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/sig", 'above')

s_age_below <- sig_vars(below,
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/no_sig/ranger.rds",
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/sig", 'below')

ns_age_above <- sig_vars(above %>% dplyr::select(-stand.age),
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/no_stand_age/no_sig/ranger.rds",
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/no_stand_age/sig", 'above')

ns_age_below<- sig_vars(below %>% dplyr::select(-stand.age),
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/no_stand_age/no_sig/ranger.rds",
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/no_stand_age/sig", 'below')



