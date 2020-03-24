#######################this script will translate my python setup to get variables and grid search combustion#####################
library(tidyverse)
library(caret)
library(doSNOW)
library(ranger)
library(parallel)
library(viridis)


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

#get above weights
low <- quantile(above$above.carbon.combusted, 0.10)
high <- quantile(above$above.carbon.combusted, 0.90)
above_weight <- above %>% mutate(Weight = ifelse(above.carbon.combusted < low, 2,
                                                 ifelse( above.carbon.combusted > high, 3, 1)))
above_weight <- above_weight$Weight

#get above weights
low <- quantile(below$below.ground.carbon.combusted, 0.10)
high <- quantile(below$below.ground.carbon.combusted, 0.90)
below_weight <-below %>% mutate(Weight = ifelse(below.ground.carbon.combusted < low, 2,
                                                 ifelse( below.ground.carbon.combusted > high, 3, 1)))
below_weight <- below_weight$Weight


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


#a function to run all my comparisons
model_compare <- function(df, out_path, category, in_weight){
  
  ##arguments are:
  #df = input tibble which has only the x and y variables, very import y is the first column!!
  #out_path = output destination
  ##
  
  #set up cluster - requires doSNOW, and parallel packages
  cores <- detectCores()
  cl <-makeCluster(cores)
  registerDoSNOW(cl)
  
  #create the outpath in case it doesn't exist
  dir.create(out_path, recursive = T)
  
  #an emptry list to store function output
  #item 1 is the tuned models, 2 is the names and 3 is the median R2 comparisons, 4 is all 10 cv 100 repeats and 5 is the plot
  for_output <- list()
  
  #rename the first column (y) to y
  df <- df %>% dplyr::rename(y = names(df)[1])
  
   
  #function to normalize if desired
  normalized <- function(x) (x- min(x))/(max(x) - min(x))
  
  #normaize the predictor variables
  df[, 2:length(colnames(df))] <- lapply(df[, 2:length(colnames(df))], normalized)
  
  #set up the rfe
  set.seed(555)
  control <- rfeControl(functions = rfFuncs,
                        method = "repeatedcv",
                        repeats = 3,
                        verbose = FALSE,
                        allowParallel = T)
  
  #y
  outcomeName<- names(df)[1]
  
  #x
  predictors<-names(df)[!names(df) %in% outcomeName]
  
  #recursive feature elimination
  pred_importance <- rfe(as.matrix(df[,predictors]), as.matrix(df[,outcomeName]),
                         rfeControl = control, sizes = c(1:10, 15, 20, 30, 40, 45, 48))
  
  plot(pred_importance, type=c("g", "o"))
  
  #optimal variables
  opt <- pred_importance$optVariables
  
  #add in the y 
  opt[[length(opt) + 1]] <- "y"
  
  #select the good variables
  df <- df %>% dplyr::select(opt)
  
  #put y as first column again
  df <- df %>% dplyr::select(y, everything())
  
  # #use learning vector quantization to rank importance
  control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
  
  mod <- train(y~., data = df, method = 'rf', trControl = control, importance = T)
  
  importance <- varImp(mod, scale = F)
  # plot(importance)
  
  #make a tibble of importance
  importance <- as.matrix(varImp(mod)$importance)
  importance <- data.frame(importance)
  importance <- as_tibble(tibble::rownames_to_column(importance, "Variables"))
  
  #set the fit control parameter for caret, here is 10 fold cv repeated 3 times
  set.seed(100)
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             savePredictions = 'final',
                             index = createResample(df$y, 25),
                             allowParallel = T)
  
  #get the best model parameters with a random grid search
  model_ranger <- train(y ~., data = df, method = 'ranger', tuneLength = 10, metric = 'RMSE', trControl = fitControl, weights = in_weight)
  # model_svmradial <- train(y ~., data = df, method = 'svmRadial', tuneLength = 10, metric = 'RMSE', trControl = fitControl)
  
  #get estimated model performance with a 10 fold cv repeated 100 times
  set.seed(200)
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 100,
                             savePredictions = 'final',  
                             allowParallel = T)
  
  
  model_ranger_final <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = model_ranger$bestTune$mtry, .splitrule = model_ranger$bestTune$splitrule, .min.node.size = model_ranger$bestTune$min.node.size), trControl = fitControl, weights = in_weight)
  # model_svmradial_final <- train(y ~., data = df, method = 'svmRadial', tuneGrid=data.frame(.sigma = model_svmradial$bestTune$sigma, .C = model_svmradial$bestTune$C), trControl = fitControl)
  
  #get heat map info
  #set the limit based on above or belowground
  limit <- ifelse(category == 'above', 2, 11)
  
  compare_ranger <- as_tibble(model_ranger_final$pred)
  compare_ranger <- compare_ranger %>% mutate(Resids = obs - pred)
  
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
  
  #get density
  compare_ranger$density <- get_density(compare_ranger$obs, compare_ranger$pred, n = 100)
  
  #get density of residuals
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
  
  #model for predictions
  model_ranger_final2 <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = model_ranger$bestTune$mtry, .splitrule = model_ranger$bestTune$splitrule, .min.node.size = model_ranger$bestTune$min.node.size), weights = in_weight)
  
  # ------------------------------------------------------attempt bias correction
  #get full model predictions and rsq
  all_predicteds <- predict(model_ranger_final2, newdata=df %>% select(-y))

  full_rsq = rsq(df$y, all_predicteds)
  
  #set the limit based on above or belowground
  limit <- ifelse(category == 'above', 2, 11)

  #calculate residuals
  res <- df$y - all_predicteds
  
  #a tibble to run the linear model
  for_bias <- tibble(Obs = df$y,
                     Pred = all_predicteds,
                     Resid = res)
  
  # ggplot(for_bias, aes(x = Pred, y = Resid)) + geom_point()
  
  bias_model <- lm(Resid ~ Pred, data = for_bias)

  #predict the residuals
  all_res_predicteds <- predict(bias_model, newdata=for_bias %>% dplyr::select(Pred))

  full_resid_rsq = rsq(for_bias$Obs, all_res_predicteds)
  print(full_resid_rsq)
  
  #plot full model
  p <- ggplot(for_bias, aes(x = Obs, y =  Pred)) + 
    geom_point() +
    labs(x = Observed ~ (kg ~C/m^2), y = Predicted ~ (kg ~C/m^2)) +  
    xlim(0, limit) + ylim(0, limit) + 
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_full_model_ob_pred.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  #get corrected full model
  corrected_full <- predict(bias_model, for_bias %>% dplyr::select(Pred))
  
  #add corrected to for_bias
  for_bias <- for_bias %>% mutate(Corrected = Pred + corrected_full)
  
  #get full corrected r2
  full_corrected_rsq = rsq(for_bias$Obs, for_bias$Corrected)
  
  #plot corrected model
  p <- ggplot(for_bias, aes(x = Obs, y =  Corrected)) + 
    geom_point() +
    labs(x = Observed ~ (kg ~C/m^2), y = Corrected ~ (kg ~C/m^2)) +  
    xlim(0, limit) + ylim(0, limit) + 
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_full_model_corrected.png')), device = 'png', dpi = 150, width = 10, height = 10)
  
  #get corrected cv model
  compare_ranger <- compare_ranger %>% 
    dplyr::rename(Pred = pred, Obs = obs) %>%
    mutate(Resid = Obs - Pred)
  
  ggplot(compare_ranger, aes(x = Pred, y = Resid)) + geom_point()
  
  bias_cv_model <- lm(Resid ~ Pred, data = compare_ranger)
  
  #predict the residuals
  corrected_cv <- predict(bias_cv_model, compare_ranger %>% dplyr::select(Pred))
  
  #add corrected to for_bias
  compare_ranger <- compare_ranger %>% mutate(Corrected = Pred + corrected_cv)
  
  #get full corrected r2
  full_corrected_rsq = rsq(compare_ranger$Obs, compare_ranger$Corrected)
  
  #get density of residuals
  compare_ranger$density2 <- get_density(compare_ranger$Obs, compare_ranger$Corrected, n = 100)
  
  compare_ranger <- compare_ranger %>% mutate(Corrected_R2 = full_corrected_rsq)
  write_csv(compare_ranger, file.path(out_path, paste0(category, '_ranger_heat_map_corrected.csv')))
            
  p <- ggplot(compare_ranger)  +
    geom_point(aes(Obs, Corrected, color = density2)) + scale_color_viridis() +
    labs(x = Observed ~ (kg ~C/m^2), y = Corrected ~ (kg ~C/m^2)) +  
    xlim(0, limit) + ylim(0, limit) + 
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed', size = 0.6) +
    theme_bw() +
    theme(text=element_text(size=18)) + 
    ggsave(filename = file.path(out_path,  paste0(category, '_ranger_heat_map_corrected.png')), device = 'png', dpi = 150, width = 10, height = 10)

  
  stopCluster(cl)
  
 
  return(for_output)
}

above_stand <- model_compare(above, "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/rfe_linear_bias", 'above', above_weight)
below_stand <- model_compare(below, "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/rfe_linear_bias", 'below', below_weight)
# 





