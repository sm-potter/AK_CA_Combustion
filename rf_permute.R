#######################this script will translate my python setup to get variables and grid search combustion#####################
library(tidyverse)
library(caret)
library(doSNOW)
library(ranger)
library(rfPermute)
library(parallel)

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
sig_vars <- function(df, grid_model, out_path){
  
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
  
  #read in the appropriate ranger file from the model_training file
  train_model <- readRDS(grid_model)
  #ranger is the first element
  train_model <- train_model[[1]]
  
  #rf permute
  permute <- rfPermute(y ~.,data = df, nrep = 100, num.cores =   cores <- detectCores(),
                       mtry = train_model$bestTune$mtry)
  
  #get the importance
  importance <- rp.importance(permute, scale = TRUE, sort.by = NULL, decreasing = TRUE)
  # plot(importance, alpha = 0.05, sig.only = TRUE)
  
  #save the variables with p <= 0.05 and %IncMSE
  importance <- data.frame(importance)
  importance <- as_tibble(tibble::rownames_to_column(importance, "Variables"))
  importance <- importance %>% filter(X.IncMSE.pval <= 0.05)
  
  write_csv(importance, file.path(out_path, 'permute_importance.csv'))
  
  #run a 10 fold cv 100 times using the new variables to get a new measure of model performance
  set.seed(200)

  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 100,
                             savePredictions = 'final',
                             allowParallel = T)

  # -----for feature elimination
  model_ranger_final <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry =   train_model$bestTune$mtry, .splitrule = 'extratrees', .min.node.size = 5), trControl = fitControl)

  #save all the r2
  all_r2 <- model_ranger_final$resample$Rsquared
  med <- median(all_r2, na.rm = T)
  all_r22 <- tibble(R2 = all_r2,
                    Median = med)
  
  write_csv(all_r22, file.path(out_path, 'all_r2.csv'))
  for_output[1] <- importance
  return(for_output)
}

s_age_above <- sig_vars(above,
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/no_sig/ranger.rds",
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/sig")
s_age_below <- sig_vars(below,
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/no_sig/ranger.rds",
                 "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/sig")

ns_age_above <- sig_vars(above %>% dplyr::select(-stand.age),
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/no_stand_age/no_sig/ranger.rds",
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/no_stand_age/sig")

ns_age_below<- sig_vars(below %>% dplyr::select(-stand.age),
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/no_stand_age/no_sig/ranger.rds",
                  "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/no_stand_age/sig")



