#######################this script will translate my python setup to get variables and grid search combustion#####################
library(tidyverse)
library(caret)
library(doSNOW)
library(ranger)
library(parallel)


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


#a function to run all my comparisons
model_compare <- function(df, out_path){
  
  ##arguments are:
  #df = input tibble which has only the x and y variables, very import y is the first column!!
  #out_path = output destination
  ##
  
  #set up cluster - requires doSNOW, and parallel packages
  cores <- detectCores() - 3 #reduce max cores by 3
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
  model_quantile <- train(y ~., data = df, method = 'qrf', tuneLength = 10, metric = 'RMSE', trControl = fitControl)
  model_lasso <- train(y ~., data = df, method = 'lasso', tuneLength = 10, metric = 'RMSE', trControl = fitControl)
  model_ridge <- train(y ~., data = df, method = 'ridge', tuneLength = 10, metric = 'RMSE', trControl = fitControl)
  # model_gam <- train(y ~., data = df, method = 'bam',  tuneLength = 4, metric = 'RMSE', trControl = fitControl)
  model_svmlinear <- train(y ~., data = df, method = 'svmLinear2', tuneLength = 4, metric = 'RMSE', trControl = fitControl)
  model_svmpoly <- train(y~., data = df, method = 'svmPoly', tuneLength = 4, metric = 'RMSE', trControl = fitControl)
  model_svmradial <- train(y ~., data = df, method = 'svmRadial', tuneLength = 4, metric = 'RMSE', trControl = fitControl)
  
  #save a list of tuned model comparisons
  #save all the models to a list
  pre_final <- list(model_ranger, model_quantile, model_lasso, model_ridge,
                    model_svmlinear, 
                    model_svmpoly, model_svmradial)
  
  for_output[1] <- pre_final
  
  #get estimated model performance with a 10 fold cv repeated 100 times
  set.seed(200)
  
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 100,
                             savePredictions = 'final',  
                             allowParallel = T)
  
  #run the cv
  model_ranger_final <- train(y ~., data = df, method = 'ranger', tuneGrid=data.frame(.mtry = model_ranger$bestTune$mtry, .splitrule = 'extratrees', .min.node.size = 5), trControl = fitControl)
  model_quantile_final <- train(y ~., data = df, method = 'qrf', tuneGrid=data.frame(.mtry = model_quantile$bestTune$mtry), trControl = fitControl)
  model_lasso_final <- train(y ~., data = df, method = 'lasso', tuneGrid=data.frame(.fraction = model_lasso$bestTune$fraction), trControl = fitControl)
  model_ridge_final <- train(y ~., data = df, method = 'ridge', tuneGrid=data.frame(.lambda = model_ridge$bestTune$lambda), trControl = fitControl)
  # model_gam_final <- train(y ~., data = df, method = 'gamboost', tuneGrid=data.frame(.mstop = 450, .prune = 'no'), trControl = fitControl)
  model_svmlinear_final <- train(y ~., data = df, method = 'svmLinear2', tuneGrid=data.frame(.cost = model_svmlinear$bestTune$cost), trControl = fitControl)
  model_svmpoly_final <- train(y~., data = df, method = 'svmPoly', tuneGrid=data.frame(.degree= model_svmpoly$bestTune$degree, .scale = model_svmpoly$bestTune$scale, .C = model_svmpoly$bestTune$C), trControl = fitControl)
  model_svmradial_final <- train(y ~., data = df, method = 'svmRadial', tuneGrid=data.frame(.sigma = model_svmradial$bestTune$sigma, .C = model_svmradial$bestTune$C), trControl = fitControl)
  
  #save all the models to a list
  all_final <- list(model_ranger_final, model_quantile_final, model_lasso_final, model_ridge_final,
                    model_svmlinear_final, 
                    model_svmpoly_final, model_svmradial_final)
  
  #a list of corresponsing names
  all_names <- list('ranger', 'quantile', 'lasso', 'ridge', 'svmlinear', 'svmpoly', 'svmradial')
  for_output[2] <- all_names
  
  #---get the vectors of all predictions as one df
  final_df <- list()
  
  for (model in 1:length(all_final)){
    in_model <- all_final[[model]]
    in_name <- all_names[[model]]
    
    final <- tibble(Model = in_name,
                    Values = in_model$resample$Rsquared)
    
    final_df[[length(final_df) + 1]] <- final
  }
  final_df = bind_rows(final_df)
  
  #rearange high to low
  order <- final_df %>% drop_na() %>% group_by(Model) %>% dplyr::summarize(Value = median(Values)) %>% arrange(desc(Value))
  
  order <- order %>% mutate(Model = as.factor(Model))
  
  #order the levels by descending R2 values
  order$Model <- reorder(order$Model, order$Value)
  levels <- levels(order$Model)
  
  #save to item one in output
  for_output[3] <- order
  
  # final_df <- final_df %>% mutate(Model = as.factor(Model)) 
  final_df <- final_df %>% mutate(Model = factor(Model, levels = levels))
  
  #save to item one in output
  for_output[4] <- final_df
  
  #save the final csv
  write_csv(final_df, file.path(out_path, 'repeated_cv.csv'))
  write_csv(order, file.path(out_path, 'median_r2_compare.csv'))
  
  #save the box and violin plot
  dodge <- position_dodge(width = 0.8)
  p <- ggplot(final_df, aes(Model, Values)) +
    # scale_color_brewer(palette = 'Set2') +
    # scale_color_manual(values = c('gray', 'gray', 'gray', 'gray', 'gray'))+
    geom_violin(position = dodge, fill = '#A4A4A4') +
    geom_boxplot(width=.15, outlier.colour=NA, position = dodge) +
    coord_flip() +
    theme_classic() +
    xlab('') + ylab(expression(R^{2})) +
    theme(legend.position="none") +
    theme(text=element_text(size=24)) +
    ggsave(filename = file.path(out_path, 'violin_plot.png'), device = 'png', dpi = 150, width = 10, height = 10)
  
  #save the best tuned model, second best, and third best
  #get the index of the best model
  index <- as.character(order$Model)[1]

  #find where in all_names this lies
  all_names_index <- match(index, all_names)

  #get the tuned final model
  selected_model <- pre_final[all_names_index]
  
  saveRDS(pre_final, file=file.path(out_path, paste0(index, '.rds')))
  
  #second best
  index <- as.character(order$Model)[2]
  
  #find where in all_names this lies
  all_names_index <- match(index, all_names)
  
  #get the tuned final model
  selected_model <- pre_final[all_names_index]
  
  saveRDS(pre_final, file=file.path(out_path, paste0(index, '.rds')))
  
  #third best
  index <- as.character(order$Model)[3]
  
  #find where in all_names this lies
  all_names_index <- match(index, all_names)
  
  #get the tuned final model
  selected_model <- pre_final[all_names_index]
  
  saveRDS(pre_final, file=file.path(out_path, paste0(index, '.rds')))
  
  #fourth best
  index <- as.character(order$Model)[4]
  
  #find where in all_names this lies
  all_names_index <- match(index, all_names)
  
  #get the tuned final model
  selected_model <- pre_final[all_names_index]
  
  saveRDS(pre_final, file=file.path(out_path, paste0(index, '.rds')))
  
  #save to item one in output
  for_output[5] <- p
  
  #stop the cluster
  stopCluster(cl)
  
  #return the output
  return(for_output)
}

#save out models with different variables
#with stand age
above_stand <- model_compare(above, "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/stand_age/no_sig")
below_stand <- model_compare(below, "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/stand_age/no_sig")
# 
# #without stand age
above_stand <- model_compare(above %>% dplyr::select(-stand.age), "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/aboveground/no_stand_age/no_sig")
below_stand <- model_compare(below %>% dplyr::select(-stand.age), "/mnt/data1/boreal/spotter/combustion/final_files/model_comparisons/belowground/no_stand_age/no_sig")

#now get observed vs predicted values for the full best model across the comparisons
#need 
#get the index of the best model
# index <- as.character(order$Model)[1]
# 
# #find where in all_names this lies
# all_names_index <- match(index,all_names)
# 
# #get the tuned final model
# selected_model <- all_final[all_names_index]
# 
# full_model <- train(y ~., data = above, method = 'qrf', tuneGrid=data.frame(.mtry = 2))



