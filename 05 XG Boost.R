library(xgboost)
library(rsample)
library(haven)


#Reading datasets for test and train
train <- read_dta('./datasets/train_23.dta') %>% 
  select(-enrid, -sfh, -visit) 
test <- read_dta('./datasets/test_23.dta') %>%
  select(-enrid, -sfh, -visit)

# Removing NA values from test and train
test <- test[complete.cases(test),]
train <- train[complete.cases(train),]

# Removing ga_birth for model building
df_test <- test %>% select(-ga_birth)
df_train <- train %>% select(-ga_birth)





# variable names
features <- setdiff(names(df_train), "ga")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(df_train, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, df_train, varRestriction = new_vars) %>% as.matrix()
response_train <- df_train$ga

# Prepare the test data
features_test <- vtreat::prepare(treatplan, df_test, varRestriction = new_vars) %>% as.matrix()
response_test <- df_test$ga

# dimensions of one-hot encoded data
dim(features_train)

dim(features_test)





# reproducibility
set.seed(123)

xgb.fit1 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:linear",  # for regression models
  verbose = 0               # silent,
)

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train   = min(train_rmse_mean),
    ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test   = min(test_rmse_mean),
  )
##   ntrees.train rmse.train ntrees.test rmse.test
## 808	0.0031402	20	0.802883	

# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")

#Grid search

# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c( .1, .3),
  max_depth = c(5, 7),
  min_child_weight = c( 5, 7),
  subsample = c( .8, 1), 
  colsample_bytree = c(.9, 1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for (i in 1:nrow(hyper_grid)) {
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}




hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)


#Train model


# parameter list
params <- list(
  eta = 0.1,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 1,
  colsample_bytree = 0.9
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 77,
  objective = "reg:linear",
  verbose = 0
)


#Variable importance


# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")



#Predicting 


# predict values for test data
#pred <- predict(xgb.fit.final, features_test)


#calculating R-2 and RMSE on test

cor((predict(xgb.fit.final,features_test)), df_test$ga)^2  #R-squared for best model
RMSE((predict(xgb.fit.final,features_test)), df_test$ga) #RMSE for model

test$xg_boost <- predict(xgb.fit.final, features_test)

#Values on Training Set

cor((predict(xgb.fit.final,features_train)), df_train$ga)^2  #R-squared for best model
RMSE((predict(xgb.fit.final,features_train)), df_train$ga) #RMSE for model

#dat$xg_boost <- predict(xgb.fit.final, features_train)
