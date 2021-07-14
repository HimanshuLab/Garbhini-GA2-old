
Library imports for all functions

library(readr)
library(forcats)
library(gbm)
library(randomForest)
library(readxl)
library(magrittr)
library(reshape2)
library(ggplot2)
suppressMessages(library(cowplot))
library(UpSetR)  #for UpSet plots 
library(dplyr)
library(dbscan)
library(caret)
library(ggridges)
library(stringr)
library(haven)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(foreign)
library(RColorBrewer)
library(ggplot2)
library(magrittr) #used for the pipe operator %>% which feeds the output of the previous function to the next function as . or assigns to first parameter by default 
library(BlandAltmanLeh)
library(DescTools)
library(rsample)
library(xgboost)
library(vtreat)

seed_value <-  666
seed_range <- 100:199

return_split_data <- function(data, ratio , seed_value,
                              force_balance = c("no","undersample")){
  set.seed(seed_value)
  trainIndex <- createDataPartition(data$ga, p = ratio, 
                                    list = FALSE, 
                                    times = 1)
  
  training_set <- data[trainIndex,]
  df_test <- data[-trainIndex,]
  
  rm(trainIndex)
  preProcValues <- preProcess(training_set, method = c("range"))
  
  training_set <- predict(preProcValues, training_set)
  df_test <- predict(preProcValues, df_test)
  return(list(training_set, df_test))
}

return_split_data <- function(data, ratio , seed_value){
  set.seed(seed_value)
  df_enrid<-data.frame(matrix(ncol=2,nrow=length(unique(data$enrid))))
  colnames(df_enrid)<-c('enrid','count')
  df_enrid$enrid<-unique(data$enrid)
  for(enr in na.omit(unique(data$enrid))){
    df_enrid[df_enrid$enrid==enr,'count']=nrow(data[data$enrid==enr,])
  }
  picked=c()
  for(x in 1:3){
    print(x)
    sample_size = round(ratio*nrow(df_enrid[df_enrid$count==x,]),digits=0)
    pick = sample(as.numeric(rownames(df_enrid[df_enrid$count==x,])),size = sample_size)
    print(length(pick)/nrow(df_enrid[df_enrid$count==x,]))
    picked=c(picked,pick)
  }
  picked_enrids<-df_enrid[picked,'enrid']
  unpicked_enrids<-df_enrid[-picked,'enrid']
  train =data[data$enrid %in% picked_enrids,]
  test =data[data$enrid %in% unpicked_enrids,]
  return(list(train,test))
}


#Removing abortions and stillbirths

df$ga_first <- ifelse(df$type == 'ds1', predict_ga(df$crl_qc_ds111, "Hadlock"), predict_ga(df$crl_qc_em1, "Hadlock")) #in weeks

#add Garbhni-1 formula and calculate ga_birth
df$ga_birth <- ifelse(df$type == 'ds1', (df$ga_first+(df$op_dt_event-df$vdt_ds1)/7), (df$ga_first+(df$op_dt_event-df$vdt_em1)/7))

#removing abortions and still births
df <- df[(df$ga_birth >= 20) ,]
live_birth <- (df$op_sing_liv_birth == 1 | is.na(df$op_sing_liv_birth))
df <- df[(live_birth),]

rm(as_present,crl_present,ds_present,em_present,features,fwb_present,fwbv_present,i)



df$ga_as<-NA
df$ga_fwb1 <- NA
df$ga_fwbv1 <- NA
df[!is.na(df$crl_qc_ds111),"ga_as"]<-(df[!is.na(df$crl_qc_ds111),"vdt_as"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Hadlock")
df[!is.na(df$crl_qc_em1),"ga_as"]<-(df[!is.na(df$crl_qc_em1),"vdt_as"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Hadlock")

df[!is.na(df$crl_qc_ds111),"ga_fwb1"]<-(df[!is.na(df$crl_qc_ds111),"vdt_fwb1"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Hadlock")
df[!is.na(df$crl_qc_em1),"ga_fwb1"]<-(df[!is.na(df$crl_qc_em1),"vdt_fwb1"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Hadlock")

df[!is.na(df$crl_qc_ds111),"ga_fwbv1"]<-(df[!is.na(df$crl_qc_ds111),"vdt_fwbv1"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Hadlock")
df[!is.na(df$crl_qc_em1),"ga_fwbv1"]<-(df[!is.na(df$crl_qc_em1),"vdt_fwbv1"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Hadlock")

# df[!is.na(df$crl_qc_ds111),"ga_birth"]<-(df[!is.na(df$crl_qc_ds111),"op_dt_event"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Garbhini1")
# df[!is.na(df$crl_qc_em1),"ga_birth"]<-(df[!is.na(df$crl_qc_em1),"op_dt_event"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Garbhini1")



write.dta(data,'./datasets/bothtrim_hadlock.dta')
sets<-return_split_data(data, ratio=0.7, seed_value = seed_value)
train<-sets[[1]]
test<-sets[[2]]
write.dta(train,'.datasets/train_23_hadlock.dta')
write.dta(test, './datasets/test_23_hadlock.dta')


#Loading datasets

test <- read_dta('test_23_hadlock.dta')
train <- read_dta('train_23_hadlock.dta')



#test = 1430
#train = 3338



#2. Random Forest
#2.1 

seed_value <- 666
seed_range <- 100:599

#making dataset without important columns
dat <- subset(train, select = -c(enrid,visit,ga_birth,sfh))


Removing NA values from test and train

#test <- test[complete.cases(test),]
#train <- train[complete.cases(train),]

 
#test = 1375
#train = 3218

#2.2 Plot showing how many trees is optimal to saturate MSE values

m1 <- randomForest(
  formula = ga~.,
  data = dat
)
m1
plot(m1)
which.min(m1$mse)
sqrt(m1$mse[which.min(m1$mse)])




#2.3 Plot showing OOB error vs test error

valid.split <- initial_split(dat,.7)

#training data 
train_set <- analysis(valid.split)

#validation data
valid <- assessment(valid.split)
x_test <- valid[setdiff(names(valid),"ga")]
y_test <- valid$ga

rf_oob_comp <- randomForest(
  formula = ga~.,
  data = train_set,
  xtest = x_test,
  ytest = y_test
)

#extract OOB and validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

#compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test Error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric,RMSE,-ntrees) %>%
  ggplot(aes(ntrees,RMSE,color = Metric)) +
  geom_line() +
  xlab("Number of Trees")




#2.4 Function for splitting into training and test and normalizing values

return_split_data <- function(data, ratio , seed_value,
                              force_balance = c("no","undersample")){
  set.seed(seed_value)
  trainIndex <- createDataPartition(data$ga, p = ratio, 
                                    list = FALSE, 
                                    times = 1)
  
  training_set <- data[trainIndex,]
  df_test <- data[-trainIndex,]
  
  rm(trainIndex)
  
  return(list(training_set, df_test))
}


#2.5 Selecting training and test datasets (70:30 split)

temp <- return_split_data(data = dat, ratio = 0.7 , seed_value = 100)
#for modelling
TrainSet <- temp[[1]]
ValidSet <- temp[[2]]
TrainComplete <- dat


#2.6 Hyper parameter tuning

hyper_grid <- expand.grid(
  mtry       = seq(3,5, by = 1),
  node_size  = seq(3, 10, by = 1),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)


#2.7 Calculating OOB error after training

require(ranger)
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = ga ~ ., 
    data            = TrainSet, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


#2.8 Histogram showing possible range of RMSE values in 100 iterations

OOB_RMSE <- vector(mode = "numeric", length = 100)
for (i in seq_along(OOB_RMSE)) {
optimal_ranger <- ranger(
    formula         = ga ~ ., 
    data            = TrainSet, 
    num.trees       = 500,
    mtry            = 3,
    min.node.size   = 10,
    sample.fraction = .55,
    importance      = 'impurity'
  )
OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}
hist(OOB_RMSE,breaks = 20)


#2.9 Plot showing relative importance of variables

var <- data.frame(optimal_ranger$variable.importance)
var <- cbind(varname = rownames(var), var) 
colnames(var) <- c('varname','imp')
ggplot(var, aes(reorder(varname, imp), imp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable", y = "Importance") +
  ggtitle("Variables in order of importance: Random forest") +
  theme_bw()


2.9.1 Calculating test and training R-2 for 500 iterations


rf_metrics <- sapply(seed_range,function(i){
  temp <- return_split_data(data = dat, ratio = 0.7 , seed_value = i)
  training_set <- temp[[1]]
  valid_set <- temp[[2]]
  ranger_model <- ranger(
    formula         = ga~ ., 
    data            = TrainSet, 
    num.trees       = 500,
    mtry            = 3,
    min.node.size   = 10  ,
    sample.fraction = .55,
  )
  y_pred <- predict(ranger_model,valid_set)$predictions
  c(test = cor(y_pred, valid_set$ga)^2,
    training = cor(predict(ranger_model,training_set)$predictions, training_set$ga)^2,
    indep_variable = ranger_model$num.independent.variables)
}) %>% t %>% as.data.frame()

#Plotting test vs train R-2 for all the 500 iterations
ggplot(rf_metrics, aes(x = training, y = test )) + geom_point() +
  theme_bw() +
  ggtitle("Random Forest algorithm") +
  ylab("R-squared on validation set (30%) ") +
  xlab("R-squared on training set (70%)") +
  labs(caption = paste("number of iterations = 500"))


#XG Boost running

#1. Reading datasets for test and train


train <- read_dta('train_23_hadlock.dta') %>% 
  select(-enrid, -sfh, -visit) 
test <- read_dta('test_23_hadlock.dta') %>%
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




{r warning = FALSE, message = FALSE}
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


Grid search

{r warning = FALSE, message = FALSE}

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
for(i in 1:nrow(hyper_grid)) {
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



Train model


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


Variable importance


# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")


Predicting 


# predict values for test data
#pred <- predict(xgb.fit.final, features_test)


#calculating R-2 and RMSE on test

cor((predict(xgb.fit.final,features_test)),df_test$ga)^2  #R-squared for best model
RMSE((predict(xgb.fit.final,features_test)),df_test$ga) #RMSE for model

test$xg_boost <- predict(xgb.fit.final, features_test)

#Values on Training Set

cor((predict(xgb.fit.final,features_train)),df_train$ga)^2  #R-squared for best model
RMSE((predict(xgb.fit.final,features_train)),df_train$ga) #RMSE for model

dat$xg_boost <- predict(xgb.fit.final, features_train)



COMPARISON OF MODELS 

Loading datasets

test <- read_dta('test_23_hadlock.dta')
train <- read_dta('train_23_hadlock.dta')

# Removing NA values from test and train
test = subset(test, select = -c(enrid,visit, sfh))
test <- test[complete.cases(test),]
#train <- train[complete.cases(train),]

#making dataset without important columns for training of Random Forest model
dat <- subset(train, select = -c(enrid,visit,ga_birth, sfh))
dat <- dat[complete.cases(dat),]
 




#1. Random Forest model 

seed_value <- 666
seed_range <- 100:599

require(ranger)
# Training Model on complete training set dat
ranger_model <- ranger(
    formula         = ga ~ ., 
    data            = dat,   
    num.trees       = 500,
    mtry            = 3,
    min.node.size   = 10,
    sample.fraction = .55,
  )

#calculating R-2 and RMSE on test

cor(predictions(predict(ranger_model,test)),test$ga)^2  #R-squared for best model
RMSE(predictions(predict(ranger_model,test)),test$ga) #RMSE for model

test$random_forest <- predictions(predict(ranger_model,test))

#Values on Training Set


cor(predict(ranger_model,dat)$predictions, dat$ga)^2
RMSE(predict(ranger_model,dat)$predictions, dat$ga)

dat$random_forest <- predict(ranger_model,dat)$predictions




predict_ga <- function(test,
                       method = c("Hadlock",
                                  "Intergrowth",
                                  "Garbhini")){
  ga <- numeric()
  hp <- test$hp
  fl <- test$fl
  ap <- test$ap
  bpd <- test$bpd
  ofd <- test$ofd
  
  if ("Hadlock" %in% method) {
    ga <- 10.85 + (0.06*hp*fl) + (0.67*bpd) + (0.168*ap)
  }
  if ("Intergrowth" %in% method) {
    ga <- exp((0.03243*(log(hp*10)^2)) + (0.001644*(fl*10)*log(hp*10)) + 3.813)/7
  }
  if ("Garbhini" %in% method) {
    ga <- predict(lm(ga ~ fl + ofd + bpd,data = dat),test)
  }
  return(ga)
}






r2_test <- data.frame(sapply(c("Hadlock",
                             "Intergrowth",
                             "Garbhini"),
                           function(i) predict_ga(test,i))) %>%
  apply(2, function(i) {
    c(RMSE = RMSE(i,test$ga) , R2 = cor(i,test$ga)^2)
  }) %>% t
r2_test



Distribution of errors on test dataset


test$hadlock <- predict_ga(test,"Hadlock")
test$garbhini <- predict_ga(test,"Garbhini")
test$intergrowth <- predict_ga(test,"Intergrowth")


hadlock <- test %>% select(ga,hadlock)
hadlock$type <- rep("Hadlock",length(hadlock$ga))
colnames(hadlock) <- c("ga","ga_formula","formula")
intergrowth <- test %>% select(ga,intergrowth)
intergrowth$type <- rep("Intergrowth-21",length(intergrowth$ga))
colnames(intergrowth) <- c("ga","ga_formula","formula")
garbhini <- test %>% select(ga,garbhini)
garbhini$type <- rep("Garbhini polynomial model",length(garbhini$ga))
colnames(garbhini) <- c("ga","ga_formula","formula")
random_forest <- test %>% select(ga,random_forest)
random_forest$type <- rep("Garbhini Random Forest",length(random_forest$random_forest))
colnames(random_forest) <- c("ga","ga_formula","formula")

xg_boost <- test %>% select(ga,xg_boost)
xg_boost$type <- rep("Garbhini XG Boost",length(xg_boost$xg_boost))
colnames(xg_boost) <- c("ga","ga_formula","formula")



published_formulae <- rbind(hadlock,intergrowth,garbhini,random_forest, xg_boost)


published_formulae$error <- published_formulae$ga_formula - published_formulae$ga



ggplot(published_formulae, aes(y = error, x = formula, fill = formula)) +
  geom_violin() + ggtitle("Distribution of errors on test set ") +
  scale_y_continuous(breaks = -8:8, limits = c(-8,8)) + theme_bw() +
  labs(caption = paste("n = 1430")) +
  theme(legend.position = "none") + coord_flip()




ggplot(hadlock,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Hadlock predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 1430"))

ggplot(intergrowth,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Intergrowth predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 1430"))

ggplot(garbhini,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Garbhini Polynomial model predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 1430"))

#parity plot
ggplot(random_forest,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Random forest predicted GA vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 1430"))

#parity plot
ggplot(xg_boost,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("XG Boost predicted GA vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 1430"))


Comparison on Training 

predict_ga <- function(dat,
                       method = c("Hadlock",
                                  "Intergrowth",
                                  "Garbhini")){
  ga <- numeric()
  hp <- dat$hp
  fl <- dat$fl
  ap <- dat$ap
  bpd <- dat$bpd
  ofd <- dat$ofd
  
  if ("Hadlock" %in% method) {
    ga <- 10.85 + (0.06*hp*fl) + (0.67*bpd) + (0.168*ap)
  }
  if ("Intergrowth" %in% method) {
    ga <- exp((0.03243*(log(hp*10)^2)) + (0.001644*(fl*10)*log(hp*10)) + 3.813)/7
  }
  if ("Garbhini" %in% method) {
    ga <- predict(lm(ga ~ fl + ofd + bpd,data = dat),dat)
  }
  return(ga)
}






r2_test <- data.frame(sapply(c("Hadlock",
                             "Intergrowth",
                             "Garbhini"),
                           function(i) predict_ga(dat,i))) %>%
  apply(2, function(i) {
    c(RMSE = RMSE(i,dat$ga) , R2 = cor(i,dat$ga)^2)
  }) %>% t
r2_test



Distribution of errors on train


dat$hadlock <- predict_ga(dat,"Hadlock")
dat$garbhini <- predict_ga(dat,"Garbhini")
dat$intergrowth <- predict_ga(dat,"Intergrowth")


hadlock <- dat %>% select(ga,hadlock)
hadlock$type <- rep("Hadlock",length(hadlock$ga))
colnames(hadlock) <- c("ga","ga_formula","formula")
intergrowth <- dat %>% select(ga,intergrowth)
intergrowth$type <- rep("Intergrowth-21",length(intergrowth$ga))
colnames(intergrowth) <- c("ga","ga_formula","formula")
garbhini <- dat %>% select(ga,garbhini)
garbhini$type <- rep("Garbhini polynomial model",length(garbhini$ga))
colnames(garbhini) <- c("ga","ga_formula","formula")
random_forest <- dat %>% select(ga,random_forest)
random_forest$type <- rep("Garbhini Random Forest",length(random_forest$random_forest))
colnames(random_forest) <- c("ga","ga_formula","formula")

xg_boost <- dat %>% select(ga,xg_boost)
xg_boost$type <- rep("Garbhini XG Boost",length(xg_boost$xg_boost))
colnames(xg_boost) <- c("ga","ga_formula","formula")



published_formulae <- rbind(hadlock,intergrowth,garbhini,random_forest, xg_boost)




published_formulae$error <- published_formulae$ga_formula - published_formulae$ga



ggplot(published_formulae, aes(y = error, x = formula, fill = formula)) +
  geom_violin() + ggtitle("Distribution of errors on train set ") +
  scale_y_continuous(breaks = -8:8, limits = c(-8,8)) + theme_bw() +
  labs(caption = paste("n = 3338")) +
  theme(legend.position = "none") + coord_flip()




ggplot(hadlock,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Hadlock predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 3338"))

ggplot(intergrowth,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Intergrowth predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 3338"))

ggplot(garbhini,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Garbhini polynomial model predicted GA vs vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 3338"))

#parity plot
ggplot(random_forest,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("Random forest predicted GA vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 3338"))

ggplot(xg_boost,aes(x = ga,y = ga_formula)) +
  geom_point(alpha = 0.8, shape = 1) +
  geom_abline(slope = 1,color = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  ggtitle("XG Boost predicted GA vs gold standard") +
  ylab("Predicted GA (weeks)") +
  xlab("Gold Standard GA (weeks)") +
  labs(caption = paste("n = 3338"))

Bland-Altman analysis
1. Train

require(BlandAltmanLeh)
dat$ga_birth = train$ga_birth
predicted_main <- subset(dat, select = -c(bpd,ofd,hp,ap,fl,garbhini,ga,ga_birth))
pairwise_BA_main <- sapply(predicted_main, function(i) {
  sapply(predicted_main,function(j){
    BA_stats <- bland.altman.stats(i,j, conf.int = 0.95)
    paste0(BA_stats$mean.diffs %>% round(3),
           "( ",BA_stats$lower.limit %>% round(3),
           ", ",BA_stats$upper.limit %>% round(3),")")
    
  })
})




2. Test

predicted_test <- subset(test, select = -c(bpd,ofd,hp,ap,garbhini,fl,ga,ga_birth))
pairwise_BA_test <- sapply(predicted_test, function(i){
  sapply(predicted_test,function(j){
    BA_stats <- bland.altman.stats(i,j, conf.int = 0.95)
    paste0(BA_stats$mean.diffs %>% round(3),
           "( ",BA_stats$lower.limit %>% round(3),
           ", ",BA_stats$upper.limit %>% round(3),")")
    
  })
})




write.dta(predicted_main,'predicted_main_hadlock.dta')
write.dta(predicted_test, 'predicted_test_hadlock.dta')

write.dta(dat,"full_train_hadlock.dta")
write.dta(test,"full_test_hadlock.dta")


3. Making table

table4 <- matrix(nrow = ncol(predicted_main), ncol = ncol(predicted_main),
                 dimnames = list(colnames(predicted_main),colnames(predicted_main)))
table4[upper.tri(table4)] <- pairwise_BA_main[upper.tri(pairwise_BA_main)]

table4[lower.tri(table4)] <- pairwise_BA_test[lower.tri(pairwise_BA_test)]
table4 %>% as.data.frame()



## OPTIONAL CHUNKS 

Optional writing functions

preterm_train <- subset(dat,select = c(hadlock,intergrowth,random_forest, xg_boost))
preterm_train <- preterm_train + (train$ga_birth - train$ga)
preterm_train$ga <- train$ga_birth

preterm_test <- subset(test,select = c(hadlock,intergrowth,random_forest, xg_boost))
preterm_test <- preterm_test + (test$ga_birth - test$ga)
preterm_test$ga <- test$ga_birth

write.dta(preterm_train,'preterm_train_23_hadlock.dta')
write.dta(preterm_test, 'preterm_test_23_hadlock.dta')




PTB analysis 


train <- read_dta('./datasets/preterm_train_23_hadlock.dta')
test <- read_dta('./datasets/preterm_test_23_hadlock.dta')



columns <- c('hadlock','intergrowth','random_forest', 'xgboost','ga')
colnames(test) <- columns
colnames(train) <- columns
#test = test %>% select(-garbhini)
#train = train %>% select(-garbhini)


Training set


preterm_train <- subset(train,select = c(hadlock,intergrowth,random_forest, xgboost,ga))


conf_train <- preterm_train %>% sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% round(3)
apply(preterm_train,2,function(i) sum(i < 37)*100/length(i)) %>% range()

conf_train

sapply(colnames(preterm_train), function(i){
  sapply(colnames(preterm_train), function(j){
    (c((preterm_train[,i] < 37) %>% table , (preterm_train[,j] < 37) %>% table) %>%
       matrix(nrow = 2) %>% 
       fisher.test())$p.value
  })
}) %>% (function(x) {
  x[lower.tri(x)] <- NA
  x
}) %>%  melt() %>% na.omit() %>% mutate( value = p.adjust(value, method = "bonferroni")) %>% filter(value < 0.05)




Test dataset

preterm_test <- subset(test,select = c(hadlock,intergrowth,random_forest, xgboost, ga))
#preterm_test <- preterm_test + (test$ga_birth - test$ga)

conf_test <- preterm_test %>% sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% round(3)
apply(preterm_test,2,function(i) sum(i < 37)*100/length(i)) %>% range()

conf_test

sapply(colnames(preterm_test), function(i){
  sapply(colnames(preterm_test), function(j){
    (c((preterm_test[,i] < 37) %>% table , (preterm_test[,j] < 37) %>% table) %>%
       matrix(nrow = 2) %>% 
       fisher.test())$p.value
  })
}) %>% (function(x) {
  x[lower.tri(x)] <- NA
  x
}) %>%  melt() %>% na.omit() %>% mutate( value = p.adjust(value, method = "bonferroni")) %>% filter(value < 0.05)





length of interval to identify narrowest interval

conf_test[3,] - conf_test[2,]

conf_train[3,] - conf_train[2,]





making plot of preterm rates with confidence intervals for all models

#importing datasets that have GA values calculated by all models
#preterm_test <- read.dta('preterm_test.dta')
#preterm_train <- read.dta('preterm_train.dta')
colnames(preterm_train) <- c("Hadlock","Intergrowth-21st","Garbhini-Random Forest", "XG Boost", "Gold Standard")

table_preterm_train <- preterm_train %>% sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% 
  t() %>% `*`(100) %>% round(2) %>%
  {
    data.frame(formula = row.names(.),
               PTB_rate = .[,1],
               CI_lower = .[,2],
               CI_higher = .[,3],
               # CI = paste0("(", .[,2] , ", " ,
               #             .[,3]," )"),
               row.names = NULL)
  } 

table_preterm_train %>%
  mutate(formula = fct_relevel(formula, "Hadlock","Intergrowth-21st","Garbhini-Random Forest","XG Boost","Gold Standard")) %>%
ggplot(aes(x = formula, y = PTB_rate)) +
#ggplot(table_preterm_test, aes(x = formula, y = PTB_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .2, size = 1) +
  coord_flip() +
  labs(title = 'PTB rates calculated by models on train dataset', y = 'PTB rate', x = 'Model') +
  theme_bw(base_size = 13) 
#ggplot(table_preterm_train, aes(x = formula, y = PTB_rate)) +
#  geom_point() +
#  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .1,) +
#  coord_flip() +
#  labs(title = 'PTB rates calculated by models on training dataset', y = 'PTB rate', x = 'Model') +
#  theme_bw()

colnames(preterm_test) <- c("Hadlock","Intergrowth-21st","Garbhini-Random Forest", "XG Boost","Gold Standard")

table_preterm_test <- preterm_test %>% sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% 
  t() %>% `*`(100) %>% round(2) %>%
  {
    data.frame(formula = row.names(.),
               PTB_rate = .[,1],
               CI_lower = .[,2],
               CI_higher = .[,3],
               # CI = paste0("(", .[,2] , ", " ,
               #             .[,3]," )"),
               row.names = NULL)
  } 


figure_s5_a <- table_preterm_test %>%
  mutate(formula = fct_relevel(formula, "Hadlock","Intergrowth-21st","Garbhini-Random Forest","XG Boost","Gold Standard")) %>%
ggplot(aes(x = formula, y = PTB_rate)) +
#ggplot(table_preterm_test, aes(x = formula, y = PTB_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .2, size = 1) +
  coord_flip() +
  labs(title = 'Hadlock as gold standard', y = 'PTB rate', x = 'Model') +
  theme_bw(base_size = 13) 
  
ggsave(file = './figures/Figure_S5_A.svg', plot = figure_s5_a, height = 8, width = 10)
figure_s5_a









agreement between formulae -> intersection by union

#preterm_test <- read_dta('preterm_test_23.dta')
#preterm_train <- read_dta('preterm_train_23.dta')

preterm_class <- preterm_train
preterm_class[preterm_class < 37] <- 1
preterm_class[!preterm_class < 37] <- 0
preterm_vals <- sapply(colnames(preterm_train), function(i){
  sapply(colnames(preterm_train), function(j){
    round((nrow(preterm_class[ which(preterm_class[,i] == 1 & preterm_class[,j] == 1) , ])*100)/nrow(preterm_class[ which(preterm_class[,i] == 1 | preterm_class[,j] == 1) , ]),3)
  })
}) %>% data.frame()

preterm_vals

preterm_class <- preterm_test
preterm_class[preterm_class < 37] <- 1
preterm_class[!preterm_class < 37] <- 0
preterm_vals <- sapply(colnames(preterm_test), function(i){
  sapply(colnames(preterm_train), function(j){
    round((nrow(preterm_class[ which(preterm_class[,i] == 1 & preterm_class[,j] == 1) , ])*100)/nrow(preterm_class[ which(preterm_class[,i] == 1 | preterm_class[,j] == 1) , ]),3)
  })
}) %>% data.frame()

preterm_vals


