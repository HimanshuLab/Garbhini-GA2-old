require(randomForest)
require(ranger)
library(haven)

#Loading datasets

test <- read_dta('./datasets/test_23.dta')
train <- read_dta('./datasets/train_23.dta')


#test = 1430
#train = 3338

#Removing NA values from test and train

test <- test[complete.cases(test),]
train <- train[complete.cases(train),]

#test = 1375
#train = 3218

#2. Random Forest
#2.1 

seed_value <- 666
seed_range <- 100:599

#making dataset without important columns
dat <- subset(train, select = -c(enrid,visit,ga_birth,sfh))




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
train <- analysis(valid.split)

#validation data
valid <- assessment(valid.split)
x_test <- valid[setdiff(names(valid),"ga")]
y_test <- valid$ga

rf_oob_comp <- randomForest(
  formula = ga~.,
  data = train,
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
figure_2b <- ggplot(var, aes(reorder(varname, imp), imp)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable", y = "Importance") +
  ggtitle("Variables in order of importance: Random forest") +
  theme_bw()
figure_2b

ggsave(file="./figures/SVG/Figure_2B.svg", plot=figure_2b, width=10, height=8)
ggsave(file="./figures/EPS/Figure_2B.eps", plot=figure_2b, width=10, height=8)
ggsave(file="./figures/PDF/Figure_2B.pdf", plot=figure_2b, width=10, height=8)


##2.9.1 Calculating test and training R-2 for 500 iterations


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
