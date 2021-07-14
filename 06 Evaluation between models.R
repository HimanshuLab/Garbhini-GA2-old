library(dplyr)
library(tidyverse)
require(ranger) #for Random Forest model
require(xgboost) #for XG Boost model

seed_value <- 666
seed_range <- 100:599

#Reading datasets for test and train
train <- read_dta('./datasets/train_23.dta') %>% 
  select(-enrid, -sfh, -visit) 
test <- read_dta('./datasets/test_23.dta') %>%
  select(-enrid, -sfh, -visit)
test <- test[complete.cases(test),]  
train <- train[complete.cases(train),] 

#1. Random Forest model 
dat <- subset(train, select = -c(ga_birth)) #removing non-important columns
dat <- dat[complete.cases(dat),]

# Best performing Random Forest Model (refer code 04 for hyper parameter tuning)
ranger_model <- ranger(
  formula         = ga ~ ., 
  data            = dat,   
  num.trees       = 500,
  mtry            = 3,
  min.node.size   = 10,
  sample.fraction = .55,
)

#2. Garbhini-poly (Polynomial model)
#3. Hadlock - Published model
#4. Intergrowth-21st - Published model
predict_ga <- function(test,     #Function for formula-based models 
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
  if ("Garbhini-Polynomial" %in% method) {
    ga <- predict(lm(ga ~ fl + ofd + bpd,data = test),test)
  }
  return(ga)
}

#5. XG Boost model (for hyperparamter tuning part, refer script 05)
set.seed(123)
df_test <- test %>% select(-ga_birth) #removing ga_birth for model building from test and train
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


# R-2, RMSE calculations on test dataset

# 1. Random Forest Garbhini-GA2 model
cor(predictions(predict(ranger_model,test)),test$ga)^2  #R-squared for best model
RMSE(predictions(predict(ranger_model,test)),test$ga) #RMSE for model
test$random_forest <- predictions(predict(ranger_model,test)) #saving values in test variable

# formula-based models 
# 2. Garbhini-poly
# 3. Hadlock 
# 4. Intergrowth-21st
r2_test <- data.frame(sapply(c("Hadlock",
                               "Intergrowth",
                               "Garbhini-Polynomial"),
                             function(i) predict_ga(test,i))) %>%
  apply(2, function(i) {
    c(RMSE = RMSE(i,test$ga) , R2 = cor(i,test$ga)^2)
  }) %>% t
r2_test # displays R2 and RMSE for all formulae based models

# 5. XG Boost: Garbhini XG Boost
cor((predict(xgb.fit.final,features_test)), df_test$ga)^2  #R-squared for best model
RMSE((predict(xgb.fit.final,features_test)), df_test$ga) #RMSE for model
test$xg_boost <- predict(xgb.fit.final, features_test) #saving values in test variable

# removing unnecessary variables before next step
rm(ranger_model, xgb.fit.final, df_train, df_test, params, treatplan, importance_matrix, dat, features_test, features_train)

# Plotting distribution of errors on test dataset
test$hadlock <- predict_ga(test,"Hadlock")
test$garbhini_poly <- predict_ga(test,"Garbhini-Polynomial")
test$intergrowth <- predict_ga(test,"Intergrowth")

random_forest <- test %>% select(ga,random_forest)
random_forest$type <- rep("Garbhini-GA2",length(random_forest$random_forest))
colnames(random_forest) <- c("ga","ga_formula","formula")

hadlock <- test %>% select(ga,hadlock)
hadlock$type <- rep("Hadlock",length(hadlock$ga))
colnames(hadlock) <- c("ga","ga_formula","formula")

intergrowth <- test %>% select(ga,intergrowth)
intergrowth$type <- rep("Intergrowth-21",length(intergrowth$ga))
colnames(intergrowth) <- c("ga","ga_formula","formula")

garbhini_poly <- test %>% select(ga,garbhini_poly)
garbhini_poly$type <- rep("Garbhini-polynomial",length(garbhini_poly$ga))
colnames(garbhini_poly) <- c("ga","ga_formula","formula")

xg_boost <- test %>% select(ga,xg_boost)
xg_boost$type <- rep("Garbhini-XGB",length(xg_boost$xg_boost))
colnames(xg_boost) <- c("ga","ga_formula","formula")

# for figure_3A - Distribution of error - Random Forest (Garbhini-GA2), Intergrowth and Hadlock 
figure_3a_dataset <- rbind(hadlock, intergrowth, random_forest)
figure_3a_dataset$error <- figure_3a_dataset$ga_formula - figure_3a_dataset$ga
figure_3a_dataset <- figure_3a_dataset %>%
  mutate(formula = factor(formula, levels = c("Hadlock", "Intergrowth-21",'Garbhini-GA2' )))

figure_3a <- ggplot(figure_3a_dataset, aes(y = error, x = formula, fill = formula)) +
  geom_violin() + ggtitle("Distribution of errors on test set ") +
  scale_y_continuous(breaks = -8:8, limits = c(-8,8)) + theme_bw() +
  labs(caption = paste("n = 1430")) +
  theme(legend.position = "none") + coord_flip() 

figure_3a

#saving Figure3A 
ggsave(file="./figures/SVG/Figure_3A.svg", plot=figure_3a, width=10, height=8)
ggsave(file="./figures/EPS/Figure_3A.eps", plot=figure_3a, width=10, height=8)
ggsave(file="./figures/pdf/Figure_3A.pdf", plot=figure_3a, width=10, height=8)

# for figure_s3
figure_s3_dataset <- rbind(hadlock, intergrowth, random_forest, xg_boost, garbhini_poly)
figure_s3_dataset$error <- figure_s3_dataset$ga_formula - figure_s3_dataset$ga
figure_s3_dataset <- figure_s3_dataset %>%
  mutate(formula = factor(formula, levels = c("Hadlock", "Intergrowth-21",'Garbhini-XGB','Garbhini-polynomial','Garbhini-GA2')))

figure_s3 <- ggplot(figure_s3_dataset, aes(y = error, x = formula, fill = formula)) +
  geom_violin() + ggtitle("Distribution of errors on test set ") +
  scale_y_continuous(breaks = -8:8, limits = c(-8,8)) + theme_bw() +
  labs(caption = paste("n = 1430")) +
  theme(legend.position = "none") + coord_flip() 

figure_s3

#saving FigureS3 
ggsave(file="./figures/SVG/Figure_S3.svg", plot=figure_s3, width=10, height=8)
ggsave(file="./figures/EPS/Figure_s3.eps", plot=figure_s3, width=10, height=8)
ggsave(file="./figures/pdf/Figure_s3.pdf", plot=figure_s3, width=10, height=8)

