# Section of code that runs GLM and Boruta to show most important features
library(readr) 
library(magrittr)
library(dplyr)
library(dbscan)
library(ggplot2)
library(caret)
library(reshape2)
library(haven)
library(gridExtra)
library(grid)
library(tidyr)
library(skimr)
library(stats)

seed_value <- 666
seed_range <- 100:599

# Read training dataset, feature list and raw dataset
train_df <- read_dta('./datasets/train_23.dta')
features_list <- scan('./features_list.txt', sep = "\n", what = "")

if (!("data_raw" %in% ls())) {
  data_raw <- read.csv("./datasets/merged_final_dataset.csv",na.strings = c(""," ","NA","88","99", "99.99","999.99","777.77"),header = TRUE )   
  #old dataset: data_raw <- read_csv("20NOV2018.csv")
  #data_raw[data_raw %in% c(""," ","NA","88","99", "99.99","777.77")] <- NA
}

# Adding associated variables from data_raw
train_var <- train_df %>%
  left_join(data_raw) %>%
  select(features_list)


data = cbind(train_df, train_var)

# Preparing dataset for feature selection
data <- subset(data, select =-c(enrid, ga_birth, visit))
data <- data[, -grep("vdt", colnames(data))]

#removing features with a high number of missing values 
column.counts <- colSums(!is.na(data[,2:ncol(data)]))
ggplot(NULL, aes(x=column.counts)) + geom_histogram()

#and then omitting rows with missing values
data <- data[,apply(data,2,function(i) sum(is.na(i)) < 270)] %>% na.omit()
#find categorical variables
x<-colnames(data)
x<-x[sapply(data,class) != "numeric"]

#converting categorical variables into factors
for(j in c("fmly_typ","rlgn",
           "part_occ","fmly_mem",
           "drnk_wtr","derived_ses_mks_2019",
           "derived_state")){
  data[,j] <- lapply(data[,j], as.factor)
}
data <- data[,-nearZeroVar(data)]

# Boruta
require(Boruta)
set.seed(seed_value)
boruta_output <- Boruta(ga ~ .,
                        data= data,
                        doTrace=0,
                        maxRuns = length(seed_range))
features_boruta<-names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])

# Feature importance graph
plot(boruta_output, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta_output$ImpHistory), function(i) 
  boruta_output$ImpHistory[is.finite(boruta_output$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_output$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las = 2,labels = names(Labels), at = 1:ncol(boruta_output$ImpHistory), cex.axis = 0.55)

# image is saved using R "Export" option



