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
library(cowplot)
library(parallel)

seed_value <- 666
seed_range <- 100:119
return_split_data <- function(data, ratio , seed_value,
                              force_balance = c("no","undersample")){
  set.seed(seed_value)
  trainIndex <- createDataPartition(data$ga_as, p = ratio, 
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
features <- c('symph_fund_hght','fl_qc_as','ofd_qc_as',
              'bpd_qc_as','hp_qc_as','ap_qc_as','cl_qc_as','map','pulse_pressure')
dat <-read_dta('/Users/RAMYA/git/second_trimester_dating/dbscan.dta')
dat <-dat[,c(features, 'ga_as')]
dat <-dat[complete.cases(dat),]
# formulas <- expand.grid("ga ~ " ,
#                         c("","hc","log(1+hc) ","sqrt(hc) "),
#                         c("","+ ac ","+ log(1+ac) ", "+ sqrt(ac) "),
#                         c("","+ fl ","+ log(1+fl) ", "+ sqrt(fl) "),
#                         c("","+ ofd ","+ log(1+ofd)","+ sqrt(ofd)"),
#                         c("","+ bpd","+ log(1+bpd)","+ sqrt(bpd)"),
#                         c("","+ bpd","+ log(1+bpd)","+ sqrt(bpd)"),
#                         c("", ", degree = 2)", ", degree = 3)", ", degree = 4)"),
#                         stringsAsFactors = FALSE) %>%
#   apply(1,function(i)paste0(i,collapse=""))
# formulas <- formulas[formulas != "ga ~ "]
# formulas[grep("degree", formulas)] <- paste0(gsub(" \\+ ",
#                                                   ", ",
#                                                   formulas[grep("degree", formulas)]))
# formulas[grep("degree", formulas)] <- sub("~|~,", "~ polym(",formulas[grep("degree", formulas)] )

transform_feature <- function(x){
  return(c("",paste0("+ ",x),
           paste0("+ ",c("log(","sqrt("),x,")")))
}

formulas <- expand.grid("ga_as ~ " ,
                        lapply(features, transform_feature) %>% 
                          expand.grid(stringsAsFactors = FALSE) %>% apply(1,function(i)paste0(i,collapse="")),
                        c("", ", degree = 2)", ", degree = 3)"),
                        stringsAsFactors = FALSE) %>%
  apply(1,function(i)paste0(i,collapse=""))

formulas <- sub("~ \\+","~",formulas)
formulas[grep("degree", formulas)] <- paste0(gsub("\\+ ",
                                                  ", ",
                                                  formulas[grep("degree", formulas)]))
formulas[grep("degree", formulas)] <- sub("~|~,", "~ polym(",formulas[grep("degree", formulas)] )
formulas<-formulas[-which((formulas) %in% c("ga_as ~ "))]
#---------------------------------------------------------------------------
bootstrap <- lapply(X = seed_range,FUN =  function(seed_value){
  print (seed_value)
  temp <- return_split_data(data = dat, ratio = 0.70, seed_value = seed_value)
  training_set <- temp[[1]]
  test_set <- temp[[2]]
  ga_pred <- lapply(formulas, function(f){
    tryCatch(expr = predict(lm(formula = as.formula(f), data = training_set), test_set),
             error = function(e) NA,
             warning = function(w) "")
  })
  data.frame(seed_value,
             formula = formulas,
             R2 = sapply(ga_pred, function(i) ifelse(length(i)==1,
                                                     NA,
                                                     R2(unlist(i),test_set$ga_as))),
             RMSE = sapply(ga_pred, function(i) ifelse(length(i)==1,
                                                       NA,
                                                       RMSE(unlist(i),test_set$ga_as))))
  
})

bootstrap <- Reduce(rbind,bootstrap) %>% na.omit
bootstrap2<-bootstrap
bootstrap$formula<-as.character(bootstrap$formula)
bootstrap_stats <- sapply(unique(bootstrap$formula), function(i){
  c(formula = i ,
    seed_count = nrow(bootstrap[bootstrap$formula == i,]),
    var_count = sum(sapply(features,function(j) grepl(j,i))),
    R2 = mean(bootstrap$R2[bootstrap$formula ==i]),
    RMSE = mean(bootstrap$RMSE[bootstrap$formula ==i]))
}, USE.NAMES = FALSE) %>% t() %>% as.data.frame()

bootstrap_stats$seed_count <- as.numeric(as.character(bootstrap_stats$seed_count))
bootstrap_stats$var_count <- as.integer(as.character(bootstrap_stats$var_count))
bootstrap_stats$R2 <- as.numeric(as.character(bootstrap_stats$R2)) %>% round(digits = 3)
bootstrap_stats$RMSE <- as.numeric(as.character(bootstrap_stats$RMSE)) %>% round(digits = 3)
bootstrap_stats<-data.frame(bootstrap_stats)
bootstrap_stats <- bootstrap_stats[bootstrap_stats$seed_count > 0.8*length(seed_range),]

n <- nrow(dat)
p <- sapply(1:nrow(bootstrap_stats),function(i){
  if(grepl("polym",bootstrap_stats$formula[i])){
    if(grepl("degree = 2", bootstrap_stats$formula[i])){
      (bootstrap_stats$var_count[i] + 1)*bootstrap_stats$var_count[i]/2 #nC2 terms = n(n-1)/2 for polym 2
    }else if(grepl("degree = 3", bootstrap_stats$formula[i])){
      (bootstrap_stats$var_count[i] + 1)*bootstrap_stats$var_count[i]*(bootstrap_stats$var_count[i]-1)/6 #nC3 terms = n(n-1)(n-2)/6 for polym 3
    }else if(grepl("degree = 4", bootstrap_stats$formula[i])){
      (bootstrap_stats$var_count[i] + 1)*bootstrap_stats$var_count[i]*(bootstrap_stats$var_count[i]-1)*(bootstrap_stats$var_count[i]-2)/24 
    }
  }else{
    bootstrap_stats$var_count[i] + 1
  }
})
bootstrap_stats$AdjR2 <- round(1 - (1-bootstrap_stats$R2)*(n-1)/(n-p), digits = 4)
rm(n,p)

bootstrap_stats <- bootstrap_stats[order(bootstrap_stats$AdjR2, decreasing = T),!(colnames(bootstrap_stats) %in% c("seed_count"))]
write.csv(bootstrap_stats, './bootstrap_stats2.csv')

bootstrap_stats2<-bootstrap_stats
bootstrap_stats<-bootstrap_stats[1:10,]

data_test <- read_dta('test.dta')
data_test<-data_test[,c(features,'ga_as')]
data_test<-data_test[complete.cases(data_test),]

test_formulas <- data.frame(formula = bootstrap_stats$formula ,
                            error = t(sapply(bootstrap_stats$formula, function(formula){
                              model <- lm(as.formula(as.character(formula)), data = dat)
                              predict(model, data_test) - data_test$ga_as
                            }))) 


ga_pred <- lapply(formulas, function(f){
  tryCatch(expr = predict(lm(formula = as.formula(f), data = dat), data_test),
           error = function(e) NA,
           warning = function(w) "")
})
test_result<-data.frame(seed_value,
                        formula = formulas,
                        R2 = sapply(ga_pred, function(i) ifelse(length(i)==1,
                                                                NA,
                                                                R2(unlist(i),test_set$ga))),
                        RMSE = sapply(ga_pred, function(i) ifelse(length(i)==1,
                                                                  NA,
                                                                  RMSE(unlist(i),test_set$ga))))

test_R2<-sapply(bootstrap_stats$formula, function(f){
  R2(predict(lm(as.formula(as.character(f)),dat), data_test),data_test$ga_as)  
})
test_R2<-data.frame(formula = bootstrap_stats$formula,
                    R2=sapply(bootstrap_stats$formula, function(f){
                      R2(predict(lm(as.formula(as.character(f)),dat), data_test),data_test$ga_as)  
                    }) %>% round(3),
                    var_count=bootstrap_stats$var_count)
test_formulas <- melt(test_formulas, id.vars = "formula")
colnames(test_formulas) <- c("formula","sample","error")
plot <- ggplot(test_formulas, aes(y=error, x= formula))+
  geom_violin(fill = "black")+ggtitle("Distribution across formulas")+
  scale_y_continuous(breaks = -8:8, limits = c(-8,8))+ theme_bw()+
  theme(legend.position = "none") + coord_flip()

ggsave(filename = "bf_plots2.svg", 
       plot = plot,
       height = 14,
       width = 7)

