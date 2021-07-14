seed_value <-  666
seed_range <- 100:199

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

df <- read_dta('./datasets/train_23.dta')
dat <- df


formulas <- expand.grid("ga ~ " ,
                        c("","hc","log(1+hc) ","sqrt(hc) "),
                        c("","+ ac ","+ log(1+ac) ", "+ sqrt(ac) "),
                        c("","+ fl ","+ log(1+fl) ", "+ sqrt(fl) "),
                        c("","+ ofd ","+ log(1+ofd)","+ sqrt(ofd)"),
                        c("","+ bpd","+ log(1+bpd)","+ sqrt(bpd)"),
                        c("", ", degree = 2)", ", degree = 3)", ", degree = 4)"),
                        stringsAsFactors = FALSE) %>%
  apply(1,function(i)paste0(i,collapse=""))
formulas <- formulas[formulas != "ga ~ "]
formulas[grep("degree", formulas)] <- paste0(gsub(" \\+ ",
                                                  ", ",
                                                  formulas[grep("degree", formulas)]))
formulas[grep("degree", formulas)] <- sub("~|~,", "~ polym(",formulas[grep("degree", formulas)] )
# formulas <- formulas[!(grepl("anc_cur_wt|hght",formulas) & grepl("bmi",formulas))]

bootstrap <- lapply(seed_range, function(seed_value){
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
                                                     R2(unlist(i),               test_set$ga))),
             RMSE = sapply(ga_pred, function(i) ifelse(length(i)==1,
                                                       NA,
                                                       RMSE(unlist(i),test_set$ga))))
  
})

bootstrap <- Reduce(rbind,bootstrap) %>% na.omit
bootstrap2<-bootstrap
bootstrap$formula<-as.character(bootstrap$formula)
bootstrap_stats <- sapply(unique(bootstrap$formula), function(i){
  c(formula = i ,
    seed_count = nrow(bootstrap[bootstrap$formula == i,]),
    var_count = sum(sapply(c('bpd','ofd','fl','hp','ap'),function(j) grepl(j,i))),
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


