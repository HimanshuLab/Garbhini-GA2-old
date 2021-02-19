# Script to clean dataset and split into train and test

library(readr)
library(dplyr)
library(foreign)


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

########  CLEANING FUNCTIONS FOR REMOVING 77777, 88888, 999999

clean_continous_variables_for_age_and_pog <- function(x){
  x <- as.character(x)
  z <- gsub('\\..*','',x)
  for(i in 1:length(z)){
    y <- unlist(strsplit(z[i],split = ''))
    if(length(y) >1 & length(unique(y)) == 1 & unique(y) %in% c(7,8,9)){
      x[i] <-  NA
    }
  }
  return(as.numeric(x))
}

clean_continous_variables <- function(x){
  x <- as.character(x)
  z <- gsub('\\..*','',x)
  for(i in 1:length(z)){
    y <- unlist(strsplit(z[i],split = ''))
    y <- unlist(strsplit(z[i],split = ''))
    if(length(y) >1 & length(unique(y)) == 1 & unique(y) %in% c(7,8,9)){
      x[i] <-  NA
    }
   }
  x <- as.numeric(x)
  x[x==0]<-NA
  return(x)
}



if(!("data_raw" %in% ls())){
  data_raw <- read_csv("./data/merged_final_dataset.csv",
                       na = c(""," ","NA","88","99", "99.99","999.99","777.77"),
                       col_names = TRUE ) %>% as.data.frame()
}

features <- readLines("./data/features_list.txt")
df <- data_raw[, (colnames(data_raw) %in% c(features))] %>% as.data.frame()

numeric <- colnames(df)[sapply(1:ncol(df), function(i) class(df[,i]) == "numeric")]
#cleaning numeric values 
for( x in numeric){
  df[,x] <- clean_continous_variables(df[,x])
}

rm(x,numeric)

#Removing participants that do not have crl value from first trimester
crl_present <- df[grepl("^crl_qc_",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
ds_present <- df[grepl("^vdt_ds",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
em_present <- df[grepl("^vdt_em",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
birth <- (!is.na(df$op_dt_event))
is_singleton <- (df$gest_ds1 == 1 | df$fetal_num_as1 == 1 | df$fetal_num_fwb1 ==1 | df$fetal_num_fwbv1 ==1 | is.na(df$fetal_num_as1) | is.na(df$gest_ds1) | is.na(df$fetal_num_fwb1) | is.na(df$fetal_num_fwbv1))
df <- df[(crl_present & (ds_present| em_present)) & birth & is_singleton,]

#Removing participants that do not have vdt in as or fwbv1 or fwb
as_present <- df[grepl("^vdt_as1",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
fwb_present <- df[grepl("^vdt_fwb1",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
fwbv_present <- df[grepl("^vdt_fwbv1",colnames(df))] %>%
  apply(1,function(i) !all(is.na(i))) 
#NAs are treated as true values

df <- df[(as_present | fwb_present | fwbv_present ),]

#using Garbhini-1
predict_ga <- function(crl,
                       method = c("Hadlock",
                                  "McLennan-Schluter",
                                  "Robinson-Fleming",
                                  "Sahota",
                                  "Verburg",
                                  "Intergrowth",
                                  "Garbhini1")){
  ga <- numeric()
  if("Hadlock" %in% method){
    ga <- c(ga,Hadlock = exp(1.684969+0.315646*crl-0.049306*(crl^2)+0.004057*(crl^3)-0.0001204568*(crl^4)))
  }
  if("McLennan-Schluter" %in% method){
    ga <- c(ga,McLennan = (32.61967 + (2.62975*crl*10)-(0.42399*log(crl*10)*(crl*10)))/7)
  }
  if("Robinson-Fleming" %in% method){
    ga <- c(ga,Robinson = (8.052*sqrt(crl*10)+23.73)/7)
  }
  if("Sahota" %in% method){
    ga <- c(ga,Sahota = (26.643+7.822*sqrt(crl*10))/7)
  }
  if("Verburg" %in% method){
    ga <- c(ga,Verburg = exp(1.4653 + 0.001737*crl*10 + 0.2313*log(crl*10)))
  }
  if("Intergrowth" %in% method){
    ga <- c(ga,Intergrowth = (40.9041 + 3.2 * (crl*10)^0.5 + 0.348956*crl*10 )/7)
  }
  if("Garbhini1" %in% method){
    ga <- c(ga,Garbhini1 = 6.73526 + 1.15018*(crl) -0.02294*(crl^2))  #new formula
  }  
  return(ga)
}

for( i in grep("^vdt_ds\\d$|^vdt_em\\d$|^vdt_scr$|^vdt_as\\d$|^vdt_fwb\\d$|^vdt_fwbv\\d$",colnames(df),value=T)){
  df[,i] <- as.Date(df[,i] %>% unlist, format = "%Y-%m-%d" , origin = "1970-01-01")
}
for( i in grep("^op_dt_",colnames(df),value=T)){
  df[,i] <- as.Date(df[,i] %>% unlist, format = "%Y-%m-%d" , origin = "1970-01-01")
}

df$type <- ifelse((!is.na(df$crl_qc_ds111)&!is.na(df$vdt_ds1)), "ds1", 
                  ifelse((is.na(df$crl_qc_ds111) & !is.na(df$crl_qc_em1) & !is.na(df$vdt_em1)), "em1", "em2"))
table(df$type)


# Removing abortions and stillbirths
df$ga_first <- ifelse(df$type == 'ds1', predict_ga(df$crl_qc_ds111, "Garbhini1"), predict_ga(df$crl_qc_em1, "Garbhini1")) #in weeks

#add Garbhni-1 formula and calculate ga_birth
df$ga_birth <- ifelse(df$type == 'ds1', (df$ga_first+(df$op_dt_event-df$vdt_ds1)/7), (df$ga_first+(df$op_dt_event-df$vdt_em1)/7))

#removing abortions and still births
df <- df[(df$ga_birth >= 20) ,]
live_birth <- (df$op_sing_liv_birth == 1 | is.na(df$op_sing_liv_birth))
df <- df[(live_birth),]

rm(as_present,crl_present,ds_present,em_present,features,fwb_present,fwbv_present,i)




df<-df[is.na(df$vdt_fwb2)&is.na(df$vdt_fwb3),] 
df<-df[is.na(df$vdt_fwbv2),] 
df <- df[, -grep("fwbv2", colnames(df))]#163 columns
df <- df[, -grep("(fwb2|fwb3)", colnames(df))]#139 columns (24 each)
df<- df[, -grep("as3", colnames(df))]#130 columns



cnames_as<-colnames(df[,grep("(as1|as2)$", colnames(df))])
#removing variables that occur throughout as form only once
cnames_as<-setdiff(cnames_as, c('fetal_num_as1','left_ute_art_pl_as1','rght_ute_art_pl_as1')) 
cnames_norm<-setdiff(colnames(df), cnames_as) #removing as columns from main list
cnames_as <- unique(substr(cnames_as, 1,nchar(cnames_as)-4)) #removing suffix _as#
df_a<- df[is.na(df$vdt_as2), c(cnames_norm, paste0(cnames_as, "_as1"))] %>% setNames(c(cnames_norm,paste0(cnames_as, "_as"))) #2400
df_b<- df[!is.na(df$vdt_as2), c(cnames_norm, paste0(cnames_as, "_as2"))] %>% setNames(c(cnames_norm,paste0(cnames_as, "_as"))) #14 
# df_c<- df[, c(cnames_norm, paste0(cnames_as, "_as3"))] %>% setNames(c(cnames_norm,paste0(cnames_as, "_as"))) #none


#taking values only where vdt_as != NA
df_new <- rbind(df_a, df_b)
rm(df_a,df_b,cnames_as,cnames_norm)




df<-df_new


#SFH
df$symph_fund_hght_as <- (df$fir_mea_cms1 + df$sec_mea_cms1)/2
df$symph_fund_hght_fwb1 <- (df$fir_mea_cms3 + df$sec_mea_cms3)/2
df$symph_fund_hght_fwbv1 <- (df$fir_mea_cms4 + df$sec_mea_cms4)/2


df$ga_as<-NA
df$ga_fwb1 <- NA
df$ga_fwbv1 <- NA
df[!is.na(df$crl_qc_ds111),"ga_as"]<-(df[!is.na(df$crl_qc_ds111),"vdt_as"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Garbhini1")
df[!is.na(df$crl_qc_em1),"ga_as"]<-(df[!is.na(df$crl_qc_em1),"vdt_as"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Garbhini1")

df[!is.na(df$crl_qc_ds111),"ga_fwb1"]<-(df[!is.na(df$crl_qc_ds111),"vdt_fwb1"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Garbhini1")
df[!is.na(df$crl_qc_em1),"ga_fwb1"]<-(df[!is.na(df$crl_qc_em1),"vdt_fwb1"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Garbhini1")

df[!is.na(df$crl_qc_ds111),"ga_fwbv1"]<-(df[!is.na(df$crl_qc_ds111),"vdt_fwbv1"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Garbhini1")
df[!is.na(df$crl_qc_em1),"ga_fwbv1"]<-(df[!is.na(df$crl_qc_em1),"vdt_fwbv1"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Garbhini1")

# df[!is.na(df$crl_qc_ds111),"ga_birth"]<-(df[!is.na(df$crl_qc_ds111),"op_dt_event"]-df[!is.na(df$crl_qc_ds111),"vdt_ds1"])/7+predict_ga(df[!is.na(df$crl_qc_ds111),"crl_qc_ds111"], "Garbhini1")
# df[!is.na(df$crl_qc_em1),"ga_birth"]<-(df[!is.na(df$crl_qc_em1),"op_dt_event"]-df[!is.na(df$crl_qc_em1),"vdt_em1"])/7+predict_ga(df[!is.na(df$crl_qc_em1),"crl_qc_em1"], "Garbhini1")


as <- df%>%select(enrid,bpd_qc_as,ofd_qc_as,hp_qc_as,ap_qc_as,fl_qc_as,ga_as, symph_fund_hght_as, ga_birth)
as$type <- rep("AS",length(as$ga_as))
fwbv <- df%>%select(enrid,bpd_fwbv1,ofd_fwbv1,hp_fwbv1,ap_fwbv1,fl_fwbv1,ga_fwbv1, symph_fund_hght_fwbv1, ga_birth)
fwbv$type <- rep("FWBV",length(fwbv$ga_fwbv1))
fwb <- df%>%select(enrid,bpd_qc_fwb1,ofd_qc_fwb1,hp_qc_fwb1,ap_qc_fwb1,fl_qc_fwb1,ga_fwb1, symph_fund_hght_fwb1, ga_birth)
fwb$type <- rep("FWB",length(fwb$ga_fwb1))
colnames(as) <- c('enrid','bpd','ofd','hp','ap','fl','ga','sfh','ga_birth','visit')
colnames(fwbv) <- c('enrid','bpd','ofd','hp','ap','fl','ga','sfh','ga_birth','visit')
colnames(fwb) <- c('enrid','bpd','ofd','hp','ap','fl','ga','sfh','ga_birth','visit')

data <- rbind(as,fwbv,fwb)
data <- data[!is.na(data$ga),]
rm(as,df,data_raw,fwb,fwbv)

data <- data[data$ga > 0,]
data <- data[!is.na(data$bpd) & 
               !is.na(data$ofd) & 
               !is.na(data$hp) &
               !is.na(data$ap) &
               !is.na(data$fl)  ,] 


sets<-return_split_data(data, ratio=0.7, seed_value = 666)
train<-sets[[1]]
test<-sets[[2]]
write.dta(train,'data/train_23.dta')
write.dta(test, 'data/test_23.dta')

