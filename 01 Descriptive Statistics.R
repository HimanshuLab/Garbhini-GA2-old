# Results 1: Generate Descriptive statistics (Table 1) for all participants

library(haven)
library(magrittr) #used for the pipe operator %>% which feeds the output of the previous function to the next function as . or assigns to first parameter by default 
library(ggplot2)
library(dplyr)
library(tidyverse)

if (!("data_raw" %in% ls())) {
  data_raw <- read.csv("./datasets/merged_final_dataset.csv",na.strings = c(""," ","NA","88","99", "99.99","999.99","777.77"),header = TRUE )   
  #old dataset: data_raw <- read_csv("20NOV2018.csv")
  #data_raw[data_raw %in% c(""," ","NA","88","99", "99.99","777.77")] <- NA
}

data_age <- read.csv("./datasets/data_raw_enrids_age_sga.csv")


# custom function definitions ---------------------------------------------

return_median_IQR <- function(x){
  paste0("\t",median(x, na.rm = T) %>% round(digits = 2),
         "\t(", quantile(x,na.rm = T)[2] %>% round(digits = 2),
         ",",quantile(x,na.rm = T)[4] %>% round(digits = 2),")")
}
return_mean_sd <- function(x){
  paste0("\t",mean(x, na.rm = T) %>% round(digits = 3),"±", sd(x, na.rm = T) %>% round(digits = 2))
}
return_percentages <- function(x){
  (table(x)*100/length(x)) %>% round(3) %>% {
    paste0("\t",names(.),"\t",.,"%", collapse = "\n")
  } 
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## TRAIN DATASET ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  

train_df = read_dta('./train_23.dta')
train_df <- train_df %>% distinct(enrid, .keep_all = TRUE)

train_var <- train_df %>%
  left_join(data_raw) %>%
  select(bmi, 
         hemglo, 
         hght, 
         derived_ses_mks_2019, 
         derived_parity, rlgn, 
         head_edu, 
         part_occ,
         rlgn,
         smok_prs,
         fuel, drnk_wtr, htn, diab, hypothy, hyprthy, prclm, swld, high_bp,
         contr_bcp)

age = train_df %>%
  left_join(data_age) %>%
  select(derived_age_approx_age)

train_var$derived_age_approx_age = age$derived_age_approx_age

file.create("tables/Table1_train_data.txt")
write(x = "#Age", file = "tables/Table1_train_data.txt" , append = T)
write(x = return_median_IQR(train_var$derived_age_approx_age),file = "tables/Table1_train_data.txt" ,append = T )

write(x = "#GA estational at enrolment ", file = "tables/Table1_train_data.txt" ,
      append = T)
write(x= return_mean_sd(train_df$ga),file = "tables/Table1_train_data.txt" ,
      append = T )


write(x = "#BMI ", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$bmi %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if(i < 18.5){
    "Underweight"
  }else if(i >= 18.5 & i < 25){
    "Normal"
  }else if( i >= 25 & i < 30){
    "Obese"
  } else "Overweight"
})  %>% return_percentages() %>%
  write(x= . ,file = "tables/Table1_train_data.txt" ,
        append = T )

write(x = "#Haemoglobin  ", file = "tables/Table1_train_data.txt" ,
      append = T)
write(x= return_median_IQR(train_var$hemglo),file = "tables/Table1_train_data.txt" ,
      append = T )

write(x = "#Height   ", file = "tables/Table1_train_data.txt" ,
      append = T)
write(x= return_median_IQR(train_var$hght),file = "tables/Table1_train_data.txt" ,
      append = T )

write(x = "#Socioeconomic Status ", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$derived_ses_mks_2019 %>%  as.character %>% 
  { 
    .[is.na(.)] <- "Undetermined"
    .
  } %>% return_percentages() %>%
  write(x= .,file = "tables/Table1_train_data.txt" ,
        append = T )

write(x = "# Parity", file = "tables/Table1_train_data.txt" ,
      append = T)
write(x= return_percentages(train_var$derived_parity),file = "tables/Table1_train_data.txt" ,
      append = T )

write(x = "# Education", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$head_edu %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Illiterate"
  }else if ( i == 12){
    "Literate or primary school  "
  }else if ( i == 13){
    "Middle school "
  }else if ( i == 14){
    "High school "
  }else if ( i == 15){
    "Post high school diploma "
  }else if ( i == 16){
    "Graduate "
  }else if ( i == 17){
    "Post-graduate "
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_train_data.txt" ,
        append = T )

write(x = "# Occupation", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$part_occ %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Unemployed"
  }else if ( i == 12){
    "Unskilled worker "
  }else if ( i == 13){
    "Semi-skilled worker "
  }else if ( i == 14){
    "Skilled worker "
  }else if ( i == 15){
    "Clerk, shop, farm owner "
  }else if ( i == 16){
    "Semi-professional "
  }else if ( i == 17){
    "Professional "
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_train_data.txt" ,
        append = T )

write(x = "# Religion", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$rlgn %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Hindu"
  }else if ( i == 12){
    "Muslim"
  }else if ( i == 13){
    "Sikh"
  }else if ( i == 14){
    "Christian"
  }else if ( i == 15){
    "Buddhist"
  }else if ( i == 16){
    "Other"
  }else if ( i == 17){
    "More than one"
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_train_data.txt" ,
        append = T )

write(x = "#Secondhand tobacco smoke", file = "tables/Table1_train_data.txt" ,
      append = T)
train_var$smok_prs %>% sapply(function(i){
  if(is.na(i)){
    "Undertermined"
  }else if(i == 1){
    "Exposed"
  }else if(i ==2){
    "Unexposed"
  }
}) %>% return_percentages() %>%
  write(x= .,file = "tables/Table1_train_data.txt" ,
        append = T )

train_var %>% 
  mutate(safe_fuel = ifelse(fuel == 11, 1,0),
         safe_water = ifelse(drnk_wtr == 11 | drnk_wtr == 19, 1, 0),
         chro_ill = ifelse(htn==1 | diab==1 |hypothy==1|hyprthy==1,1,0),
         past_htp = if_else(prclm==1|(swld>30 & high_bp==1),1,0,missing = 0)) %>% 
  select(safe_fuel, safe_water, chro_ill, past_htp) %>% colMeans(na.rm = TRUE) %>%
  round(4) %>% {data.frame(col = names(.), val = ., stringsAsFactors = FALSE)} %>%
  apply(1,function(i){
    write(x = paste0("# ",i[1],"\n\t","TRUE = ", 100*as.numeric(i[2]) ,"\n\tFALSE = ",100*(1-as.numeric(i[2]))),
          file = "tables/Table1_train_data.txt" ,
          append = T )
  }) %>% invisible()

write(x = "#History of contraceptive use", file = "tables/Table1_train_data.txt" ,
      append = T)
write(x = paste0("\n\t", "TRUE = ", round(length(train_var[(train_var$contr_bcp == 1),]$contr_bcp)/6498 * 100, 2)), 
      file = "tables/Table1_train_data.txt", 
      append = T)
write(x = paste0("\n\t", "FALSE = ", round(length(train_var[(train_var$contr_bcp == 2),]$contr_bcp)/6498 *100, 2)), 
      file = "tables/Table1_train_data.txt", 
      append = T)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## TEST DATASET ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

test_df = read_dta('./test_23.dta')
test_df <- test_df %>% distinct(enrid, .keep_all = TRUE)

test_var <- test_df %>%
  left_join(data_raw) %>%
  select(bmi, 
         hemglo, 
         hght, 
         derived_ses_mks_2019, 
         derived_parity, rlgn, 
         head_edu, 
         part_occ,
         rlgn,
         smok_prs,
         fuel, drnk_wtr, htn, diab, hypothy, hyprthy, prclm, swld, high_bp,
         contr_bcp)

file.create("tables/Table1_test_data.txt")

age = test_df %>%
  left_join(data_age) %>%
  select(derived_age_approx_age)

test_var$derived_age_approx_age = age$derived_age_approx_age

write(x = "#Age", file = "tables/Table1_test_data.txt" , append = T)
write(x= return_median_IQR(age$derived_age_approx_age),file = "tables/Table1_test_data.txt" ,append = T )

write(x = "#GA estational at enrolment ", file = "tables/Table1_test_data.txt" ,
      append = T)
write(x= return_mean_sd(test_df$ga),file = "tables/Table1_test_data.txt" ,
      append = T )


write(x = "#BMI ", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$bmi %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if(i < 18.5){
    "Underweight"
  }else if(i >= 18.5 & i < 25){
    "Normal"
  }else if( i >= 25 & i < 30){
    "Obese"
  } else "Overweight"
})  %>% return_percentages() %>%
  write(x= . ,file = "tables/Table1_test_data.txt" ,
        append = T )

write(x = "#Haemoglobin  ", file = "tables/Table1_test_data.txt" ,
      append = T)
write(x= return_median_IQR(test_var$hemglo),file = "tables/Table1_test_data.txt" ,
      append = T )

write(x = "#Height   ", file = "tables/Table1_test_data.txt" ,
      append = T)
write(x= return_median_IQR(test_var$hght),file = "tables/Table1_test_data.txt" ,
      append = T )

write(x = "#Socioeconomic Status ", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$derived_ses_mks_2019 %>%  as.character %>% 
  { 
    .[is.na(.)] <- "Undetermined"
    .
  } %>% return_percentages() %>%
  write(x= .,file = "tables/Table1_test_data.txt" ,
        append = T )

write(x = "# Parity", file = "tables/Table1_test_data.txt" ,
      append = T)
write(x= return_percentages(test_var$derived_parity),file = "tables/Table1_test_data.txt" ,
      append = T )

write(x = "# Education", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$head_edu %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Illiterate"
  }else if ( i == 12){
    "Literate or primary school  "
  }else if ( i == 13){
    "Middle school "
  }else if ( i == 14){
    "High school "
  }else if ( i == 15){
    "Post high school diploma "
  }else if ( i == 16){
    "Graduate "
  }else if ( i == 17){
    "Post-graduate "
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_test_data.txt" ,
        append = T )

write(x = "# Occupation", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$part_occ %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Unemployed"
  }else if ( i == 12){
    "Unskilled worker "
  }else if ( i == 13){
    "Semi-skilled worker "
  }else if ( i == 14){
    "Skilled worker "
  }else if ( i == 15){
    "Clerk, shop, farm owner "
  }else if ( i == 16){
    "Semi-professional "
  }else if ( i == 17){
    "Professional "
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_test_data.txt" ,
        append = T )

write(x = "# Religion", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$rlgn %>% sapply(function(i){
  if(is.na(i)){
    NA
  }else if ( i == 11){
    "Hindu"
  }else if ( i == 12){
    "Muslim"
  }else if ( i == 13){
    "Sikh"
  }else if ( i == 14){
    "Christian"
  }else if ( i == 15){
    "Buddhist"
  }else if ( i == 16){
    "Other"
  }else if ( i == 17){
    "More than one"
  }
}) %>% return_percentages() %>% 
  write(x= .,file = "tables/Table1_test_data.txt" ,
        append = T )

write(x = "#Secondhand tobacco smoke", file = "tables/Table1_test_data.txt" ,
      append = T)
test_var$smok_prs %>% sapply(function(i){
  if(is.na(i)){
    "Undertermined"
  }else if(i == 1){
    "Exposed"
  }else if(i ==2){
    "Unexposed"
  }
}) %>% return_percentages() %>%
  write(x= .,file = "tables/Table1_test_data.txt" ,
        append = T )

test_var %>% 
  mutate(safe_fuel = ifelse(fuel == 11, 1,0),
         safe_water = ifelse(drnk_wtr == 11 | drnk_wtr == 19, 1, 0),
         chro_ill = ifelse(htn==1 | diab==1 |hypothy==1|hyprthy==1,1,0),
         past_htp = if_else(prclm==1|(swld>30 & high_bp==1),1,0,missing = 0)) %>% 
  select(safe_fuel, safe_water, chro_ill, past_htp) %>% colMeans(na.rm = TRUE) %>%
  round(4) %>% {data.frame(col = names(.), val = ., stringsAsFactors = FALSE)} %>%
  apply(1,function(i){
    write(x = paste0("# ",i[1],"\n\t","TRUE = ", 100*as.numeric(i[2]) ,"\n\tFALSE = ",100*(1-as.numeric(i[2]))),
          file = "tables/Table1_test_data.txt" ,
          append = T )
  }) %>% invisible()

write(x = "#History of contraceptive use", file = "tables/Table1_test_data.txt" ,
      append = T)
write(x = paste0("\n\t", "TRUE = ", round(length(test_var[(test_var$contr_bcp == 1),]$contr_bcp)/6498 * 100, 2)), 
      file = "tables/Table1_test_data.txt", 
      append = T)
write(x = paste0("\n\t", "FALSE = ", round(length(test_var[(test_var$contr_bcp == 2),]$contr_bcp)/6498 *100, 2)), 
      file = "tables/Table1_test_data.txt", 
      append = T)

