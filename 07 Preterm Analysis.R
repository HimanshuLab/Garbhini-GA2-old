####################################################################################################

#                       PRETERM ANALYSIS

####################################################################################################

test <- read_dta('./datasets/test_23.dta')
preterm_test <- read_dta('./datasets/predicted_test.dta')

preterm_test <- preterm_test + (test$ga_birth - test$ga)
preterm_test$ga <- test$ga_birth

# 95% Confidence intervals with percentage preterm births
conf_test <- preterm_test %>% sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% round(3)
apply(preterm_test,2,function(i) sum(i < 37)*100/length(i)) %>% range()

conf_test #table displaying % with upper and lower limits for 95% CI

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



colnames(preterm_test) <- c("Garbhini-GA2", "Garbhini-XGB", "Hadlock","Intergrowth-21st", "Gold Standard")

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

figure_s3b <- table_preterm_test %>%
  mutate(formula = fct_relevel(formula, "Hadlock","Intergrowth-21st","Garbhini-XGB","Garbhini-GA2","Gold Standard")) %>%
  ggplot(aes(x = formula, y = PTB_rate)) +
  #ggplot(table_preterm_test, aes(x = formula, y = PTB_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .2, size = 1) +
  coord_flip() +
  labs(title = 'PTB rates calculated by models on test dataset', y = 'PTB rate', x = 'Model') +
  theme_bw(base_size = 13) 

figure_s3b

#saving Figure S3B
ggsave(file="./figures/SVG/Figure_S3_B.svg", plot=figure_s3b, width=10, height=8)
ggsave(file="./figures/EPS/Figure_S3_B.eps", plot=figure_s3b, width=10, height=8)
ggsave(file="./figures/pdf/Figure_S3_B.pdf", plot=figure_s3b, width=10, height=8)

table_preterm_test <- preterm_test %>% 
  select(-c('Garbhini-XGB')) %>% 
  sapply(function(i) BinomCI(sum(i < 37),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% 
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

figure_3b <- table_preterm_test %>%
  mutate(formula = fct_relevel(formula, "Hadlock","Intergrowth-21st","Garbhini-GA2","Gold Standard")) %>%
  ggplot(aes(x = formula, y = PTB_rate)) +
  #ggplot(table_preterm_test, aes(x = formula, y = PTB_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .2, size = 1) +
  coord_flip() +
  labs(title = 'PTB rates calculated by models on test dataset', y = 'PTB rate', x = 'Model') +
  theme_bw(base_size = 13) 

figure_3b

ggsave(file = './figures/SVG/Figure_3B.svg', plot = figure_3b, width = 10, height = 8)
ggsave(file = './figures/EPS/Figure_3B.eps', plot = figure_3b, width = 10, height = 8)
ggsave(file = './figures/PDF/Figure_3B.pdf', plot = figure_3b, width = 10, height = 8)

####################################################################################################

#                       JACARD SIMILARITY ANALYSIS

####################################################################################################

preterm_class <- preterm_test
preterm_class[preterm_class < 37] <- 1
preterm_class[!preterm_class < 37] <- 0
preterm_vals <- sapply(colnames(preterm_test), function(i){
  sapply(colnames(preterm_test), function(j){
    round((nrow(preterm_class[ which(preterm_class[,i] == 1 & preterm_class[,j] == 1) , ])*100)/nrow(preterm_class[ which(preterm_class[,i] == 1 | preterm_class[,j] == 1) , ]),3)
  })
}) %>% data.frame()

preterm_vals


####################################################################################################

#                       QUADRANT PLOTS

####################################################################################################

colnames(preterm_test) <- c("random_forest", "xg_boost", "hadlock", "intergrowth", "ga")

preterm_plot_h <- preterm_test[c("hadlock", "ga")]
preterm_plot_h[preterm_plot_h$hadlock>=37 & preterm_plot_h$ga>=37,'level'] <- 'Term'
preterm_plot_h[preterm_plot_h$hadlock>=37 & preterm_plot_h$ga<37,'level'] <- 'Preterm, GA'
preterm_plot_h[preterm_plot_h$hadlock<37 & preterm_plot_h$ga>=37,'level'] <- 'Preterm, Hadlock'
preterm_plot_h[preterm_plot_h$hadlock<37 & preterm_plot_h$ga<37,'level'] <- 'Preterm'


plot_preterm_h <- ggplot(preterm_plot_h) +
  geom_point(aes(x=hadlock, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=37, linetype="dashed", size=0.5) +
  geom_vline(xintercept=37, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Term" = "darkgreen", "Preterm" = "red", "Preterm, GA" = "blue", "Preterm, Hadlock" = "orange"))+
  labs(x="GA by Hadlock at birth (weeks)", y="True GA (weeks)", 
       color="PTB by formula", caption=paste("n = 1430"))+
  ggtitle('True GA vs Hadlock')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_preterm_h


preterm_plot_i <- preterm_test[c("intergrowth", "ga")]
preterm_plot_i[preterm_plot_i$intergrowth>=37 & preterm_plot_i$ga>=37,'level']<-'Term'
preterm_plot_i[preterm_plot_i$intergrowth>=37 & preterm_plot_i$ga<37,'level']<-'Preterm, GA'
preterm_plot_i[preterm_plot_i$intergrowth<37 & preterm_plot_i$ga>=37,'level']<-'Preterm, Intergrowth'
preterm_plot_i[preterm_plot_i$intergrowth<37 & preterm_plot_i$ga<37,'level']<-'Preterm'



plot_preterm_i<-ggplot(preterm_plot_i) +
  geom_point(aes(x=intergrowth, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=37, linetype="dashed", size=0.5) +
  geom_vline(xintercept=37, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Term" = "darkgreen", "Preterm" = "red", "Preterm, GA" = "blue", "Preterm, Intergrowth" = "orange"))+
  labs(x="GA by intergrowth at birth (weeks)", y="True GA (weeks)", 
       color="PTB by formula", caption=paste("n = 1430"))+
  ggtitle('True GA vs Intergrowth')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_preterm_i



preterm_plot <- preterm_test[c("random_forest", "ga")]
preterm_plot[preterm_plot$random_forest>=37 & preterm_plot$ga>=37,'level']<-'Term'
preterm_plot[preterm_plot$random_forest>=37 & preterm_plot$ga<37,'level']<-'Preterm, GA'
preterm_plot[preterm_plot$random_forest<37 & preterm_plot$ga>=37,'level']<-'Preterm, Garbhini-GA2'
preterm_plot[preterm_plot$random_forest<37 & preterm_plot$ga<37,'level']<-'Preterm'



plot_preterm<-ggplot(preterm_plot) +
  geom_point(aes(x=random_forest, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=37, linetype="dashed", size=0.5) +
  geom_vline(xintercept=37, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Term" = "darkgreen", "Preterm" = "red", "Preterm, GA" = "blue", "Preterm, Garbhini-GA2" = "orange"))+
  labs(x="GA by Garbhini-GA2 at birth (weeks)", y="True GA (weeks)", 
       color="PTB by formula", caption=paste("n = 1430"))+
  ggtitle('True GA vs Garbhini-GA2')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_preterm


preterm_plot_xgb <- preterm_test[c("xg_boost", "ga")]
preterm_plot_xgb[preterm_plot_xgb$xg_boost>=37 & preterm_plot_xgb$ga>=37,'level'] <- 'Term'
preterm_plot_xgb[preterm_plot_xgb$xg_boost>=37 & preterm_plot_xgb$ga<37,'level'] <- 'Preterm, GA'
preterm_plot_xgb[preterm_plot_xgb$xg_boost<37 & preterm_plot_xgb$ga>=37,'level'] <- 'Preterm, Garbhini:XG Boost'
preterm_plot_xgb[preterm_plot_xgb$xg_boost<37 & preterm_plot_xgb$ga<37,'level'] <- 'Preterm'

plot_preterm_xgb <- ggplot(preterm_plot_xgb) +
  geom_point(aes(x=xg_boost, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=37, linetype="dashed", size=0.5) +
  geom_vline(xintercept=37, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Term" = "darkgreen", "Preterm" = "red", "Preterm, GA" = "blue", "Preterm, Garbhini:XG Boost" = "orange"))+
  labs(x="GA by xg_boost at birth (weeks)", y="True GA (weeks)", 
       color="PTB by formula", caption=paste("n =",nrow(test)))+
  ggtitle('Classification of PTB: True GA vs Garbhini:XG Boost')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot_preterm_xgb

ggsave(file="./figures/SVG/Figure_3C_1.svg", plot = plot_preterm, width=8, height=8)
ggsave(file="./figures/SVG/Figure_3C_2.svg", plot = plot_preterm_i, width=8, height=8)
ggsave(file="./figures/SVG/Figure_3C_3.svg", plot = plot_preterm_h, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S3_C.svg", plot = plot_preterm_xgb, width=8, height=8)

ggsave(file="./figures/EPS/Figure_3C_1.eps", plot=plot_preterm, width=8, height=8)
ggsave(file="./figures/EPS/Figure_3C_2.eps", plot=plot_preterm_i, width=8, height=8)
ggsave(file="./figures/EPS/Figure_3C_3.eps", plot=plot_preterm_h, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S3_C.eps", plot = plot_preterm_xgb, width=8, height=8)

ggsave(file="./figures/PDF/Figure_3C_1.pdf", plot=plot_preterm, width=8, height=8)
ggsave(file="./figures/PDF/Figure_3C_2.pdf", plot=plot_preterm_i, width=8, height=8)
ggsave(file="./figures/PDF/Figure_3C_3.pdf", plot=plot_preterm_h, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S3_C.pdf", plot = plot_preterm_xgb, width=8, height=8)






