####################################################################################################

#                       POST TERM ANALYSIS

####################################################################################################
test <- read_dta('./datasets/test_23.dta')
post_test <- read_dta('./datasets/predicted_test.dta')
post_test <- post_test + (test$ga_birth - test$ga)
post_test$ga <- test$ga_birth

# 95% Confidence intervals with percentage preterm births
conf_test <- post_test %>% sapply(function(i) BinomCI(sum(i > 42),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% round(3)
apply(post_test,2,function(i) sum(i > 42)*100/length(i)) %>% range()

conf_test #table displaying % with upper and lower limits for 95% CI

sapply(colnames(post_test), function(i){
  sapply(colnames(post_test), function(j){
    (c((post_test[,i] > 42) %>% table , (post_test[,j] > 42) %>% table) %>%
       matrix(nrow = 2) %>% 
       fisher.test())$p.value
  })
}) %>% (function(x) {
  x[lower.tri(x)] <- NA
  x
}) %>%  melt() %>% na.omit() %>% mutate( value = p.adjust(value, method = "bonferroni")) %>% filter(value < 0.05)



colnames(post_test) <- c("Garbhini-GA2", "Garbhini-XGB", "Hadlock","Intergrowth-21st", "Gold Standard")

table_post_test <- post_test %>% sapply(function(i) BinomCI(sum(i > 42),length(i), conf.level = 0.95, method = "clopper-pearson")) %>% 
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

table_post_test %>%
  mutate(formula = fct_relevel(formula, "Hadlock","Intergrowth-21st","Garbhini-XGB","Garbhini-GA2","Gold Standard")) %>%
  ggplot(aes(x = formula, y = PTB_rate)) +
  #ggplot(table_post_test, aes(x = formula, y = PTB_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher), width = .2, size = 1) +
  coord_flip() +
  labs(title = 'PTB rates calculated by models on test dataset', y = 'PTB rate', x = 'Model') +
  theme_bw(base_size = 13) 


####################################################################################################

#                       JACARD SIMILARITY ANALYSIS

####################################################################################################

post_class <- post_test
post_class[!(post_class > 42)] = 0
post_class[post_class > 42] = 1
post_vals <- sapply(colnames(post_test), function(i){
  sapply(colnames(post_test), function(j){
    round((nrow(post_class[ which(post_class[,i] == 1 & post_class[,j] == 1) , ])*100)/nrow(post_class[ which(post_class[,i] == 1 | post_class[,j] == 1) , ]),3)
  })
}) %>% data.frame()

post_vals


####################################################################################################

#                       QUADRANT PLOTS

####################################################################################################

colnames(post_test) <- c("random_forest", "xg_boost", "hadlock", "intergrowth", "ga")

postdated_plot_h <- post_test[c("hadlock", "ga")]
postdated_plot_h[postdated_plot_h$hadlock<=42 & postdated_plot_h$ga<=42,'level']<-'Not Post-dated'
postdated_plot_h[postdated_plot_h$hadlock<=42 & postdated_plot_h$ga>42,'level']<-'Post-dated, GA'
postdated_plot_h[postdated_plot_h$hadlock>42 & postdated_plot_h$ga<=42,'level']<-'Post-dated, Hadlock'
postdated_plot_h[postdated_plot_h$hadlock>42 & postdated_plot_h$ga>42,'level']<-'Post-dated'

plot_postdated_h<-ggplot(postdated_plot_h) +
  geom_point(aes(x=hadlock, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=42, linetype="dashed", size=0.5) +
  geom_vline(xintercept=42, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Not Post-dated" = "darkgreen", "Post-dated" = "red", "Post-dated, GA" = "blue", "Post-dated, Hadlock" = "orange"))+
  labs(x="GA by Hadlock at birth (weeks)", y="True GA (weeks)", 
       color="Post-dated by formula", caption=paste("n =",nrow(post_test)))+
  ggtitle('True GA vs Hadlock')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_postdated_h

postdated_plot_i<-post_test[c("intergrowth", "ga")]
postdated_plot_i[postdated_plot_i$intergrowth<=42 & postdated_plot_i$ga<=42,'level']<-'Normal'
postdated_plot_i[postdated_plot_i$intergrowth<=42 & postdated_plot_i$ga>42,'level']<-'Post-dated, GA'
postdated_plot_i[postdated_plot_i$intergrowth>42 & postdated_plot_i$ga<=42,'level']<-'Post-dated, Intergrowth'
postdated_plot_i[postdated_plot_i$intergrowth>42 & postdated_plot_i$ga>42,'level']<-'Post-dated'

plot_postdated_i<-ggplot(postdated_plot_i) +
  geom_point(aes(x=intergrowth, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=42, linetype="dashed", size=0.5) +
  geom_vline(xintercept=42, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Normal" = "darkgreen", "Post-dated" = "red", "Post-dated, GA" = "blue", "Post-dated, Intergrowth" = "orange"))+
  labs(x="GA by Intergrowth at birth (weeks)", y="True GA (weeks)", 
       color="Post-dated by formula", caption=paste("n =",nrow(post_test)))+
  ggtitle('True GA vs Intergrowth-21st')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_postdated_i

postdated_plot<-post_test[c("random_forest", "ga")]
postdated_plot[postdated_plot$random_forest<=42 & postdated_plot$ga<=42,'level']<-'Normal'
postdated_plot[postdated_plot$random_forest<=42 & postdated_plot$ga>42,'level']<-'Post-dated, GA'
postdated_plot[postdated_plot$random_forest>42 & postdated_plot$ga<=42,'level']<-'Post-dated, Random_forest'
postdated_plot[postdated_plot$random_forest>42 & postdated_plot$ga>42,'level']<-'Post-dated'

plot_postdated<-ggplot(postdated_plot) +
  geom_point(aes(x=random_forest, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=42, linetype="dashed", size=0.5) +
  geom_vline(xintercept=42, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Normal" = "darkgreen", "Post-dated" = "red", "Post-dated, GA" = "blue", "Post-dated, Garbhini-GA2" = "orange"))+
  labs(x="GA by Garbhini-GA2 at birth (weeks)", y="True GA (weeks)", 
       color="Post-dated by formula", caption=paste("n =",nrow(post_test)))+
  ggtitle('True GA vs Garbhini-GA2')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_postdated

postdated_plot_x<-post_test[c("xg_boost", "ga")]
postdated_plot_x[postdated_plot_x$xg_boost <=42 & postdated_plot_x$ga<=42,'level']<-'Normal'
postdated_plot_x[postdated_plot_x$xg_boost<=42 & postdated_plot_x$ga>42,'level']<-'Post-dated, GA'
postdated_plot_x[postdated_plot_x$xg_boost>42 & postdated_plot_x$ga<=42,'level']<-'Post-dated, XG_Boost'
postdated_plot_x[postdated_plot_x$xg_boost>42 & postdated_plot_x$ga>42,'level']<-'Post-dated'

plot_postdated_x<-ggplot(postdated_plot_x) +
  geom_point(aes(x=xg_boost, y= ga, color=level),size=1, alpha=0.5) +
  geom_hline(yintercept=42, linetype="dashed", size=0.5) +
  geom_vline(xintercept=42, linetype="dashed", size=0.5) +
  scale_colour_manual(values=c("Normal" = "darkgreen", "Post-dated" = "red", "Post-dated, GA" = "blue", "Post-dated, XG_Boost" = "orange"))+
  labs(x="GA by XG_boost at birth (weeks)", y="True GA (weeks)", 
       color="Post-dated by formula", caption=paste("n =",nrow(post_test)))+
  ggtitle('True GA vs XG Boost')+
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_postdated_x

ggsave(file="./figures/SVG/Figure_S4_1.svg", plot = plot_postdated, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S4_2.svg", plot = plot_postdated_i, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S4_3.svg", plot = plot_postdated_h, width=8, height=8)
ggsave(file="./figures/SVG/Figure_S4_4.svg", plot = plot_postdated_x, width=8, height=8)

ggsave(file="../figures/EPS/Figure_S4_1.eps", plot=plot_postdated, width=8, height=8)
ggsave(file="../figures/EPS/Figure_S4_2.eps", plot=plot_postdated_i, width=8, height=8)
ggsave(file="../figures/EPS/Figure_S4_3.eps", plot=plot_postdated_h, width=8, height=8)
ggsave(file="../figures/SVG/Figure_S4_4.eps", plot = plot_postdated_x, width=8, height=8)

ggsave(file="../figures/PDF/Figure_S4_1.pdf", plot=plot_postdated, width=8, height=8)
ggsave(file="../figures/PDF/Figure_S4_2.pdf", plot=plot_preterm_i, width=8, height=8)
ggsave(file="../figures/PDF/Figure_S4_3.pdf", plot=plot_preterm_h, width=8, height=8)
ggsave(file="../figures/SVG/Figure_S4_4.pdf", plot = plot_preterm_x, width=8, height=8)






