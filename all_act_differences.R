library(readxl)
library(tidyverse)
setwd("YOUR PATH")
#1_ load data and preprocess 
#(SHAKES)
wo_act <- read.csv(file = 'shakedracore_metrics7.csv')
wo_act1 <- mutate_all(wo_act[,8:25], function(x) as.numeric(as.character(x))) 
wo_act1 <- tibble(wo_act[,1:7], wo_act1)

#GerDracor
wo_act <- read.csv(file = 'gercore_metrics_for_5acts.csv')
wo_act1 <- mutate_all(wo_act[,3:20], function(x) as.numeric(as.character(x))) 
wo_act1 <- tibble(wo_act[,1:2], wo_act1)

#2_ calculating differences between whole drama and wo the last act
#(could be ina loop)
wo_act_diff <- wo_act1 %>% 
  mutate(dens_diff1 = whole_density - wo1_density ) %>% 
  mutate(dens_diff2 = whole_density - wo2_density ) %>% 
  mutate(dens_diff3 = whole_density - wo3_density ) %>% 
  mutate(dens_diff4 = whole_density - wo4_density ) %>% 
  mutate(dens_diff5 = whole_density - wo5_density ) %>% 
  
  mutate(avclust_diff1 = whole_average_clustering - wo1_average_clustering ) %>% 
  mutate(avclust_diff2 = whole_average_clustering - wo2_average_clustering ) %>% 
  mutate(avclust_diff3 = whole_average_clustering - wo3_average_clustering ) %>% 
  mutate(avclust_diff4 = whole_average_clustering - wo4_average_clustering ) %>% 
  mutate(avclust_diff5 = whole_average_clustering - wo5_average_clustering) %>% 
  
  mutate(diam_diff1 = whole_diameter - wo1_diameter ) %>% 
  mutate(diam_diff2 = whole_diameter - wo2_diameter ) %>% 
  mutate(diam_diff3 = whole_diameter - wo3_diameter ) %>% 
  mutate(diam_diff4 = whole_diameter - wo4_diameter ) %>% 
  mutate(diam_diff5 = whole_diameter - wo5_diameter )  

#save Com and Trag separately
wo_act_com <- wo_act_diff %>% 
  filter(genre == "Comedy")

wo_act_trag <- wo_act_diff %>% 
  filter(genre == "Tragedy")

#3_ calculate average differences (if positive: all is bigger, if negative: wo_act is bigger)
#Density
#com
av_densdif1_com <- mean(wo_act_com$dens_diff1)
av_densdif2_com <- mean(wo_act_com$dens_diff2)
av_densdif3_com <- mean(wo_act_com$dens_diff3)
av_densdif4_com <- mean(wo_act_com$dens_diff4)
av_densdif5_com <- mean(wo_act_com$dens_diff5)

#trag
av_densdif1_trag <- mean(wo_act_trag$dens_diff1)
av_densdif2_trag <- mean(wo_act_trag$dens_diff2)
av_densdif3_trag <- mean(wo_act_trag$dens_diff3)
av_densdif4_trag <- mean(wo_act_trag$dens_diff4)
av_densdif5_trag <- mean(wo_act_trag$dens_diff5)

#Average Cluster Coeff
#com
av_clustdif1_com <- mean(wo_act_com$avclust_diff1)
av_clustdif2_com <- mean(wo_act_com$avclust_diff2)
av_clustdif3_com <- mean(wo_act_com$avclust_diff3)
av_clustdif4_com <- mean(wo_act_com$avclust_diff4)
av_clustdif5_com <- mean(wo_act_com$avclust_diff5)

#trag
av_clustdif1_trag <- mean(wo_act_trag$avclust_diff1)
av_clustdif2_trag <- mean(wo_act_trag$avclust_diff2)
av_clustdif3_trag <- mean(wo_act_trag$avclust_diff3)
av_clustdif4_trag <- mean(wo_act_trag$avclust_diff4)
av_clustdif5_trag <- mean(wo_act_trag$avclust_diff5)

#Diameter
#com
av_diam1_com <- mean(wo_act_com$diam_diff1)
av_diam2_com <- mean(wo_act_com$diam_diff2)
av_diam3_com <- mean(wo_act_com$diam_diff3)
av_diam4_com <- mean(wo_act_com$diam_diff4)
av_diam5_com <- mean(wo_act_com$diam_diff5)

#trag
av_diam1_trag <- mean(wo_act_trag$diam_diff1)
av_diam2_trag <- mean(wo_act_trag$diam_diff2)
av_diam3_trag <- mean(wo_act_trag$diam_diff3)
av_diam4_trag <- mean(wo_act_trag$diam_diff4)
av_diam5_trag <- mean(wo_act_trag$diam_diff5)

#4_ are these diff_ significant?
# differences in density
wilcox.test(wo_act_com$dens_diff5, wo_act_trag$dens_diff5)
#GerDracor p-value = 0.4986 ---> not significant
#Shake p-value = 0.001097 --> significant

# differences without the last act
wilcox.test(wo_act_com$wo5_density, wo_act_trag$wo5_density)
#GerDracor pvalue = 0.02858 --> significant
#Shake  p-value = 0.0003702

#differences whole drama
wilcox.test(wo_act_com$whole_density, wo_act_trag$whole_density)
#GerDracor p-value = 0.01626 ---> significant
#Shake  p-value = 0.00002745 --> significant


#without other acts

#1
wilcox.test(wo_act_com$wo1_density, wo_act_trag$wo1_density)
#GerDracor p-value = 0.02212
#Shaek p-value = 0.00001886

#2
wilcox.test(wo_act_com$wo2_density, wo_act_trag$wo2_density)
#GerDracor  p-value = 0.02544
#Shake p-value = 0.000003637

#3
wilcox.test(wo_act_com$wo3_density, wo_act_trag$wo3_density)
#GerDracor  p-value = 0.03057
#shake p-value = 2.745e-05

#4
wilcox.test(wo_act_com$wo4_density, wo_act_trag$wo4_density)
#GerDracor  p-value = 0.007576
#Shake p-value = 0.0001608




#Plot differences
p <- ggplot(wo_act_diff, aes(dens_diff5, genre)) +
  geom_boxplot(position = "identity")+ 
  coord_flip()+ #theme(axis_text=element_text(size=10),
  #     axis_title=element_text(size=10))+
  xlab("The Effect of the Last Act on Density")+
  ylab ("Genre")+
  ggtitle ("GerDracor")

p1 <- ggplot(wo_act_diff, aes(dens_diff5, genre)) +
  geom_boxplot(position = "identity")+ 
  coord_flip()+ #theme(axis_text=element_text(size=10),
                 #     axis_title=element_text(size=10))+
  xlab("The Effect of the Last Act on Density")+
  ylab ("Genre")+
  ggtitle ("ShaDracor")

ggarrange(p,p1)

#Changes over the Paly 
av_densdif_com <- c(av_densdif1_com, av_densdif2_com, av_densdif3_com,
                    av_densdif4_com, av_densdif5_com)

av_densdif_trag <- c(av_densdif1_trag, av_densdif2_trag, av_densdif3_trag,
                    av_densdif4_trag, av_densdif5_trag)

data <- tibble(av_densdif_com, c(1:5)) #tibble(av_densdif_trag, c(1:5))
colnames(data) <- c("Effect", "Act")

p <- ggplot(data = data, aes(Act, Effect))+
  geom_point()+
  geom_line() +
  ylab ("Effect of the Acts on Density")+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", size =1) +
  ggtitle("Comedies", subtitle = "GerDraCor") +
  theme_bw()

p1 <- ggplot(data = data, aes(Act, Effect))+
  geom_point()+
  geom_line() +
  ylab ("")+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", size =1) +
  ggtitle("", subtitle = "ShakeDraCor") +
  theme_bw()

ggarrange(p, p1)
