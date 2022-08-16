library(readxl)
library(tidyverse)
#1. load data and preprocess (SHAKES)
wo_last <- read_excel("C:/Users/DELL/Desktop/munka/dramakorpusz/cluster/wo_last_act_tidy.xlsx")
wo_last1 <- mutate_all(wo_last[,3:10], function(x) as.numeric(as.character(x)))
wo_last1 <- tibble(wo_last[,1:2], wo_last1)

#2. calculating differences between whole drama and wo the last act
wo_last2 <- wo_last1 %>% 
  mutate(dens_diff = `Density all` - `Density wo last` ) %>% 
  mutate(avpath_diff = `Average path length all` - `Average path length wo last` ) %>%
  mutate(avclust_diff = `Average clustering coefficient all` - `Average clustering coefficient wo last`) 

#save Com and Trag separately
wo_last_com <- wo_last2 %>% 
  filter(Genre == "Comedy")

wo_last_trag <- wo_last2 %>% 
  filter(Genre == "Tragedy")

#3. calculate average differences (if positive: all is bigger, if negative: wo_last is bigger)
av_densdif_com <- mean(wo_last_com$dens_diff)
av_densdif_trag <- mean(wo_last_trag$dens_diff)

av_pathdif_com <- mean(wo_last_com$avpath_diff)
av_pathdif_trag <- mean(wo_last_trag$avpath_diff)

av_clustdif_com <- mean(wo_last_com$avclust_diff)
av_clustif_trag <- mean(wo_last_trag$avclust_diff)

#4. are these diff. significant?
# differences in density
wilcox.test(wo_last_com$dens_diff, wo_last_trag$dens_diff)
# p-value = 0.0001825 --> significant

# differences in av.path lenght
wilcox.test(wo_last_com$avpath_diff, wo_last_trag$avpath_diff)
# p-value = 0.0002836 --> significant

# differences without the last act
wilcox.test(wo_last_com$`Density wo last`, wo_last_trag$`Density wo last`)
# p-value = 0.07417 --> not-significant

#differences whole drama
wilcox.test(wo_last_com$`Density all`, wo_last_trag$`Density all`)
# p-value = 0.0002358 --> significant


#Plot differences

p <- ggplot(wo_last2, aes(dens_diff, Genre)) +
  geom_boxplot(position = "identity")+ 
  coord_flip()+ theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10))+
  xlab("Difference in density")

p1 <- ggplot(wo_last2, aes(dens_diff, Genre)) +
  geom_boxplot(position = "identity")+ 
  coord_flip()+ theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10))+
  xlab("Difference in diameter")

ggarrange(p, p1)
