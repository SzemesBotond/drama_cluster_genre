library(dplyr)
setwd("YOUR DIR")
#load data and preprocess 
#(SHAKES)
cum_act <- read.csv(file = 'shakedracor_cumulative1.csv')
# or (GER) gerdracor_cumulative2_no_pro_epi.csv
cum_com <- cum_act %>% 
  filter(genre == "Comedy")
cum_trag <- cum_act %>% 
  filter(genre != "Comedy")

#Visualize the means/medians
all <- c(median(cum_act$acts_1_density),
         median(cum_act$acts_1.2_density),
         median(cum_act$acts_1.2.3_density),
         median(cum_act$acts_1.2.3.4_density),
         median(cum_act$acts_1.2.3.4.5_density))
trag <- c(median(cum_trag$acts_1_density),
          median(cum_trag$acts_1.2_density),
          median(cum_trag$acts_1.2.3_density),
          median(cum_trag$acts_1.2.3.4_density),
          median(cum_trag$acts_1.2.3.4.5_density))
com <- c(median(cum_com$acts_1_density),
         median(cum_com$acts_1.2_density),
         median(cum_com$acts_1.2.3_density),
         median(cum_com$acts_1.2.3.4_density),
         median(cum_com$acts_1.2.3.4.5_density))

#or cumclust
median_cumdens <- tibble(
          c(rep("All",5),
            rep("Tragédia", 5),
            rep("Komédia", 5)),
  
          c(all, trag, com),
  
          rep(c(1,
                2,
                3,
                4,
                5), 3))
colnames(median_cumdens) <- c("Genre", "Density", "Act") #Or Arverage Clustering

median_cumdens <- median_cumdens %>% 
  filter(Genre != "All")

library(ggplot2)
p1 <- ggplot(median_cumdens,aes(x = Act, y = Density, color = Genre)) + 
  geom_line() +
  geom_point()+
  scale_color_manual(values=c("orange", "black")) +
  xlab('Acts') +
  ylab('Density (Median)')+
  theme_bw()+
  ggtitle ("ShakeDracor", subtitle = "")


## Interactive visualization of all the plays &
all <- c(cum_act$acts_1_density,
         cum_act$acts_1.2_density,
         cum_act$acts_1.2.3_density,
         cum_act$acts_1.2.3.4_density,
         cum_act$acts_1.2.3.4.5_density)
#or cumclust
all_cumcdens <- tibble(
  rep(cum_act$title_pretty, 5),
  
  c(cum_act$acts_1_density,
    cum_act$acts_1.2_density,
    cum_act$acts_1.2.3_density,
    cum_act$acts_1.2.3.4_density,
    cum_act$acts_1.2.3.4.5_density),
  
  rep(c(rep(1,nrow(cum_act)),
        rep(2,nrow(cum_act)),
        rep(3,nrow(cum_act)),
        rep(4,nrow(cum_act)),
        rep(5,nrow(cum_act)))
  ))

colnames(all_cumdens) <- c("Title", "Density", "Act") #Or Arverage Clustering

colnames(median_cumdens) <- c("Title", "Density", "Act") #Or Arverage Clustering
median_cumdens$Title <- c(rep("Tragedy Median", 5), rep("Comedy Median", 5))

pos <- nrow(all_cumdens)
for(i in 1:nrow(median_cumdens)){
  all_cumdens[pos+i,] <- median_cumdens[i,]
}


library(plotly)
plot_ly(all_cumdens, x = ~Act, 
        y = ~Density, 
        # groups and assigns different colors in one step
        color = ~Title,
        # name = 'all_terms', 
        type = 'scatter', 
        mode = 'lines+markers') %>%
  layout(showlegend = TRUE, legend = list(font = list(size = 7)))

## for clustering coeff. calculating sd, and var.test

first_cluster <- all %>% 
  filter(Act == 1)
second_cluster <- all_cumclust %>% 
  filter(Act == 2)
third_cluster <- all_cumclust %>% 
  filter(Act == 3)
forth_cluster <- all_cumclust %>% 
  filter(Act == 4)
last_cluster <- all_cumclust %>% 
  filter(Act == 5)

sd_cluster <- c(sd(first_cluster$average_clustering),
                sd(second_cluster$average_clustering),
                sd(third_cluster$average_clustering),
                sd(forth_cluster$average_clustering),
                sd(last_cluster$average_clustering)
)

wilcox.test(first_cluster$average_clustering, 
         last_cluster$average_clustering)
plot(sd_cluster, type = "b", main = "", xlab= "", ylab = "")
title(main = "Change in SD of Average Clustering Coeff.",
      sub = "ShakeDracor", #or GER
      xlab = "Acts",
      ylab = "Standard Deviation")


