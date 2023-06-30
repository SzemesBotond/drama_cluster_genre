library(dplyr)
# k-means vs. random samples - 
# based on the values from drama_cluster_genre.R ("drama_zscores")
#1. k-mean
cols <- c(6:18)
k2 <- kmeans(drama_zscores[,cols], centers = 10)
drama_zscores$cluster <- k2$cluster
drama_zscores <- drama_zscores %>%  group_by(cluster)

#just to have a look on the separate clusters
drama_kmeans_group <- group_split(drama_zscores)

# calculate the sd of years in clusters
clust_year_sd <- drama_zscores%>% 
  summarise_at(vars(yearNormalized), list(year_sd=sd)) 

mean_year_clust <- mean(clust_year_sd$year_sd)

# calculate the absdiff bewtween num of each genres

clust_genre <- drama_zscores%>% 
  group_by(normalizedGenre, cluster) %>% 
  count(vars(normalizedGenre)) %>%
  ungroup() %>% 
  group_by(cluster) %>% 
  group_split()

#adding 0 Com and Trag, which is needed in case of the absence of a genre in a cluster
add_zero <- tibble(normalizedGenre = c("Tragedy", "Comedy"),
                   n = c(0,0))

clust_genre1 <- lapply(clust_genre, function(x)
  x %>% 
    bind_rows(add_zero) %>% 
    group_by(normalizedGenre) %>% 
    summarise(n = sum(n)) %>% 
    mutate(abs_diff = abs(diff(n))) %>% 
    filter(row_number()==1))

clust_genre_diff <- bind_rows(clust_genre1)$abs_diff
diff_genre_clust <- mean(clust_genre_diff)

#2. create random subsets

sizes <- k2$size
# split the num of rows into k groups with the same size as in k2$sizes
random_samples1 <- lapply(seq_along(1:1000), function(i)
  split(1:nrow(drama_zscores), sample(rep(1:length(sizes), sizes)))
)

#subset original dataframe on the basis of the groups created
random_samples <- lapply(seq_along(1:1000), function(i)
  lapply(seq_along(1:length(sizes)), function(j)
    drama_zscores[unlist(random_samples1[[i]][[j]]),]
  )
)
  
# calculating sd of years
random_year_sd <- lapply(random_samples, function(i)
  lapply(i, function(x)
    sd(x$yearNormalized)
  ))

random_year_all <- as.numeric(unlist(random_year_sd )) #otherwised named num
mean_year_random <- mean(unlist(random_year_sd ))

#calculating genre diff
add_zero <- tibble(normalizedGenre = c("Tragedy", "Comedy"),
                   n = c(0,0))

random_genre <- lapply(random_samples, function (i) 
  lapply(i, function(x)
    x %>% 
      ungroup() %>% 
      count(normalizedGenre) %>%
      bind_rows(add_zero) %>% 
      group_by(normalizedGenre) %>% 
      summarise(n = sum(n)) %>% 
      mutate(abs_diff = abs(diff(n)))
    
  ))

random_genre1 <- lapply(random_genre, function (i)
  lapply(i, function(x)
    as.numeric(x[1,3])))

random_genre_all <- as.numeric(unlist(random_genre1))

diff_genre_random <- mean(unlist(random_genre1))

# results

mean_year_clust
mean_year_random
diff_genre_clust
diff_genre_random


# save the results separately of the different k sizes
clust4 <- data.frame(year_sd = c(mean_year_clust, mean_year_random),
                 genre_diff = c(diff_genre_clust, diff_genre_random))
rownames(clust4) <- c("cluster4", "random4")

clust6 <- data.frame(year_sd = c(mean_year_clust, mean_year_random),
                     genre_diff = c(diff_genre_clust, diff_genre_random))
rownames(clust6) <- c("cluster6", "random6")

clust8 <- data.frame(year_sd = c(mean_year_clust, mean_year_random),
                     genre_diff = c(diff_genre_clust, diff_genre_random))
rownames(clust8) <- c("cluster8", "random8")

clust10 <- data.frame(year_sd = c(mean_year_clust, mean_year_random),
                     genre_diff = c(diff_genre_clust, diff_genre_random))
rownames(clust10) <- c("cluster10", "random10")

results <- bind_rows(clust4,
                    clust6,
                    clust8,
                    clust10)
results

results$clustnum <- c(4,4,6,6,8,8,10,10)
results$type <- rep(c("k-means", "random"), 4)

p <- ggplot(results, aes(x =clustnum, year_sd, colour = type))+
  geom_point()+
  geom_line()+
  ylim(30,70)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  labs( x ="",
        y ="SD of Year\n(Mean)",
        colour = "Method")
  
p1 <- ggplot(results, aes(clustnum, genre_diff, colour = type))+
    geom_point()+
    geom_line()+
    ylim(0,25)+
    scale_color_manual(values=c("#E69F00", "#56B4E9"))+
    theme_bw()+
    labs( x ="Number of clusters",
          y ="Abs Diff of Num of Genres\n(Mean)",
          colour = "Method")
  
library(ggpubr)
ggarrange(p, p1, nrow = 2, common.legend = T, legend = "right")


# test differences / similarity

#year
a <- clust_year_sd$year_sd
b <- lapply(seq_along(1:1000), function(i)
  unlist(random_year_sd[[i]]))
diff_y <- lapply(seq_along(1:1000), function(i)
  a-b[[i]])
diff_y1 <- tibble(difference = unlist(diff_y))

#gen
a <- clust_genre_diff
b <- lapply(seq_along(1:1000), function(i)
  unlist(random_genre1[[i]]))
#cosine(a, b)
diff_gen <- lapply(seq_along(1:1000), function(i)
  a-b[[i]])

diff_gen1 <- tibble(difference = unlist(diff_gen))

# all
dat <- tibble(dens = c(unlist(diff_gen), unlist(diff_y1)),
              cluster_value = c(rep("genre-diff", length(sizes)*1000), 
                                rep("sd-year", length(sizes)*1000 ))
)

ggplot(dat, aes(x = dens, fill = cluster_value)) + 
  geom_density(alpha = 0.5, bw= 3)+
  geom_vline(xintercept = 0)+
  xlab("")+
  ylab("Density")+
  ggtitle("k = 10")+
  theme_bw()


