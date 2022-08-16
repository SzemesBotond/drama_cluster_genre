setwd("YOUR DIRECTORY")
library(readxl)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(ClusterR)
library(cluster)
library(dendextend)
library(colorspace)
library(stylo)


#1.
###load corpus, clean columns ###
drama_all <- read.csv("/gerdracor-metadata.csv")
drama_all <- drama_all[order(drama_all$id),]
drama_all <- select(drama_all, -c(firstAuthor, 
                                  numOfCoAuthors,
                                  subtitle,
                                  #normalizedGenre, #just in shake -in others needed
                                  size,
                                  libretto, 
                                  maxDegreeIds,
                                  yearPremiered,
                                  yearPrinted,
                                  numOfSpeakersFemale,
                                  numOfSpeakersMale,
                                  numOfSpeakersUnknown,
                                  numPersonGroups,
                                  yearWritten,
                                  wikipediaLinkCount,
                                  numOfActs,
                                  wordCountText,
                                  wordCountSp,
                                  wordCountStage,
                                  numOfP,
                                  numOfL
))

#for gerdracor where there are labels in normalizedGenre
drama_all <- drama_all %>%
  filter(normalizedGenre == "Comedy"|
           normalizedGenre == "Tragedy") %>%
  filter(numOfSpeakers > 5) %>% # could be more than five
  filter(numOfSegments > 2)





#2.
### calculating scores from individual dramas ###

input.dir <- "C:/Users/DELL/Desktop/munka/dramakorpusz/cluster/ger_data"
files.v <- dir(input.dir, "\\.csv$")
title <- gsub("[a-z]+[0-9]+-", "", files.v)
title <- gsub("-cast.csv", "", title)
setwd("C:/Users/DELL/Desktop/munka/dramakorpusz/cluster/ger_data")

'''
#if the treshold more than 5 chars in play (eg.10), than need to filter out the dramas here too
# less drama is needed when we load the induvidual texts

# find the titles with more than 10 chars
drama_10 <- list()
for(i in 1:nrow(drama_all)){
  drama_10[i] <- paste(drama_all[i,]$id,"-", drama_all[i,]$name,"-cast.csv", sep="")
}
drama_10 <- unlist(drama_10)

#check result (not needed)
# files_10 <- intersect(drama_10, files.v)
'''


#2.1
#add speech distribution: high, medium and low speech (k-means)
dramas <- list()
kmeans.char <- list()
distrib <- list()
distrib1 <- list()
for (i in 1:nrow(drama_all)) {
  #if more than 5 char, than use "drama_x" instead of "files.v"
  dramas[[i]] <- read.table(files.v[[i]], header = T, sep = ",")
  dramas[[i]] <- dramas[[i]] %>%
    filter(isGroup == "false")
  kmeans.char [[i]] <- kmeans(dramas[[i]]$numOfWords, centers = 3)
  distrib [[i]] <- tibble(kmeans.char[[i]]$size, kmeans.char[[i]]$centers)
  distrib [[i]]$rel <- kmeans.char[[i]]$size/sum(kmeans.char[[i]]$size)
  distrib [[i]] <- distrib [[i]][order(distrib [[i]][,2]),]
  distrib [[i]]$quant <- c("LowSpeech", "MediumSpeech", "HighSpeech")
  distrib [[i]]$drama <- title[[i]]
  distrib1 [[i]] <- distrib[[i]][,3:5] %>% spread(quant, rel)
}

Speech_Dist <- bind_rows(distrib1)
drama_all <- cbind(drama_all, Speech_Dist[2:4])

#2.2.
#add high, medium, low Weigthed Degree (k-means)
#just nongroup characters
kmeans.char.deg <- list()
distrib2 <- list()
distrib3 <- list()
p <- list()
q <- list()
for (i in 1:nrow(drama_all)) {
  p[[i]] <- dramas[[i]]$weightedDegree
  q[[i]] <- length(unique(p[[i]]))
  if (q[[i]] > 2){
    #degree / wieightedDegree
    kmeans.char.deg [[i]] <- kmeans(dramas[[i]]$weightedDegree, centers = 3)
    distrib2 [[i]] <- tibble(kmeans.char.deg[[i]]$size, kmeans.char.deg[[i]]$centers)
    distrib2 [[i]]$rel <- kmeans.char.deg[[i]]$size/sum(kmeans.char.deg[[i]]$size)
    distrib2 [[i]] <- distrib2 [[i]][order(distrib2 [[i]][,2]),]
    distrib2 [[i]]$quant <- c("LowWeigthedDegree", "MediumWeigthedDegree", "HighWeigthedDegree")
    distrib2 [[i]]$drama <- title[[i]]
    distrib3 [[i]] <- distrib2[[i]][,3:5] %>% spread(quant, rel)}
  else {
    distrib3 [[i]] <- tibble(drama = title[[i]],  HighWeigthedDegree=1, LowWeigthedDegree =0, MediumWeigthedDegree=0)   
  }
}

WeigthedDeg <- bind_rows(distrib3)
drama_all <- cbind(drama_all, WeigthedDeg[2:4])

#2.3.
#average character speech (num words)
#just more than 10 speech act chars (?)

AverageCharSpeech <- list()
for (i in 1:length(files.v)) { #length(files.v) or length(drama_10)
  dramas[[i]] <- dramas[[i]] %>%
    filter(numOfSpeechActs > 10)
  AverageCharSpeech [[i]] <- sum(dramas[[i]]$numOfWords)/sum(dramas[[i]]$numOfSpeechActs)
}

AverageCharSpeech <- unlist(AverageCharSpeech)
drama_all <- cbind(drama_all, AverageCharSpeech)

#2.4.
#average number of char in scene / num of scene
AvCharinScene <- list()
for (i in 1:length(files.v)) {
  dramas[[i]] <- read.table(files.v[[i]], header = T, sep = ",")
  dramas[[i]] <- dramas[[i]] %>%
    filter(isGroup == "false")
  AvCharinScene [[i]] <- sum(dramas[[i]]$numOfScenes)/nrow(dramas[[i]])
}

AvCharinScene <- unlist(AvCharinScene) /drama_all$numOfSegments
drama_all <- cbind(drama_all, AvCharinScene)

#2.5.
MaxBetweenness <- list()
for (i in 1:length(files.v)) { #length(files.v) or length(drama_10)
  dramas[[i]] <- read.table(files.v[[i]], header = T, sep = ",")
  dramas[[i]] <- dramas[[i]] %>%
    filter(isGroup == "false")
  MaxBetweenness [[i]] <- max(dramas[[i]]$betweenness)
}
MaxBetweenness <- unlist(MaxBetweenness)
drama_all <- cbind(drama_all, MaxBetweenness)

#2.6.
drama_all$AvDegreeInMaxDegree <- drama_all$averageDegree/drama_all$maxDegree

#2.7.
drama_all$MaxDegreeInAllChar <- drama_all$maxDegree/(drama_all$numOfSpeakers -1)



#Delete some variables
drama_all1 <- select(drama_all, -c(numOfSegments,
                                   maxDegree,
                                   averageDegree,
                                   numConnectedComponents,
                                   numOfSpeakers,
                                   averagePathLength,
                                   MaxDegreeInAllChar)
) 


#3.
###Calculating the z-scores 
# with numConnectedcomp, avdegreeInMaxDegree, diameter and averagePath: 21 variable
#if there is Genre in the original dataset, than 6:21
#in Shakespeare: 5:20
drama_all1[,6:18] <- mutate_all(drama_all1[,6:18], function(x) as.numeric(as.character(x)))
drama_zscores <- as.data.frame(sapply(drama_all1[,6:18], function(data) (data-mean(data))/sd(data)))
drama_zscores <- tibble(drama_all[1:5], drama_zscores)

#3.1. Boxplots of different feauteres in different Genre
library(ggplot2)
library(ggpubr)
colnames(drama_zscores[6:18])
p <- ggplot(drama_zscores, aes(averagePathLength, normalizedGenre)) +
  geom_boxplot(position = "identity")+ 
  coord_flip()+ theme(axis.text=element_text(size=10),
                      axis.title=element_text(size=10))

#save all graphs in a variable "px", than plot them together
ggarrange(p, p2, p3, p4, p5,p6, p7, p8, p9, p10, p11, p12, p13,  
          ncol = 3, nrow = 5)

# 3.2. plot two features and the correlation between them


# 3.3. Check statistical significance
drama_all_com <- drama_all1 %>% 
  filter(normalizedGenre == "Comedy")

drama_all_trag <- drama_all1 %>% 
  filter(normalizedGenre == "Tragedy")


wilcox.test(drama_all_com$MaxDegreeInAllChar, drama_all_trag$MaxDegreeInAllChar)


# 3.4. Test features
#Correlation Matrix
library(DataExplorer)

drama_feature <- drama_zscores[,c(4,6:18)]
plot_correlation(drama_feature, type = "all")

#significance test
library(psych)
drama_feature1 <- drop_columns(drama_feature, 1) 
cormatrix <- corr.test(drama_feature1, method= "pearson")
print(cormatrix, short= F)
cormatrix$ci


'''
#4.
#add Genre if necessary - Shakespeare
Genre <- c(rep("Comedy", 14), rep("History", 10),rep("Tragedy", 13))
Comedy_Other <- c(rep("Comedy", 14), rep("Other",23))
drama_zscores <- cbind(drama_zscores, Genre,Comedy_Other)
'''
##6. SUPERVISED CLASS
#svm

library(stylo)
library(e1071) # for SVM


comedies <- which(drama_zscores$normalizedGenre == "Comedy" )
tragedies <- which(drama_zscores$normalizedGenre != "Comedy" )
results_df <- NULL
cols <- c(6:18) # 
## leave-one-out cross validation
## each com/trag participates as a test set
for (i in 1:length(tragedies)) { 
  
  ## data for train vs. test (one vector)
  #train <- drama_zscores[c(comedies[-i],tragedies),c(4,cols)] #22 is the Genre in Shake; other Corpus: 4
  #test <- drama_zscores[comedies[i],c(4,cols)]
  #or test the other
  train <- drama_zscores[c(tragedies[-i],comedies),c(4,cols)] 
  test <- drama_zscores[tragedies[i],c(4,cols)]
  
  ## fit SVM with linear kernel 
  drama_svm <- svm(as.factor(normalizedGenre) ~., kernel="linear",data = train)#or Comedy_other for Shakespera
  
  ## predict the `test` sample (remove the class column in the end)
  pr_svm <- predict(drama_svm,test) %>% table()
  
  ## write results 
  df_pred_svm <- tibble(pred=pr_svm["Tragedy"],Genre = "Tragedy") #or Other for Shake, Tragedy for notShake
  ## combine in overall table
  results_df <- bind_rows(results_df, df_pred_svm)
  
} 


results1 <- results_df %>%
  group_by(Genre) %>%
  summarize(mean_acc=mean(pred))

#print the results
write.csv(results1,"C:/Users/DELL/Desktop/munka/dramakorpusz/cluster/accuracy_results_18COL_withDIAMETER_gerdracor_all.csv" )

# calculate precision and recall

tragres <- results_df %>% 
  filter(Genre == "Tragedy")
comres <- results_df %>% 
  filter(Genre == "Comedy")

truepos <- sum(comres$pred) #or tragres
falsepos <-  length(tragedies) - sum(tragres$pred) #or comres
falseneg <- length(comedies) - truepos #or tragedies
recall =  truepos/(truepos + falseneg)
precision = truepos/(truepos + falsepos)

f1 = 2 * precision * recall / (precision + recall) 
f1


#6.
##PCA
library(ggfortify)
library(ggplot2)

pca <- prcomp(drama_zscores[,cols], scale=T)
rownames(drama_zscores) <- drama_zscores$name

autoplot(pca, data = drama_zscores, 
         colour = "normalizedGenre", label.size = 4.5, 
         shape = "normalizedGenre",
         loadings = T,  loadings.label = T, loadings.label.size = 2) +
  #ggtitle("")+
  theme(plot.title = element_text(size=12)) +
  theme(plot.subtitle = element_text(hjust=1.0))+
  #guides(
  #  colour = guide_legend(title = "Normalized Genre")  ) +
  scale_color_manual(values = c("#C1C1C1","#3D3D3D"))+
  theme_bw()


