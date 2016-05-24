
library(XLConnect)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(RWeka)

# Old Data
{
wb = loadWorkbook("C:/Users/Gustav/Documents/Data-Mining-course/Project/ProjectData.xlsx")
Playerdata = readWorksheet(wb, sheet = "Modified - Metrics per 90", header = TRUE)

ClusterData <- Playerdata[,-c(1:4, 52)]
ClusterData <- data.frame(lapply(ClusterData, function(x) scale(x,  center = TRUE, scale = TRUE)))

test <- kmeans(ClusterData,centers = 2, iter.max = 100)
test$betweenss / test$totss

twssFrame <- data.frame(twss=0, varExp = 0)
for(i in 1:10){
  set.seed(311015)
  km <- kmeans(ClusterData,centers = i, iter.max = 100)
  twssFrame[i,1] <- km$tot.withinss
  twssFrame[i,2] <- km$betweenss / km$totss
}
plot(twssFrame[,1], type="o")

# Tests with another seed to compare results
twss2 <- 0
for(i in 1:15){
  set.seed(12345)
  twss2[i] <- kmeans(ClusterData,centers = i, iter.max = 100)$tot.withinss
}
plot(twss2, type="o")

# Starts to take shorter steps after 3?
ggplot(twssFrame, aes(y=twss, x=1:nrow(twssFrame))) + geom_line(col="darkorange",size=1.05) + geom_point(col="blue", size=3) +
  theme_bw() + scale_x_continuous(breaks=c(2,4,6,8,10)) + xlab("Number of clusters") + 
  ylab("Total within-cluster sum of squares") + ggtitle("Scree plot - The Elbow method") + 
  geom_vline(xintercept = 3,linetype = 2)



}

# New Data
{
wb2 = loadWorkbook("C:/Users/Gustav/Documents/Data-Mining-course/Project/ProjectDataNew.xlsx")
Playerdata2 = readWorksheet(wb2, sheet = "Blad1", header = TRUE)

ClusterData2 <- Playerdata2[,-c(1:4)]
ClusterData2 <- data.frame(lapply(ClusterData2, function(x) scale(x,  center = TRUE, scale = TRUE)))

twssFrame <- data.frame(twss=0, varExp = 0)
for(i in 1:10){
  set.seed(910814)
  km <- kmeans(ClusterData2,centers = i, iter.max = 100)
  twssFrame[i,1] <- km$tot.withinss
  twssFrame[i,2] <- km$betweenss / km$totss
}
plot(twssFrame[,1], type="o")

# Tests with another seed to compare results
twss2 <- 0
for(i in 1:15){
  set.seed(12345)
  twss2[i] <- kmeans(ClusterData2,centers = i, iter.max = 100)$tot.withinss
}
plot(twss2, type="o")

# Starts to take shorter steps after 3?
ggplot(twssFrame, aes(y=twss, x=1:nrow(twssFrame))) + geom_line(col="darkorange",size=1.05) + geom_point(col="blue", size=3) +
  theme_bw() + scale_x_continuous(breaks=c(2,4,6,8,10)) + xlab("Number of clusters") + 
  ylab("Total within-cluster sum of squares") + ggtitle("Scree plot - The Elbow method") + 
  geom_vline(xintercept = 3,linetype = 2)
}

# Cluster data with K = 3
set.seed(311015)
km3 <- kmeans(ClusterData2,centers = 3, iter.max = 100)

ClusterData2$KM3 <- km3$cluster
KM3.m <- data.frame(ClusterData2 %>%
                      group_by(KM3) %>%
                      summarise_each(funs(mean(., na.rm=TRUE)),-KM3))
KM3.m <- melt(KM3.m, id.vars="KM3")
KM3.m$KM3 <- as.factor(KM3.m$KM3)
KM3.m <- KM3.m %>% mutate(variable = factor(variable), 
                          variable = factor(variable, levels = rev(levels(variable))))

ggplot(KM3.m[1:42,], aes(variable, KM3)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw() +coord_flip()

ggplot(KM3.m[1:42,], aes(variable, KM3)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "red", high = "royalblue") + theme_bw() +coord_flip()

# Should visualizations be done with original data instead of the transformed data?
Playerdata2 <- Playerdata2[,-c(1:4)]
Playerdata2$KM3 <- km3$cluster
KM3.m <- data.frame(Playerdata2 %>%
                      group_by(KM3) %>%
                      summarise_each(funs(mean(., na.rm=TRUE)),-KM3))
KM3.m <- melt(KM3.m, id.vars="KM3")
KM3.m$KM3 <- as.factor(KM3.m$KM3)
require(dplyr)
KM3.m <- KM3.m %>% mutate(variable = factor(variable), 
                          variable = factor(variable, levels = rev(levels(variable))))

ggplot(KM3.m[1:42,], aes(variable, KM3)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "green") + theme_bw() +coord_flip()

ggplot(KM3.m[1:42,],aes(x = variable, y = value, fill = KM3, group=KM3)) +
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("indianred","royalblue","darkorange","darkorange")) +
  coord_flip()

# For the tile plot transformed values are better and for the bar plots is non-transformed values better

