library(XLConnect)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(RWeka)
library(fpc)

wb = loadWorkbook("C:/Users/Gustav/Documents/Data-Mining-course/Project/ProjectData.xlsx")
Playerdata = readWorksheet(wb, sheet = "Modified - Metrics per 90", header = TRUE)

meanFrame <- data.frame(Playerdata %>%
                          group_by(League) %>%
                          summarise_each(funs(mean(., na.rm=TRUE)),-Player))
meanFrame[,2:51] <- data.frame(lapply(meanFrame[,-1], function(x) scale(x,
                        center = FALSE, scale = TRUE)))

meanFrameLong <- melt(meanFrame, id.vars="League")

meanFrameLong <- meanFrameLong %>% mutate(variable = factor(variable), 
                                          variable = factor(variable, levels = rev(levels(variable))))

ggplot(meanFrameLong[13:60, ], aes(y=value, x=variable ,color=League, group=League))  +
  theme_bw()+geom_line(size=1.25)+scale_color_manual(values=c("darkorange","indianred","seagreen","royalblue"),name="")+
  coord_flip() + theme(legend.position = "bottom" )

ggplot(meanFrameLong[73:84, ], aes(y=value, x=variable ,color=League, group=League))  +
  theme_bw()+geom_line(size=1.25)+scale_color_manual(values=c("darkorange","indianred","seagreen","royalblue"),name="")+
  coord_flip() + theme(legend.position = "bottom")   

ggplot(meanFrameLong[105:140, ], aes(y=value, x=variable ,color=League, group=League))  +
  theme_bw()+geom_line(size=1.25)+scale_color_manual(values=c("darkorange","indianred","seagreen","royalblue"),name="")+
  coord_flip() + theme(legend.position = "bottom")   

ggplot(meanFrameLong[141:188, ], aes(y=value, x=variable ,color=League, group=League))  +
  theme_bw()+geom_line(size=1.25)+scale_color_manual(values=c("darkorange","indianred","seagreen","royalblue"),name="")+
  coord_flip() + theme(legend.position = "bottom")   

corClusterData <- cor(ClusterData[,1:47], use = "pair")

PlayerMeltDataPL <- melt(Playerdata[1:47,-c(2:4, 52)], id="Player")
ggplot(PlayerMeltDataPL[1:705,], aes(variable, Player)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw()

ggplot(PlayerMeltDataPL[706:940,], aes(variable, Player)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw()


# clustering 
ClusterData <- Playerdata[,-c(1:4, 52)]
ClusterData <- data.frame(lapply(ClusterData, function(x) scale(x,
                           center = TRUE, scale = TRUE)))
XMeans(ClusterData)

# Simple K-means
Kmeans2 <- SimpleKMeans(ClusterData, Weka_control(N=2, S=311015))
ClusterData$KM2 <- Kmeans2$class_ids
Kmeans3 <- SimpleKMeans(ClusterData, Weka_control(N=3, S=311015))
ClusterData$KM3 <- Kmeans3$class_ids
Kmeans4 <- SimpleKMeans(ClusterData, Weka_control(N=4, S=311015))
ClusterData$KM4 <- Kmeans4$class_ids
Kmeans5 <- SimpleKMeans(ClusterData, Weka_control(N=5, S=311015))
ClusterData$KM5 <- Kmeans5$class_ids



# Want to see the characteristics for each cluster and compare them
# After that, select some typical players from each cluster to exemplify. 
KM4.m <- ClusterData[,-c(48,49,51)]
KM4.m <- data.frame(KM4.m %>%
                      group_by(KM4) %>%
                      summarise_each(funs(mean(., na.rm=TRUE)),-KM4))
KM4.m <- melt(KM4.m, id.vars="KM4")
KM4.m$KM4 <- as.factor(KM4.m$KM4)
KM4.m <- KM4.m %>% mutate(variable = factor(variable), 
                          variable = factor(variable, levels = rev(levels(variable))))

# Should compare these ones to when the columns are scaled. Probably the scaling should be done before the clustering?
# Should read about the k-means algorithm before I do the scaling then?

ggplot(KM4.m[1:60,], aes(variable, KM4)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw() +coord_flip()
ggplot(KM4.m[c(1:12,17:20, 25:44,49:60),], aes(variable, KM4)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "red", high = "royalblue") + theme_bw() +coord_flip()

# For this one, column KM4 has to be a factor with levels
ggplot(KM4.m[c(1:12,17:20, 25:44,49:60),],aes(x = variable, y = value, fill = KM4, group=KM4)) +
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("indianred","royalblue","seagreen","darkorange")) +
  coord_flip()


# Hierarchical clustering
# distance matrix
PlayerDist <- dist(ClusterData[,1:47], method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

HclustSingle <- hclust(PlayerDist, method="single")
plot(HclustSingle)

HclustComplete <- hclust(PlayerDist, method="complete")
plot(HclustComplete)

Hclustaverage <- hclust(PlayerDist, method="average")
plot(Hclustaverage)

HclustWard <- hclust(PlayerDist, method="ward.D")
plot(HclustWard)

ClusterData$hComp <- cutree(HclustWard, 3)

hComp.m <- ClusterData[,-c(48,49,50,51)]
hComp.m <- data.frame(hComp.m %>%
                        group_by(hComp) %>%
                        summarise_each(funs(mean(., na.rm=TRUE)),-hComp))
hComp.m <- melt(hComp.m, id.vars="hComp")

# Note, index depends on number of clusters (3 and 4 below)
1:9,13:15,19:33,37:45
1:12,17:20, 25:44,49:60
ggplot(hComp.m[c(1:9,13:15,19:33,37:45),], aes(variable, hComp)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw() +coord_flip()

# DBScan
db_test <- dbscan(ClusterData[,-c(48,49,50,51,52)], eps = 3, MinPts = 4)
table(db_test$cluster)
table(Kmeans5$class_ids)
table(ClusterData$hComp)
