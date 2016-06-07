
library(XLConnect)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(RWeka)
library(grid)
library(gridExtra)

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
Playerdata = readWorksheet(wb2, sheet = "Blad1", header = TRUE)

ClusterData2 <- Playerdata[,-c(1:4)]
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
  geom_vline(xintercept = 4,linetype = 2)
}

# Cluster data with K = 3
set.seed(12345)
km5 <- kmeans(ClusterData2,centers = 5, iter.max = 100)
ClusterData2$KM5 <- km5$cluster
table(ClusterData2$KM5)

KM5.m <- data.frame(ClusterData2 %>%
                      group_by(KM5) %>%
                      summarise_each(funs(mean(., na.rm=TRUE)),-KM5))
KM5.m <- melt(KM5.m, id.vars="KM5")
KM5.m$KM5 <- as.factor(KM5.m$KM5)
KM5.m <- KM5.m %>% mutate(variable = factor(variable), 
                          variable = factor(variable, levels = rev(levels(variable))))

ggplot(KM5.m[1:70,], aes(variable, KM5)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "indianred", high = "springgreen") + theme_bw() +coord_flip()

ggplot(KM5.m[1:70,], aes(variable, KM5)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "darkorange", high = "royalblue") + theme_bw() +coord_flip()

# Should visualizations be done with original data instead of the transformed data?
Playerdata2 <- Playerdata[,-c(1:4)]
Playerdata2$KM5 <- km5$cluster
KM5.m <- data.frame(Playerdata2 %>%
                      group_by(KM5) %>%
                      summarise_each(funs(mean(., na.rm=TRUE)),-KM5))
KM5.m <- melt(KM5.m, id.vars="KM5")
KM5.m$KM5 <- as.factor(KM5.m$KM5)

KM5.m <- KM5.m %>% mutate(variable = factor(variable, levels = rev(levels(variable))))

ggplot(KM5.m[1:70,], aes(variable, KM5)) + geom_tile(aes(fill = value),  colour = "white") +
  scale_fill_gradient(low = "darkorange", high = "royalblue") + theme_bw() +coord_flip()

plots <- function(Data){
  ggplot(Data,aes(x = variable, y = value, fill = KM5, group=KM5)) + theme_bw() + 
    geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("indianred","royalblue","darkorange",
       "darkgrey","springgreen4"), name="Cluster") + coord_flip() + theme(legend.position="bottom") + xlab("")
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


Goals1 <- plots(KM5.m[71:100,])
Goals2 <- plots(KM5.m[101:125,])
legend<-g_legend(Goals2)
grid.arrange(arrangeGrob(Goals1 + theme(legend.position="none"), Goals2 + theme(legend.position="none"),ncol=2),
             legend, nrow=2,heights=c(10, 1),top=textGrob("Goal metrics",gp=gpar(fontsize=18,font=3)))


shots1 <- plots(KM5.m[1:35,])
shots2 <- plots(KM5.m[36:70,])
legend<-g_legend(shots2)

grid.arrange(arrangeGrob(shots1 + theme(legend.position="none"), shots2 + theme(legend.position="none"),ncol=2),
             legend, nrow=2,heights=c(10, 1),top=textGrob("Shot metrics",gp=gpar(fontsize=18,font=3)))


Passes1 <- plots(KM5.m[166:185,])
Passes2 <- plots(KM5.m[186:205,])
legend<-g_legend(Passes2)
grid.arrange(arrangeGrob(Passes1 + theme(legend.position="none"), Passes2 + theme(legend.position="none"),ncol=2),
             legend, nrow=2,heights=c(10, 1),top=textGrob("Key pass metrics",gp=gpar(fontsize=18,font=3)))


other1 <- plots(KM5.m[126:150,])
other2 <- plots(KM5.m[c(151:165,206:220),])
legend<-g_legend(other2)
grid.arrange(arrangeGrob(other1 + theme(legend.position="none"), other2 + theme(legend.position="none"),ncol=2),
             legend, nrow=2,heights=c(10, 1),top=textGrob("Other metrics",gp=gpar(fontsize=18,font=3)))


# Lewandowski's replacement
Playerdata$Cluster <- km5$cluster
subset(Playerdata$Cluster, Playerdata$Player == "Robert Lewandowski")
# Cluster 2, there are only nine players in the same cluster as Lewandowski

data.frame(Players=subset(Playerdata$Player, Playerdata$Cluster == 2), Team=c("Tottenham", "Manchester City", "Liverpool FC",
       "Bayern Munchen", "Dortmund", "Napoli", "Barcelona", "Real Madrid", "Real Sociedad"))
