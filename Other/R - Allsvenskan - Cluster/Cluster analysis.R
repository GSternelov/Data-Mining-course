library(dplyr)
library(RWeka)
library(ggplot2)
library(tidyr)
# Read in data
blad1 <- read.csv("C:\\Users\\Gustav\\Desktop\\R - Allsvenskan - Cluster\\Spelardata_blad1.csv", sep=";",stringsAsFactors=FALSE)
blad2 <- read.csv("C:\\Users\\Gustav\\Desktop\\R - Allsvenskan - Cluster\\Spelardata_blad2.csv", sep=";",stringsAsFactors=FALSE)
blad3 <- read.csv("C:\\Users\\Gustav\\Desktop\\R - Allsvenskan - Cluster\\Spelardata_blad3.csv", sep=";",stringsAsFactors=FALSE)
food <- read.arff("C:\\Users\\Gustav\\Desktop\\Data Mining - Master\\food.arff")

# Merge data sets
merge_test <- dplyr::left_join(blad1, blad2, by = c("First.name", "Last.name", "Team"))
merge_test2 <- dplyr::left_join(merge_test, blad3, by = c("First.name", "Last.name", "Team"))

# Data cleaning
merge_test2[merge_test2=="-" | is.na(merge_test2)] <- 0
for(i in 3:11){
  merge_test2[,i] <- as.numeric(merge_test2[,i])
}

# Create new variables
merge_test2 <- dplyr::mutate(merge_test2, NPG = G - STM, Min90 = Min/90, Sh_NPG=SKO/NPG, SOT_NPG=SKM/NPG,
                             NPG_Sh=NPG/SKO, NPG_SOT=NPG/SKM)

# Scale variables, measures per 90 min
merge_test2 <- dplyr::mutate(merge_test2, NPG90=NPG/Min90, A90=A/Min90, P90=(NPG+A)/Min90, Sh90=SKO/Min90, 
                             SOT90=SKM/Min90, Off90=OFF/Min90, FoulsA90=ORF/Min90, FoulsF90=TIF/Min90)
merge_test2[is.na(merge_test2) | merge_test2=="Inf"] <- 0

# Select interesting variables
FullData <- select(merge_test2, -c(17,18,20,21,26,28,29))
# Filter observations after playing time, min 8 games played
SubsetData <- dplyr::filter(FullData, Min90 >= 8 & P. != "GK")

## Perform cluster analysis ##
# Data to be analysed
SubsetData <- tidyr::unite(SubsetData, "Full_name", c(First.name, Last.name), sep=" ")
ggplot(SubsetData, aes(x=Sh90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=P.), size=5) + 
  geom_text(data=subset(SubsetData, Sh90 > 3.5 | Sh_NPG > 20))
ggplot(SubsetData, aes(x=Sh90, y=NPG_Sh)) + geom_point(aes(col=P.), size=5)
ggplot(SubsetData, aes(x=SOT90, y=SOT_NPG)) + geom_point(aes(col=P.), size=5)
ggplot(SubsetData, aes(x=SOT90, y=NPG_SOT)) + geom_point(aes(col=P.), size=5)

# Partitioning methods
Kmeans1 <- SimpleKMeans(SubsetData[, c(32,39,41)], Weka_control(N=5))
ggplot(SubsetData, aes(x=Sh90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=Kmeans1$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData, Sh90 > 3.5 | Sh_NPG > 20),vjust = 0, nudge_y = 0.7) +  
  scale_colour_gradientn(colours = rainbow(5))
ggplot(SubsetData, aes(x=Sh90, y=Off90, label=Full_name)) + geom_point(aes(col=Kmeans1$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData,Off90 > 1.35 | Sh90 > 4),vjust = 0, nudge_y = 0.05) +
  scale_colour_gradientn(colours = rainbow(5))
ggplot(SubsetData, aes(x=Off90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=Kmeans1$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData,Off90 > 1.35 | Sh_NPG > 25),vjust = 0, nudge_y = 0.7) +
  scale_colour_gradientn(colours = rainbow(5))

# With XMeans choosing number of clusters
cl2 <- XMeans(SubsetData[, c(32,39,41)], c("-L", 3, "-H", 7, "-use-kdtree",
                "-K", "weka.core.neighboursearch.KDTree -P"))
ggplot(SubsetData, aes(x=Sh90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=cl2$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData, Sh90 > 3.5 | Sh_NPG > 20),vjust = 0, nudge_y = 0.7) +  
  scale_colour_gradientn(colours = rainbow(3))
ggplot(SubsetData, aes(x=Sh90, y=Off90, label=Full_name)) + geom_point(aes(col=cl2$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData,Off90 > 1.35 | Sh90 > 4),vjust = 0, nudge_y = 0.05) +
  scale_colour_gradientn(colours = rainbow(3))
ggplot(SubsetData, aes(x=Off90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=cl2$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData,Off90 > 1.35 | Sh_NPG > 25),vjust = 0, nudge_y = 0.7) +
  scale_colour_gradientn(colours = rainbow(3))

cl2v.2 <- XMeans(SubsetData[, c(39,41)], c("-L", 3, "-H", 7, "-use-kdtree",
                                           "-K", "weka.core.neighboursearch.KDTree -P"))
ggplot(SubsetData, aes(x=Sh90, y=Sh_NPG, label=Full_name)) + geom_point(aes(col=cl2v.2$class_ids, shape=P.), size=5) + 
  geom_text(data=subset(SubsetData, Sh90 > 3.5 | Sh_NPG > 20),vjust = 0, nudge_y = 0.7) +  
  scale_colour_gradientn(colours = rainbow(3))

# Only on strikers
StrikerData <- dplyr::filter(SubsetData, P. == "M")
strikerC2 <- SimpleKMeans(StrikerData[, c(38,42)], Weka_control(N=2))
strikerC3 <- SimpleKMeans(StrikerData[, c(38,42)], Weka_control(N=3))
strikerC4 <- SimpleKMeans(StrikerData[, c(38,42)], Weka_control(N=4))
ggplot(StrikerData, aes(x=P90, y=FoulsA90, label=Full_name)) + geom_point(aes(col=strikerC2$class_ids), size=5) + 
  geom_text(vjust = 0, nudge_y = 0.005,check_overlap = TRUE) +  
  scale_colour_gradientn(colours = rainbow(2))
ggplot(StrikerData, aes(x=P90, y=FoulsA90, label=Full_name)) + geom_point(aes(col=strikerC3$class_ids), size=5) + 
  geom_text(vjust = 0, nudge_y = 0.02,check_overlap = TRUE) +   
  scale_colour_gradientn(colours = rainbow(3))
ggplot(StrikerData, aes(x=P90, y=FoulsA90, label=Full_name)) + geom_point(aes(col=strikerC4$class_ids), size=5) + 
  geom_text(vjust = 0, nudge_y = 0.005,check_overlap = TRUE) +  
  scale_colour_gradientn(colours = rainbow(4))


# Hierarchical methods



# Density based methods
DBScan(SubsetData[, c(39,41)], control = NULL)
WOW(optics_dbScan)

WPM("install-package", "optics_dbScan") 
WPM("load-package", "optics_dbScan") 
make_Weka_classifier("optics_dbScan") 

