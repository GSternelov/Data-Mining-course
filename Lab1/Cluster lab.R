library(RWeka)
library(ggplot2)
library(gridExtra)
library(GGally)

food <- read.arff("C:\\Users\\Gustav\\Desktop\\Data Mining - Master\\food.arff")

pairs(food[, 2:6])
ggpairs(food[, 2:6], title = "Scatterplot matrix")
# Simple K-means
# Always ignore attribute 'Name', why? 
# It's a categorical variable. Also, is just the name of the respective products.
# Unless categories are created of the products they aren´t giving any useful information.

# Chooses the attributes energy, fat and protein. Why? Uses scatterplots and selects those who seem 
# most interesting. Calcium do not seem to be particulary interesting. All points in abunch and a few outliers.

foodVars <- food[, c(2:4, 6)]

# Performs SimpleKMeans clustering
Kmeans2 <- SimpleKMeans(foodVars, Weka_control(N=2))
Kmeans4 <- SimpleKMeans(foodVars, Weka_control(N=4))
#Graphs 
{
means2Var12 <- ggplot(food, aes(x=Energy, y=Protein)) + geom_point(aes(col=Kmeans2$class_ids),size=5) + theme(legend.position="none") +
  ggtitle("Energy vs Protein")
means2Var13 <- ggplot(food, aes(x=Energy, y=Fat)) + geom_point(aes(col=Kmeans2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Fat")
means2Var14 <- ggplot(food, aes(x=Energy, y=Iron)) + geom_point(aes(col=Kmeans2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Iron")
means2Var23 <- ggplot(food, aes(x=Protein, y=Fat)) + geom_point(aes(col=Kmeans2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Fat")
means2Var24 <- ggplot(food, aes(x=Protein, y=Iron)) + geom_point(aes(col=Kmeans2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Iron")
means2Var34 <- ggplot(food, aes(x=Fat, y=Iron)) + geom_point(aes(col=Kmeans2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Fat vs Iron")
grid.arrange(means2Var12, means2Var13,means2Var14,means2Var23,means2Var24,means2Var34, ncol=2)

means4Var12 <- ggplot(food, aes(x=Energy, y=Protein)) + geom_point(aes(col=Kmeans4$class_ids),size=5) + theme(legend.position="none") +
  ggtitle("Energy vs Protein") +  scale_colour_gradientn(colours = rainbow(4))
means4Var13 <- ggplot(food, aes(x=Energy, y=Fat)) + geom_point(aes(col=Kmeans4$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Fat")+  scale_colour_gradientn(colours = rainbow(4))
means4Var14 <- ggplot(food, aes(x=Energy, y=Iron)) + geom_point(aes(col=Kmeans4$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
means4Var23 <- ggplot(food, aes(x=Protein, y=Fat)) + geom_point(aes(col=Kmeans4$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Fat")+  scale_colour_gradientn(colours = rainbow(4))
means4Var24 <- ggplot(food, aes(x=Protein, y=Iron)) + geom_point(aes(col=Kmeans4$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
means4Var34 <- ggplot(food, aes(x=Fat, y=Iron)) + geom_point(aes(col=Kmeans4$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Fat vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
grid.arrange(means4Var12, means4Var13,means4Var14,means4Var23,means4Var24,means4Var34, ncol=2)
}


# Do the same thing, but for a different seed

Kmeans2v2 <- SimpleKMeans(foodVars, Weka_control(N=2, S=28))
Kmeans4v2 <- SimpleKMeans(foodVars, Weka_control(N=4,S=28))

{
means2v2Var12 <- ggplot(food, aes(x=Energy, y=Protein)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5) + theme(legend.position="none") +
  ggtitle("Energy vs Protein")
means2v2Var13 <- ggplot(food, aes(x=Energy, y=Fat)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Fat")
means2v2Var14 <- ggplot(food, aes(x=Energy, y=Iron)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Iron")
means2v2Var23 <- ggplot(food, aes(x=Protein, y=Fat)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Fat")
means2v2Var24 <- ggplot(food, aes(x=Protein, y=Iron)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Iron")
means2v2Var34 <- ggplot(food, aes(x=Fat, y=Iron)) + geom_point(aes(col=Kmeans2v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Fat vs Iron")
grid.arrange(means2v2Var12, means2v2Var13,means2v2Var14,means2v2Var23,means2v2Var24,means2v2Var34, ncol=2)

means4v2Var12 <- ggplot(food, aes(x=Energy, y=Protein)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5) + theme(legend.position="none") +
  ggtitle("Energy vs Protein") +  scale_colour_gradientn(colours = rainbow(4))
means4v2Var13 <- ggplot(food, aes(x=Energy, y=Fat)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Fat")+  scale_colour_gradientn(colours = rainbow(4))
means4v2Var14 <- ggplot(food, aes(x=Energy, y=Iron)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Energy vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
means4v2Var23 <- ggplot(food, aes(x=Protein, y=Fat)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Fat")+  scale_colour_gradientn(colours = rainbow(4))
means4v2Var24 <- ggplot(food, aes(x=Protein, y=Iron)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Protein vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
means4v2Var34 <- ggplot(food, aes(x=Fat, y=Iron)) + geom_point(aes(col=Kmeans4v2$class_ids),size=5)+ theme(legend.position="none")+
  ggtitle("Fat vs Iron")+  scale_colour_gradientn(colours = rainbow(4))
grid.arrange(means4v2Var12, means4v2Var13,means4v2Var14,means4v2Var23,means4v2Var24,means4v2Var34, ncol=2)
}

# For 2 clusters the results are very similar. For 4 clusters the results changes.
# The seed controls the initial seeds. The starting points for the respective centroids. 

# I think that the clusters obtained when the number of clusters is set to 2 is quite okey.
# The difference are in most cases clear, values in the different clusters are dissimilar,
# and it is in almost all cases possible to give a natural explanation of the clusters.
# Protein vs Iron is the only case when this not is true.

# Equal protein - High energy
# Low fat and energy - High fat and energy
# Approx equal iron - High energy
# Approx equal protein - High fat
# Protein vs Iron - hard to say anything
# Approx equal Iron - High fat

## The second cluster, dark blue points, has higher levels of energy and of fat.
# It has low variance for values of Protein and Iron.
# The first cluster separates food with high levels of energy and fat.
## The First cluster, light blue points, has lower values of energy and fat.
# More spread values for Protein and Iron.
# Is a low energy and low fat cluster.  


