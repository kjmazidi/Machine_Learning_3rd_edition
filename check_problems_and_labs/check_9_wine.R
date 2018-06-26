# Check Your Understanding
# kNN
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# red and white wine csv files were combined into one
# with new column type


#############################################
# 9.1
# load wine data  6497 x 12
wine <- read.csv("wine_all.csv", header=TRUE)
wine$type <- as.integer(wine$type)
str(wine)
attach(wine)

# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wine <- as.data.frame(lapply(wine, normalize))

# k means
set.seed(1234)
wine_clust <- kmeans(wine, 2, nstart=20)
wine_clust

cor(wine_clust$cluster, type)  # 1 

# plot is too dense with all data so randomly select 250
j <- sample(1:nrow(wine), 250, replace=FALSE)
# plot - using pH and alcohold to spread out the points
# color = white/red wine
# shape = cluster identified by kmeans
plot(pH[j], alcohol[j], pch=wine_clust$cluster[j], 
     col=c("brown3","goldenrod1")[wine$type[j]+1],
     xlab="alcohol", ylab="pH")
# only plotting in 2 dimensions, in 13-d space, white and red wines must be "close"
#############################################
# 9.2

d <- dist(wine)  # data was normalized above
fit.average <- hclust(d, method="averag")
plot(fit.average, hang=-1, cex=.8)

# try fewer examples
j <- sample(1:nrow(wine), 10, replace=FALSE)
d <- dist(wine[j,])  # data was normalized above
fit.average <- hclust(d, method="averag")
plot(fit.average, hang=-1, cex=.8)

# compare 1085 and 123   # diff = -0.34
sum(wine[123,] - wine[1085,])
# compare 1085 and 5994
sum(wine[1085,] - wine[5994,])  # diff = -1.07, 3 times as much