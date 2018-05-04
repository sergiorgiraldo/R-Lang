# K Means Clustering is an unsupervised learning algorithm that tries to cluster data based on
# their similarity. Unsupervised learning means that there is no outcome to be predicted, and the
# algorithm just tries to find patterns in the data. In k means clustering, we have the specify 
# the number of clusters we want the data to be grouped into. The algorithm randomly assigns each
# observation to a cluster, and finds the centroid of each cluster. Then, the algorithm iterates
# through two steps:
# 
#     Reassign data points to the cluster whose centroid is closest.
#     Calculate new centroid of each cluster.
#  
# These two steps are repeated till the within cluster variation cannot be reduced any further.
# The within cluster variation is calculated as the sum of the euclidean distance between the
# data points and their respective cluster centroids.

library(datasets)
library(ggplot2)
head(iris)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
set.seed(20)
irisCluster <- kmeans(iris[, 1:2], 5, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = irisCluster$cluster)) + geom_point()

not.error <- unclass(iris$Species) == irisCluster$cluster
iris$noerror <- not.error
#See errors 
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) +
  geom_point(size = 3, alpha = 0.5, aes(shape = iris$noerror))
