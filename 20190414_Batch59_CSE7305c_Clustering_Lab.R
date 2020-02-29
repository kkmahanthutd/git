rm(list=ls(all=TRUE))
library(fpc)

#Consider mtacrs data of R-datasets
data(mtcars)
str(mtcars)
mydata <- data.frame(mtcars[,1:7])
sum(is.na(mydata))
summary(mydata)
str(mydata)

mydata <- scale(mydata) # standardize variables 

# simple distance calculation between HOnda Civic and Camaro Z28
x <-mydata["Honda Civic",] 
y <- mydata["Camaro Z28",] 
dist(rbind(x, y),method="euclidean") #Is this method appropriate

# # distance between Camaroz28 and Firebird
# z <- mydata["Pontiac Firebird",] 
# dist(rbind(y, z))
# summary(mydata)

###-------------------------    Hierarchical Clustering     ------------------------###
# Ward's method 
# distance matrix euclidean
d <- dist(mydata,method = "euclidean") 

#View(data.matrix(d))
fit <- hclust(d, method="ward.D") #WARD is a min variance method to find compact clusters

plot(fit) # display dendogram
fit$merge
fit$dist.method


?hclust()

groups <- cutree(fit, k=5) # cut tree into 5 clusters
groups

#groups <- cutree(fit, k=2) # cut tree into 2 clusters
#groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 
mydata_clusters=data.frame(mydata,groups)
par(mfrow=c(2,2))
fit1 <- hclust(d, method="complete")
fit2 <- hclust(d, method="single")
fit3 <- hclust(d, method='average')
fit4 <- hclust(d, method='ward.D')

############
dev.off()
par(mfrow = c(2, 2))
plot(fit3,  main = "Average", xlab = "")
plot(fit4,  main = "Ward", xlab = "")
plot(fit2,  main = "Single", xlab = "")
plot(fit1,  main = "Complete", xlab = "")

### Finding Optimum number of clusters
#install.packages("factoextra",dependencies=T)
library(factoextra)
fviz_nbclust(mydata, hcut, method = "wss")

###-------------------------    K- means Clustering     ------------------------###
# K-Means Cluster Analysis with k = 4
set.seed(123)
fit <- kmeans(mydata, 4) # 4 cluster solution
fit$withinss
fit$betweenss
#study the model
fit$cluster
fit$tot.withinss
fit
fit$centers

#dev.off()
fviz_cluster(fit, mydata)

### Finding Optimum number of clusters
# K-means:  Determine number of clusters by considering the withinness measure
#Using factoextra library
fviz_nbclust(mydata, kmeans, method = "wss")
#k=6!?

# K-Means Cluster Analysis with k = 6
set.seed(123)
fit <- kmeans(mydata, 6) # 6 cluster solution

# append cluster label to the actual data frame
mydata <- data.frame(mydata, fit$cluster)
#write.csv(mydata,"kmeans_2.csv")
head(mydata)


###-------------------------  on unseen data   ------------------------###
# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- mtcars[sample(1:nrow(mtcars),1),]
closest.cluster <- function(x) {
        cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
        print(cluster.dist)
        return(which.min(cluster.dist)[1])
}

closest.cluster(test_datapoint)

###-------------------------  Quality check   ------------------------###
library(cluster)
#gower_dist = daisy(x = mydata, metric = "gower")
distance_matrix = daisy(x = mydata, metric = "euclidean")
clust_assignment = mydata$fit.cluster
sil_value_hc_mixed = silhouette(clust_assignment,
                                dist = distance_matrix)
plot(sil_value_hc_mixed)


###-------------------------  stability check   ------------------------###
#stabilitycheck
set.seed(123)
index <- (sample(nrow(mydata),.70*nrow(mydata)))
data <- mydata[index,]
fit2 <- kmeans(data,5)
data$clusters <- fit2$cluster

group1 <- mydata$fit.cluster[index]
group2 <- data$clusters

#loop dis for n imes. 
#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck
#across samples: avg_stabilitycheck
#Index value between 0 and 1, where 1 means the two clustering outcomes match identically.
#install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method="independence")
Stabindex



km_stability <- clusterboot(data = data, clustermethod=kmeansCBI,
                            krange = 5, seed = 123, B = 100, count = F, showplots = T)
groups_km = km_stability$result$partition 
table(groups_km)
#Cluster stability values
mean(km_stability$bootmean)
