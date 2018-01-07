
#lab02_CLUSTERING

#1>
#k-means on the iris data (with k = 3)......
#We want to cluster the measurement data but the dataset gives the true species for each flower in column 5.
#Thus, we remove column 5 from the data and store the resulting data as X
data(iris)
head(iris)
dim(iris)
class(iris)
plot(iris, col=iris$Species)

X <- iris[, -5] 
pairs(X)
plot(X)

# Run k-means (with K=3) and store the resulting fit... in 'fitkm.'
fitkm <- kmeans(X, centers = 3); fitkm

# converging to a local minimum? don't know? ,then Run k-means (with K=3 with 10 random starts)..
fitkm <- kmeans(X, centers = 3, nstart = 10); fitkm



#####Let's compare the clustering results to the iris species. #############################

#For ploting, let's make a vector of numbers # with each number corresponding to a species.. 
iris[,5]
colvec <- as.numeric(iris[,5]); colvec #wow unbelievable!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#this is extracting the real cluster vector (forcing numeric!!!)

#Plot the "true" clustering.The plot symbol (pch) and color (col) correspond to 'species' 
plot(X, col=colvec, pch=colvec) #wowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowowow COLor PCH-pt-shape.. 

#Plot the 'k-means' clustering.The plot symbol (pch) and color (col) correspond to 'cluster' 
fitkm$cluster #this is the estimated cluster vector

sort(fitkm$cluster)
table(fitkm$cluster, colvec) #lets compare..

plot(X, col=fitkm$cluster, pch=fitkm$cluster)

#put them side by side
par(mfrow= c(1,2)) #what?
plot(X, col=colvec, pch=colvec, main='species')
plot(X, col=fitkm$cluster, pch=fitkm$cluster, main='clusters')
par(mfrow= c(1,1)) #what?

#what if...col=colvec, pch=fitkm$cluster ???
plot(X, col=colvec, pch=fitkm$cluster)


# Can you ???nd the cluster centres in the output? Can you ???nd the total sum of squares?
#-----------------------------------------
K-means clustering with 3 clusters of sizes 38, 50, 62

Cluster means:
  Sepal.Length Sepal.Width Petal.Length Petal.Width
1     6.850000    3.073684     5.742105    2.071053
2     5.006000    3.428000     1.462000    0.246000
3     5.901613    2.748387     4.393548    1.433871

Within cluster sum of squares by cluster:
  [1] 23.87947 15.15100 39.82097
(between_SS / total_SS =  88.4 %)
#-----------------------------------------find each mu that minimizing SS? mu = (sum of each X) / (size of each cluster) 

points(fitkm$centers, col=1:3, pch=8) #????????



#2> EXTRA
# HOW TO SCALE DATA?
#We usually scale data to have mean 0 and standard deviation 1 for each variable in the data.
Xs = scale(X); Xs

pairs(Xs)

fitkm2 <- kmeans(Xs, centers = 3, nstart = 10); fitkm2
colvec <- as.numeric(iris[,5]); colvec 

plot(Xs, col=colvec, pch=colvec, main='species')
plot(Xs, col=fitkm2$cluster, pch=fitkm2$cluster, main='clusters')
plot(Xs, col=colvec, pch=fitkm2$cluster)

#To the clustering results change if you scale the data?









