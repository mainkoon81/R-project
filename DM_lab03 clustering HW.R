
#lab03_Assessing clustering

# 1. Silhouette
#The silhouette plot is used to assess cluster conherence. 
#Observations with HIGH silhouette value are much closer to their own cluster members than the members of other clusters
#Observations with low (or even negative) silhouette are close to other cluster members and thus may have 
#ambiguous cluster membership. 

#silhouette()takes two arguments: a clustering of the data and a distance matrix. 

data("iris")
#first extract the numerical variables of interest to a new matrix.
X = iris[,-5]
pairs(X)

k=3

fitkm <- kmeans(X, centers = k); fitkm #it's a list...

#plot the data colored by clusters
?rainbow
clcolor = rainbow(k, alpha = 0.2); colvec # ?????????????? set up color palette..
pairs(X, col=clcolor[fitkm$cluster], pch=19)


library(cluster)

#find a dissimilarity matrix using "squared Euclidean distance"!
dist.iris <- dist(X,'euclidean', diag = T); dist.iris
dist.iris <- dist(X, 'euclidean')^2; dist.iris ################# what's the difference?

#compute the silhouette for each obv..
sil.iris <- silhouette(fitkm$cluster, dist.iris); plot(sil.iris)


#let's plot!
#which values or obv have low/high silhouette?----using a scatterplot matrix!
#The more red colored the plot symbol, the higher the sihouette. The more blue colored the plot symbol, the lower the silhouette.
##Observations with negative silhouette are blue but use a + symbol..

# Store the silhouette value in a vector..
silval <- sil.iris[,3]

# Choose the plot symbol for the positive/negative silhouette observations 
pchvec <- rep(19, nrow(X))
pchvec[silval<0] <- 3 

# Replace negative silhouettes with zero (just for plot coloring) 
silval[silval<0] <- 0


pairs(X, col=rgb(silval, 0, 1-silval, alpha = 0.2), pch=pchvec) #????????



# How does the cluster consistency/coherence change with K???????
##################################################################################################################################

# 2. Rand Index
#The 'e1071' package can be used for compute the Rand and adjusted Rand indices. 
#The command for computing the Rand (and adjusted Rand) is classAgreement(). 

#Let's use it to compare the results of k-means and k-medoids (with Manhattan distance) with K = 3.

#k-means
k=3
fitkm <- kmeans(X, centers = k, nstart = 10); fitkm

#k-medoids
dist.iris <- dist(X, 'manhattan', diag = T); dist.iris
fitkmed <- pam(dist.iris, k=k); fitkmed

#tabulate the results
tab <- table(fitkm$cluster, fitkmed$clustering); tab


#Compute the Rand and adjusted Rand indices 
library(e1071)
classAgreement(tab)



#How do the results di???er if we change K or even make K di???erent for both algorithms?

#Look at the matchClasses() command. 
#This command can ???nd the "best" mapping of one clustering result to the another. 
#The method option within the command is worth investigating too (but some options only work 
#when the K values match for both clusterings).








# homework02 ###################################################################################################################
#The International Association of Ultrarunners (IAU) world championship, World Masters Athletics (WMA) world championship 
#and an open race (OP) over 100 km were held simultaneously in Los Alcazares. Spain on 27th November 2016. The course of the 
#race consisted of 10×10km lap. The lap speed (in metres per second) of each competitor was recorded at the end of each of 
#the 10 laps. The race entered (IAU, WMA and OP) and the gender of the athlete (M and F) are also recorded. The data are stored 
#in laps100km.Rdata (in Data Folder on Blackboard). 
# Analyze the data to establish if there are clusters of athletes with similar lap speeds throughout the race. 
# Comment on any clusters found. Also, investigate if the clusters correspond to the type of race entered or the gender of
# the athlete.
load("C:/Users/Minkun/Desktop/r_practice/+++++++++++School R/LAB_data mining/data/laps100km.Rdata")
class(X)
head(X)
dim(X)
levels(X$class)
levels(X$sex)


#first extract the numerical variables of interest to a new matrix.
X.new = X[, 1:10]; X.new
plot(X.new)
class(X.new)

#there are three races associated with this dataset, so k=3
#there are two genders associated with this dataset, so k=2
#then compare which clustering is much tighter, using silhouette and rand index.


#want to plot SS VS k coz we need to find k....###########################################################################
WGSS = rep(0,6)
n = nrow(X.new)

# if k=1, add up all SS...what we calulate here is var so need to delete the bottom of var ==> (n-1) coz we need SS.. 
WGSS[1] = (n-1)*sum(apply(X.new, 2, var))

#WGSS was designed to store up each SS according to different k!
for(k in 2:6){
  WGSS[k] <- sum(kmeans(X.new, centers = k)$withinss) # Within cluster sum of squares by cluster..
} 

#we want to plot k versus WGSS. we can find the best k that is shown by the elbow in the curve.
plot(1:6, WGSS, type = 'b', xlab = 'k', ylab = 'wgSS')
###########################################################################################################################
#the k versus WGSS plot suggests k = 2 is a good clustering solution.










##############################################
# Are those data assiciated with races? (k=3)?

kmcl.X <- kmeans(X.new, centers = 3, nstart = 100); kmcl.X #kmeans cluster takes the dataset...k=3
table(kmcl.X$cluster)

#let's assess the output of k-means clustering using silhouette.
library(cluster)
#find a dissimilarity matrix using "squared Euclidean distance"!
dist.lap <- dist(X.new, 'euclidean')^2

#compute the silhouette for each obv..
sil.race <- silhouette(kmcl.X$cluster, dist.lap); plot(sil.race)

#when k=3, compare the clustering results to the class: races.
colvec.race <- as.numeric(X[,11]); colvec.race  #real....extract level vectors
kmcl.X$cluster #estimation 

# fuck... too many variables...don't plot...
plot(X.new, col=colvec.race, pch=kmcl.X$cluster)

#tabulate the results
tab <- table(colvec.race, kmcl.X$cluster); tab

#Compute the Rand and adjusted Rand indices 
library(e1071)
classAgreement(tab)



##############################################
# Are those data assiciated with gender? (k=2)?

kmcl.X <- kmeans(X.new, centers = 2); kmcl.X #kmeans cluster takes the dataset...k=2
table(kmcl.X$cluster)

#let's assess the output of k-means clustering using silhouette.
#find a dissimilarity matrix using "squared Euclidean distance"!



#compute the silhouette for each obv..
sil.gen <- silhouette(kmcl.X$cluster, dist.lap); plot(sil.gen)

#when k=2, compare the clustering results to the class: genders.
colvec.gen <- as.numeric(X[,12]); colvec.gen #real....extract level vectors
kmcl.X$cluster #estimation 

# fuck... too many variables...don't plot...
plot(X.new, col=colvec.race, pch=kmcl.X$cluster)

#tabulate the results
tab <- table(colvec.gen, kmcl.X$cluster); tab

#Compute the Rand and adjusted Rand indices 
classAgreement(tab)



#When we did k-means clustering, we used the total within sum of squares to choose an appropriate value of k. 
#It was di„0²5cult to decide when there was an ¡°elbow¡± in the plot. 
#We could use bootstrapping to estimate the variability of the within sum of squares. We could then use the values to establish 
#what value of k might be most appropriate.



##Count how many observations are in the data...
n = nrow(X.new)

##Decide how many bootstrap samples to take..
BS = 100

##Choose the maximum number of clusters to fit..
kmax = 20

##Set up a matrix to store the results
result = matrix(n,BS,kmax)

##Loop over the number of bootstrap samples (from 1 to BS)..."b"
for(b in 1:BS) {
  ##Loop over the number of clusters from 1 to Kmax..."k"
  for(k in 1:kmax) {
    
    #Sample the observation indices with replacement????????
    index = sample(1:n, replace = T)
    #Make a copy of the data with each observation occurring as often as its index happens
    X_2 = X.new[index, ]
    #Run k-means for this value of k. Do 4 random starts to avoid local minima???
    fit <- kmeans(X_2, centers = k, nstart = 4)
    #Store the total within sum of squares
    result[b,k] = fit$tot.withinss 
    
  }#closing loop over k
}#closing loop over b


##compute summaries from our output to get a feeling for what k to use...
#Compute summaries for the within SS for each k. 
apply(result,2,summary)


##Compute the 2.5%, 50% and 97.5% quantiles of the statistic for each k 
withinss = apply(result, 2, quantile, probs=c(0.025, 0.5, 0.975)); withinss
matplot(t(withinss))





#In this bootstrap simulation, the 95% bootstrap intervals for the withinSS start to overlap once k is 12 of larger...
#this means we have little evidence of needing more than 12 groups to cluster the data....

#Re-run the analysis of the data with the variables scaled to have mean 0 and standard deviation 1. 
#The data can be easily scaled using the scale() command.










