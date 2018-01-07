
#lab05_bootstrapping

#When we did k-means clustering, we used the total within sum of squares to choose an appropriate value of k. 
#It was diﬃcult to decide when there was an “elbow” in the plot. 
#We could use bootstrapping to estimate the variability of the within sum of squares. We could then use the values to establish 
#what value of k might be most appropriate.


data("faithful")

##Count how many observations are in the data...
N = nrow(faithful)

##Decide how many bootstrap samples to take..
B = 50

##Choose the maximum number of clusters to fit..
kmax = 10

##Set up a matrix to store the results
result = matrix(N,B,kmax)

##Loop over the number of bootstrap samples (from 1 to B)..."b"
for(b in 1:B) {
  ##Loop over the number of clusters from 1 to Kmax..."k"
  for(k in 1:kmax) {
  
  #Sample the observation indices with replacement????????
  index = sample(1:N, replace = T)
  #Make a copy of the data with each observation occurring as often as its index happens
  xnew = faithful[index, ]
  #Run k-means for this value of k. Do 4 random starts to avoid local minima???
  fit <- kmeans(xnew, centers = k, nstart = 4)
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

#In this bootstrap simulation, the 95% bootstrap intervals for the withinSS start to overlap once k is 5 of larger...
#this means we have little evidence of needing more than 5 groups to cluster the data....


#Re-run the analysis of the Old Faithful data with the variables scaled to have mean 0 and standard deviation 1. 
#The data can be easily scaled using the scale() command.







































