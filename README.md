# R-project-ML-Basic(01)

## Multivariate_Analysis / Machine Learning playground with R

### [Contents] 

__Lab-01.__ Association Rule Mining 
  - package: arules 
  - func:

__Lab-02.__ Clustering 
  - package: cluster, e1071, mclust 
  - func: `kmean()`,  

__Lab-03.__ Regression 
  - package: MASS, binomTools, nnet, ROCR
  - func: `glm()`, `multinom()` 

----------------------------------------------------------------------
#### >Lab-01. Association Rule Mining

<img src="https://user-images.githubusercontent.com/31917400/32491154-907d38fa-c3ad-11e7-95a2-7a5ce5588d81.jpg" />

__Data:__ The Data were collected recording votes in the Irish parliament (D´ailEireann), prior to the general election, in early 2016. Extra details of the votes can be found at (http://www.oireachtas.ie/parliament/) and the data are for the votes on January 14th-28th.

__Story:__ Association rule mining has a purpose to find frequent co-occurring relationships among a collections of independent items in the dataset so that we can predict the occurrance of next items. In this project, we aim to discover and investigate certain patterns or trends that might reside in their **voting behaviour** on diverse (23) propositions. Each column numbers in the dataset refers each different proposition put to those votes. And we tag each ‘YES-votes’, ‘NO-votes’, using those column numbers that reflect what propositions those voting choices correspond to. 

What we focus on, in the beginning, is to find association pattern that reveals interesting connections between ‘Yes-votes.’ This analysis is repeated between ‘No-votes’ as well. **Given that the association patterns we are looking for are not subject to the causal and effect attributes in the dataset, it is not a bad idea to rely more on seeking co-occurrence relationships** between items. Likewise, when it comes to the choice of quality measures as a threshold for rule mining, **Support measure** over Confidence or Lift seems to be the most convincing as Support, by definition, embodies the intersecting (co-occurrence) relationship between items. However, Lift measure also can be useful to verify reliability of the association rules that are generated by Support measure. For example, if a Lift-value is greater than 1, it represents a positive effect on the occurrence of one another. If a Lift-value is equal to 1, it means the occurrences of the item are independent each other. If a Lift-value is less than 1, it means the proposed occurrence has a probability less than 1.  


#### *|Data Wrangling + Mining|*

<img src="https://user-images.githubusercontent.com/31917400/32518404-5a73d148-c401-11e7-909f-0817c2f1cfde.jpg" width="600" height="200" />

- __issue:__ We need to get the data into an appropriate binary format (from Y's and N's to 1’s and 0’s) before analysis, then set it up in transaction format. The matrix is not a numeric binary matrix. Once converting the data into 1 and 0 and doing a `summary(data)` we see that the matrix is reading the columns in as factor variables and is not a numeric matrix. To convert the matrix as a numeric matrix, we use the command `as.numeric` on each column in the dataset.

- **[Yes-vote analysis]**
```
for(i in 1:ncol(votes)){
  votes[which(votes[,i]=="n"),i]<-0
  votes[which(votes[,i]=="a"),i]<-0
  votes[which(votes[,i]=="y"),i]<-1 
}
vote_Y<-apply(votes,2,function(x)as.numeric(x))

colnames(vote_Y) <- paste('Y', 1:23, sep=''); vote_Y
transvote_Y <- as(vote_Y, 'transactions'); inspect(transvote_Y)

fit<-apriori(transvote_Y, parameter=list(support=0.3, minlen=1, maxlen=10)) 
fit<-sort(fit,by="support")
inspect(fit)
```
<img src="https://user-images.githubusercontent.com/31917400/32521319-f7470400-c40a-11e7-9b57-46d36f3a3284.jpg" width="600" height="200" />
<img src="https://user-images.githubusercontent.com/31917400/32520233-2cf894be-c407-11e7-8a6e-20271aa4069d.jpg" width="600" height="200" />

- __Interpretation:__ Looking at the dataset in the transaction format overall, we notice these 13 politicians out of 166 in total – Caolin Caoimhgh, Snodaigh Aengus, Colreavy Michael, Dooley Timmy, Fitzmaurice Michael, Healy-Rae Michael, Mathews Peter, McConalogue Charlie, McGrath Finian, McLellan Sandra, Murphy Catherine, Sullivan Maureen, Wallace Mick – show a higher tendency to vote yes (13 to 14 times) in favour of the motions  while 28 politicians out of 166 in total -  Bannon James, Barrett, Collins Joan, Conlan, Coppinger Ruth, Creighton Lucinda,  Crowe, Ellis Dessie, Ferris Martin, Flanagan Terence, Gilmore Eamon, Higgins Joe, Keaveney Colm, Kelly Alan, Kenny Enda, Lowry Michael, Maloney Eamonn, McDonald Mary Lou, McLoughlin Tony, Murphy Paul, Noonan Michael, Dea Willie, Penrose Willie, Perry John, Shatter Alan, Smith Brendan,  Stanley Brian, Timmins Billy – hardly voted to yes (never or just once) on any propositions. 
The Apriori algorithm gives us several rules with ‘Support’ threshold greater than 0.3 and it shows that there is a strong relationship between “Yes-votes to the motion 4, 5, 21, 22.” 
This demonstrates the most frequent co-occurrence – 36% to 48% (by Support) with the Lift values closer to 2. Specifically, the motion 4 and 5 are for the “improvement of social protection” and Fine Gael, Labour appear to be the major advocate of the motions. The motion 21 and 22 are sharing the issue of “improvement of social housing” and Fine Gael, Labour show again higher inclination to vote yes, but the motions did not pull enough votes compared to the motion 4 and 5. 

- **[No-vote analysis]**
```
for(i in 1:ncol(votes)){
  votes[which(votes[,i]=="n"),i]<-1
  votes[which(votes[,i]=="a"),i]<-1
  votes[which(votes[,i]=="y"),i]<-0 
}
vote_N <-apply(votes,2,function(x)as.numeric(x)); vote_N

colnames(vote_N) <- paste('N', 1:23, sep=''); vote_N
transvote_N <- as(vote_N, 'transactions'); inspect(transvote_N)

fit<-apriori(transvote_N, parameter=list(support=0.3, minlen=3)) 
fit<-sort(fit,by="support")
inspect(fit)
```
<img src="https://user-images.githubusercontent.com/31917400/32521327-fad294ae-c40a-11e7-8e3a-7e296df5ac13.jpg" width="600" height="200" />
<img src="https://user-images.githubusercontent.com/31917400/32520365-a3d9d6b0-c407-11e7-9934-0a590c4aa83a.jpg" width="600" height="200" />

- __Interpretation:__ There are several rules that attract our attention. ‘Support’ threshold greater than 0.3 reveals two different groups of associations rules between No-votes. One is for the motion 18, 19, 20 and the other is for the motion 10, 11, 12, 13, 14, but the first one manifests slightly stronger correlation. The motion 18, 19, 20 are all about criminal justice and the motion 10, 11, 12, 13, 14 address consistently some issues arising from the dissolution of certain educational institution. We can say that the Irish politicians shows certain consistency in voting behaviour for the motions in similar threads. In sum, this association rule analysis can be seen successful. This is because its result and those suggested association rules effectively picked up a series of the political motions possibly in similar threads and they are clearly in concordance with actual data recorded in the website. Therefore, it is safe to say that those association rules yielded by a priori algorithm turn out to be a reliable source of reference to predict the pattern of behaviour of Irish politicians.    
-------------------------------------------------------------------------------
#### >Lab-02. Clustering

<img src="https://user-images.githubusercontent.com/31917400/32509308-d95aa8be-c3e4-11e7-9be4-02d14275584a.jpg" />
<img src="https://user-images.githubusercontent.com/31917400/32509312-e00f8c2e-c3e4-11e7-889a-d7dd939d8200.jpg" />

__Data:__ The International Association of Ultrarunners (IAU) world championship, World Masters Athletics (WMA) world championship and an open race (OP) over 100 km were held simultaneously in Los Alcazares, Spain on 27th November 2016. The course of the race consisted of 10×10km lap. The lap speed (in metres per second) of each competitor was recorded at the end of each of the 10 laps. The race entered (IAU, WMA and OP) and the gender of the athlete (M and F) are also recorded. Extra details of the votes can be found at (http://100kworldchampionship2016.com/)  

__Story:__ We want to analyze the data to establish if there are clusters of athletes with similar lap speeds throughout the race, and investigate if the clusters correspond to the type of race entered or the gender of the athlete. 

#### *|K-means Clustering|*

<img src="https://user-images.githubusercontent.com/31917400/32523548-adc68248-c413-11e7-88f0-861e2bad997c.jpg" width="600" height="170" />

- __issue:__ We can see that there are two levels - male, female - in the “sex” variable and three levels - IAU, OP, WMA - in the “class” variable. We’d like to see that if our result of clustering analysis would correspond to those levels. But how many “k” do we need? K=2 or K=3 ? We want to make our clusters as tight as possible in order to improve the accuracy of our clustering analysis and this goal seems to be achievable by minimizing Sum of Squares. Interestingly, the value of Sum of Squares diminishes as we have more clusters which is represented by the value of “k.” Therefore, what we need is the plot of “k”values against the Sum of Squares so that we can find the elbow of the curve that manifesting the most desirable ‘”k” value. 

- **[Exploration : Searching the best 'K'?]**
```
##Extracting the numerical variables of interest##
X.new = X[, 1:10]
plot(X.new) 

##Setting the limit (k=6)##
WGSS = rep(0,6) 
n = nrow(X.new)

##If k=1, add up all SS. What we calulate here is variance so take off the bottom of variance ==> (n-1)##
WGSS[1] = (n-1)*sum(apply(X.new, 2, var))

##WGSS was designed to store up each 'Within cluster(group) sum of squares' in accordance with different k##
for(k in 2:6){
  WGSS[k] <- sum(kmeans(X.new, centers = k)$withinss) 
} 

#Plotting k versus WGSS##
plot(1:6, WGSS, type = 'b', xlab = 'k', ylab = 'wgSS')
```
**The k versus WGSS plot suggests k = 2 is the best clustering solution.**
<img src="https://user-images.githubusercontent.com/31917400/32524508-09c66fdc-c418-11e7-9b0e-9e304ab9fc8c.jpeg" width="500" height="250" />

- **[Actual Investigation I: Are those similar lap speeds associated with races-IAU/OP/WMA? (k=3)]**

More precisely, we compare the clustering result with the “class” variable in the original dataset, using cross tabulation and Rand Index.  
```
## kmeans() takes the dataset (k=3)##
kmcl.X <- kmeans(X.new, centers = 3, nstart = 100); kmcl.X 
table(kmcl.X$cluster)

##Assessing the output of k-means clustering using silhouette and finding a 'dissimilarity matrix' using 'squared Euclidean distance'##
dist.lap <- dist(X.new, 'euclidean')^2

##Computing the silhouette for each observation.##
sil.race <- silhouette(kmcl.X$cluster, dist.lap); plot(sil.race)

##when k=3, in order to compare the clustering results to the class, we extract the column: "class"##
colvec.race <- as.numeric(X[,11])

##tabulating the results##
tab <- table(colvec.race, kmcl.X$cluster); tab

##Computing the Rand and adjusted Rand indices## 
classAgreement(tab)
```

**As we expected, the Silhouette plot reveals unreliability of the k=3 clustering result by presenting the negative silhouette width in the second cluster** (rand index value: 0.5782136). As seen in the output as follows, there is no consistency in the matching pattern between the actual clusters in the dataset and estimated clusters produced by k=3 means algorithm. Therefore, we can conclude that there is no notable connection between the recorded lap speed of athletes and the type of races - IAU, OP, WMA. 

<img src="https://user-images.githubusercontent.com/31917400/32546229-49220e94-c476-11e7-9064-697a91f9dd9c.jpg" />


- **[Actual Investigation II: Are those similar lap speeds associated with genders? (k=2)]**

Comparing the clustering result with the “class” variable in the original dataset, using cross tabulation and Rand Index.  
```
## kmeans() takes the dataset (k=2)##
kmcl.X <- kmeans(X.new, centers = 2, nstart = 100); kmcl.X 
table(kmcl.X$cluster)

##Computing the silhouette for each observation.##
sil.gen <- silhouette(kmcl.X$cluster, dist.lap); plot(sil.gen)

##when k=2, in order to compare the clustering results to the class, we extract the column: "sex"##
colvec.gen <- as.numeric(X[,12])

##tabulating the results##
tab <- table(colvec.gen, kmcl.X$cluster); tab

##Computing the Rand and adjusted Rand indices## 
classAgreement(tab)
```

In this plot below, there is no negative silhouette width and we can say that k=2 means clustering works better (rand index value: 0.5048549). However, it seems that the 'clusters' k=2 means algorithm produced and the 'clusters' defined by sex-column in the actual dataset do not agree with each other. In addition, the two rand index results 0.5782136 (k=3), and 0.5048549 (k=2) are not that different and not that close to '1'(so not super reliable). Can we say the 'race(class)' and 'gender(sex)' do not offer absolute help in predicting speed of athletes?     

<img src="https://user-images.githubusercontent.com/31917400/32546386-d3b04c6a-c476-11e7-80f0-97ff7659b5ed.jpg" />

> **Bootstrap:** When we did k-means clustering, we used the total within sum of squares to choose an appropriate value of k, but It was difficult to decide when there was an “elbow” in the plot. We could use **bootstrapping** to estimate the variability of the **within sum of squares.** We could then use the values to establish what value of k might be most appropriate. In Bootstrapping, we generate the randome samples from the sampling distribution PDF(area=probability) -'f(x)' that comes from the empirical CDF(height=probability) -"F(X)" which is only estimated from our original dataset.  

<img src="https://user-images.githubusercontent.com/31917400/32548095-5fac9912-c47c-11e7-958b-4f9c5782f28a.JPG" />

```
##Count how many observations are in the original dataset##
n = nrow(X.new)

##Decide how many bootstrap samples to take##
BS = 100

##Choose the maximum number of clusters to fit##
kmax = 10

##Set up a matrix to store the results##
result = matrix(n,BS,kmax)

##Loop over the number of bootstrap samples (from 1 to BS) and the number of clusters from 1 to Kmax 'k'##
##Sample the observation indices with replacement##
##Make a copy of the data with each observation occurring as often as its index happens##
##Run k-means for this value of k. Do 4 random starts to avoid local minima##
##Store the total within sum of squares##

for(b in 1:BS) {
  for(k in 1:kmax) {
    index = sample(1:n, replace = T)
    X_2 = X.new[index, ]
    fit <- kmeans(X_2, centers = k, nstart = 4)
    result[b,k] = fit$tot.withinss 
  }
}

##compute summaries from our output to get a feeling for what k to use (summaries for the within SS for each k)## 
apply(result,2,summary)

##Compute the 2.5%, 50% and 97.5% quantiles of the statistic for each k## 
withinss = apply(result, 2, quantile, probs=c(0.025, 0.5, 0.975)); withinss
matplot(t(withinss))
```
<img src="https://user-images.githubusercontent.com/31917400/32550190-153fc842-c484-11e7-8e46-2c4f7d96a61b.jpg" />

> In this bootstrap simulation, the 95% bootstrap intervals for the withinSS start to overlap once k is 12 of larger. This means we have little evidence of needing more than 12 groups to cluster our dataset. 


-------------------------------------------------------------------------------
#### >Lab-03. Regression

<img src="https://user-images.githubusercontent.com/31917400/32504026-b28a518e-c3d6-11e7-93b6-9c8ad96a3d8a.jpg" />

__Data:__ Within the 'MASS' library, there is a dataset called 'birthwt' collected at Baystate Medical Center, Springﬁeld, MA during 1986. UCD data mining class provided this chance to practice logistic regression.  

__Story:__ No story, Just practice logistic regression! 
  - 'low' indicator of birth weight less than 2.5kg 
  - 'age' mother's age in years 
  - 'lwt' mother's weight in pounds at last menstrual period 
  - 'race' mother's race (1= white, 2 = black, 3 = other) /////////////////categorical(multi)
  - 'smoke' smoking status during pregnancy (1=Yes, 0=No) /////////////////categorical
  - 'ptl' number of previous premature labours (0, 1, 2, ...) 
  - 'ht' history of hypertension (1=Yes, 0=No) ////////////////////////////categorical
  - 'ui' presence of uterine irritability (1=Yes, 0=No) ///////////////////categorical
  - 'ftv' number of physician visits during the ???rst trimester (0, 1, 2, ...)
  - 'bwt' baby weight? It's a response variable, so save it for later.


#### *|Basic Logistic Regression|*

<img src="https://user-images.githubusercontent.com/31917400/32555369-6fc669be-c494-11e7-97d6-edc6c417ad7b.jpg" width="600" height="110" />

```
#Ensure that "any categorical variables" are coded as factors##
birthwt$race = as.factor(birthwt$race)
birthwt$smoke = as.factor(birthwt$smoke)
birthwt$ht = as.factor(birthwt$ht)
birthwt$ui = as.factor(birthwt$ui)
fit <- glm(low~age+lwt+race+smoke+ptl+ht+ui+ftv, data=birthwt, family = 'binomial'); summary(fit)

##the fitted value from the model, but it is still the values of "log odd"## 
pred <- predict(fit); head(pred, 10) 
##the fitted value from the model, and the final predicted probabilities by each row##
pred <- predict(fit, type = 'response'); head(pred, 10) 

##Extract the coefficients of the model and Compute the probabilities## 
beta<-coef(fit)
exp(beta)
##Compute a 95% confidence interval for coefficients & probabilities. qt()is a t-statisitics##
summ<-summary(fit)
betaLB<-summ$coef[,1] - qt(0.975,summ$df.residual)*summ$coef[,2] 
betaUB<-summ$coef[,1] + qt(0.975,summ$df.residual)*summ$coef[,2]
##Store coefficients & confidence limits in matrix (BETA)## 
BETA<-cbind(betaLB,beta,betaUB)
##Compute the odds & confidence limits for the odds## 
exp(BETA)
```
<img src="https://user-images.githubusercontent.com/31917400/32556787-44954a68-c498-11e7-8cac-8cdba3d95adc.jpg" width="600" height="560" />

- **[Obtaining Residual]**
```
##Plotting the predicted values versus the deviance residuals##
plot(pred, residuals(fit, type = 'deviance')) 

##Plotting the predicted values versus the pearson residuals##
plot(pred, residuals(fit, type = 'pearson')) 
```
<img src="https://user-images.githubusercontent.com/31917400/32558345-6bc06d58-c49c-11e7-82cf-baf11f456114.jpeg" width="600" height="200" />

- **[Goodness of Fit]**

The deviance goodness of fit test-statistic is already given in the output from the glm() through `summary(fit)`.
```
##Using 'HLtest()' in the 'binomTools' package to complete the Hosmer-Lemeshow goodness-of-fit test##
install.packages("binomTools")
library(binomTools)

##The 'HLtest()'for HL and 'X2GOFtest()'for Pearson needs the logistic regression output to be put into Rsq##
HLtest(Rsq(fit))
X2GOFtest(Rsq(fit))
```
<img src="https://user-images.githubusercontent.com/31917400/32558964-f79391d8-c49d-11e7-94e3-afa31c590718.jpg" width="600" height="280" />

- **[Performance]**
















