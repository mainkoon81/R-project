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
  - package: 
  - func:

----------------------------------------------------------------------
#### >Lab-01. Association Rule Mining

<img src="https://user-images.githubusercontent.com/31917400/32491154-907d38fa-c3ad-11e7-95a2-7a5ce5588d81.jpg" />

__Data:__ The Data were collected recording votes in the Irish parliament (D´ailEireann), prior to the general election, in early 2016. Extra details of the votes can be found at (http://www.oireachtas.ie/parliament/) and the data are for the votes on January 14th-28th.

__Story:__ Association rule mining has a purpose to find frequent co-occurring relationships among a collections of independent items in the dataset so that we can predict the occurrance of next items. In this project, we aim to discover and investigate certain patterns or trends that might reside in their **voting behaviour** on diverse (23) propositions. Each column numbers in the dataset refers each different proposition put to those votes. And we tag each ‘YES-votes’, ‘NO-votes’, using those column numbers that reflect what propositions those voting choices correspond to. 

What we focus on, in the beginning, is to find association pattern that reveals interesting connections between ‘Yes-votes.’ This analysis is repeated between ‘No-votes’ as well. **Given that the association patterns we are looking for are not subject to the causal and effect attributes in the dataset, it is not a bad idea to rely more on seeking co-occurrence relationships** between items. Likewise, when it comes to the choice of quality measures as a threshold for rule mining, **Support measure** over Confidence or Lift seems to be the most convincing as Support, by definition, embodies the intersecting (co-occurrence) relationship between items. However, Lift measure also can be useful to verify reliability of the association rules that are generated by Support measure. For example, if a Lift-value is greater than 1, it represents a positive effect on the occurrence of one another. If a Lift-value is equal to 1, it means the occurrences of the item are independent each other. If a Lift-value is less than 1, it means the proposed occurrence has a probability less than 1.  


#### *|Data Wrangling + Mining|*

<img src="https://user-images.githubusercontent.com/31917400/32518404-5a73d148-c401-11e7-909f-0817c2f1cfde.jpg" />

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
<img src="https://user-images.githubusercontent.com/31917400/32521319-f7470400-c40a-11e7-9b57-46d36f3a3284.jpg" />
<img src="https://user-images.githubusercontent.com/31917400/32520233-2cf894be-c407-11e7-8a6e-20271aa4069d.jpg" />

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
<img src="https://user-images.githubusercontent.com/31917400/32521327-fad294ae-c40a-11e7-8e3a-7e296df5ac13.jpg" />
<img src="https://user-images.githubusercontent.com/31917400/32520365-a3d9d6b0-c407-11e7-9934-0a590c4aa83a.jpg" />

- __Interpretation:__ There are several rules that attract our attention. ‘Support’ threshold greater than 0.3 reveals two different groups of associations rules between No-votes. One is for the motion 18, 19, 20 and the other is for the motion 10, 11, 12, 13, 14, but the first one manifests slightly stronger correlation. The motion 18, 19, 20 are all about criminal justice and the motion 10, 11, 12, 13, 14 address consistently some issues arising from the dissolution of certain educational institution. We can say that the Irish politicians shows certain consistency in voting behaviour for the motions in similar threads. In sum, this association rule analysis can be seen successful. This is because its result and those suggested association rules effectively picked up a series of the political motions possibly in similar threads and they are clearly in concordance with actual data recorded in the website. Therefore, it is safe to say that those association rules yielded by a priori algorithm turn out to be a reliable source of reference to predict the pattern of behaviour of Irish politicians.    
-------------------------------------------------------------------------------
#### >Lab-02. Clustering

<img src="https://user-images.githubusercontent.com/31917400/32509308-d95aa8be-c3e4-11e7-9be4-02d14275584a.jpg" />
<img src="https://user-images.githubusercontent.com/31917400/32509312-e00f8c2e-c3e4-11e7-889a-d7dd939d8200.jpg" />

__Data:__ The International Association of Ultrarunners (IAU) world championship, World Masters Athletics (WMA) world championship and an open race (OP) over 100 km were held simultaneously in Los Alcazares, Spain on 27th November 2016. The course of the race consisted of 10×10km lap. The lap speed (in metres per second) of each competitor was recorded at the end of each of the 10 laps. The race entered (IAU, WMA and OP) and the gender of the athlete (M and F) are also recorded. Extra details of the votes can be found at (http://100kworldchampionship2016.com/)  

__Story:__ We want to analyze the data to establish if there are clusters of athletes with similar lap speeds throughout the race, and investigate if the clusters correspond to the type of race entered or the gender of the athlete. 

#### *|K-means Clustering|*

<img src="https://user-images.githubusercontent.com/31917400/32523548-adc68248-c413-11e7-88f0-861e2bad997c.jpg" />

- __issue:__ We can see that there are two levels - male, female - in the “Sex” variable and three levels - IAU, OP, WMA - in the “Class” variable. We’d like to see that if our result of clustering analysis would correspond to those levels. But how many “k” do we need? K=2 or K=3 ? We want to make our clusters as tight as possible in order to improve the accuracy of our clustering analysis and this goal seems to be achievable by minimizing Sum of Squares. Interestingly, the value of Sum of Squares diminishes as we have more clusters which is represented by the value of “k.” Therefore, what we need is the plot of “k”values against the Sum of Squares so that we can find the elbow of the curve that manifesting the most desirable ‘”k” value. 

- **[Searching the best 'K']**
```
X.new = X[, 1:10]
plot(X.new) #extracting the numerical variables of interest#


































-------------------------------------------------------------------------------
#### >Lab-03. Regression

<img src="https://user-images.githubusercontent.com/31917400/32504026-b28a518e-c3d6-11e7-93b6-9c8ad96a3d8a.jpg" />

__Data:__ 




