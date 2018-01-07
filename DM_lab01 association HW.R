

library(arules)

data(Groceries)

# find all association rules with 'Support' greater than 0.01 and 'Confidence' greater than 0.5.....
# 15 rules were mined with these parameter settings.

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01)) #what if,minlen=1,maxlen=1 ? has NO MEANING!!
fit <- sort(fit,by="lift"); inspect(fit) 
fit <- sort(fit,by='support'); inspect(fit)
fit <- sort(fit,by='confidence'); inspect(fit)

fit<-apriori(Groceries,parameter=list(confidence=0.0,support=0.01, minlen=2,maxlen=2)) #limiting NO.of matching items from 2..
fit<-sort(fit,by="support"); inspect(fit)


####ENHANCING OUTPUT####
#We can add extra statistics to the output. In this case, adding the values of P(A) and P(B). 
#Notice that the output now includes P(A) and P(B) in columns called PA and PB.

qual <- quality(fit); qual #???what? accessing only quality part???? 

PA <- qual$support/qual$confidence 
PB <- qual$confidence/qual$lift

quality(fit) <- data.frame(qual,PA,PB); inspect(fit) 
#qual is a new dataframe..but u add PA,PB and insert the new dataframe into...the 'quality part' of 'fit'?


#Adding the 'frechet' bound' to the output to help with interpretation ('lift' can take...)
Upper_B <- cbind(1/PA, 1/PB) 
Upper_B <- apply(Upper_B,1,min) #the smaller one b/w them === min?

Lower_B <- cbind(0, 1/PA + 1/PB - 1/(PA*PB)) 
Lower_B <- apply(Lower_B,1,max) #the bigger one b/w them === max?

quality(fit) <- data.frame(qual,Lower_B,Upper_B); inspect(fit) 


#We can add the improved bounds and standardized lift to the output
supplb <- 0.01 
conflb <- 0.5 
Lft_B <- cbind(0, 1/PA + 1/PB - 1/(PA*PB), supplb/(PA*PB), conflb/PB) 
Lft_B <- apply(Lft_B,1,max) 
sLift <- (qual$lift-Lower_B)/(Upper_B-Lower_B)
quality(fit) <- data.frame(qual,Lower_B,Upper_B,sLift)
inspect(fit)

#Although greater than one, some rules have lift equal to the lower bound of the range of possible values.....??????

#We have seen that the range of values that lift takes can depend on the minimum support (s) and confidence (c) thresholds used..
#Whilst it is possible to standardize the lift values using the bounds, the values of standardized lift vary with the 
#choice of thresholds used.



# extra practice ################################################################################################################
#A??Items with support greater than threshold
fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=1,maxlen=1)) # have meaning???
fit <- sort(fit,by="support")
inspect(fit)

#A??Pairs of items with support greater than threshold
fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)
#A??Pairs of items with support greater than threshold (confidence threshold too)
fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# Triples of items with support greater than threshold
fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)
# Triples of items with support greater than threshold (confidence threshold too)
fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# Quadruples of items with support greater than threshold
fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=4,maxlen=4))
fit <- sort(fit,by="support")
inspect(fit)









#################################################################################################################################
#Data Mining Code (Association Rules)

#[practice A]------------------------------------------------------------------------------------------------------------------

# This set of notes shows how we can use association rule mining to analyze voting data. 
#There are 435 members of congress and they are either Democrat or Republican.
#We can investigate if there are structures in the voting behaviour of the congress members
#A Further details on the data are available here: http://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records

#A??Turn on the arules package
library(arules)

#A??Read in data from the web
dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.
colnames(dat) <- c("Party", paste("Vote",1:16,sep="")) #col.1,and col.2~16 paste paste paste...sep="" 

#A??Look at the data
dat

#A??Note that the ?'s are people being absent from the vote. We recode the ?'s as n's
dat[dat=="?"] <- "n"

#A??I will remove "party" from the data so that we only analyze votes.
dat<-dat[,-1]


# Let's make a binary version of the data (1="y" and 0="n")
datnew <- 1*(dat=="y") # this turns it into matrix.......i don't know why...
colnames(datnew) <- paste("Yes",1:16,sep=""); datnew

#A??Getting the data into the transaction format that arules uses. 
votes <- as(datnew,"transactions")
inspect(votes)

# We mine all assocation rules with support greater than 0.4 and confidence greater than 0.8

fit<-apriori(votes,parameter=list(support=0.4)) #try to set the limit with respect to confidence!!!
fit<-sort(fit,by="support")
inspect(fit)

# Note that we are treating the votes in an asymmetric manner. 
# It could be argued that treating "y" as the item of interest is arbitrary.
# We could instead move the focus to the "n"s. 

datnew <- 1*(dat=="n")#??????????????????????????????????????INVERSION?????????????????????????????????????????????????
colnames(datnew)<-paste("No",1:16,sep="")

#A??Getting the data into the format that arules uses.

novotes <- as(datnew,"transactions")
inspect(novotes)

fit<-apriori(novotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# It could be further argued that treating "n" or "y" is still arbitrary.
# We could construct the dataset with both "y" and "n" votes recorded. 

allvotes<-merge(votes,novotes)#???????????????????????????ADDING INVERSION merge b/w transactions???????????????????????
inspect(allvotes)

fit<-apriori(allvotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

#A??We get many more rules so we may wish to consider changing the parameter settings. 
# In particular, we should increase the support threshold. 

fit<-apriori(allvotes,parameter=list(support=0.45,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)











# [Practice B] -----------------------------------------------------------------------------------------------------------------

##NLTCS_01######################################################################################################################
#Read In Data from National Long Term Care Survey
dat<-read.table("http://mathsci.ucd.ie/~brendan/Notes/NLTCS.txt",header=TRUE)

# Look at Data (note the format)....it already is a binary dataset.
head(dat)


#,we EXPANDING OBSERVATIONS!!!!,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
# Let's make a dataset where each pattern occurs in a row (as often as it should)
patt<-dat[,1:16] # 16 columns. DELICIOUS PART!!! 
count<-dat[,17] # a single column: 'count.' RECYCLING!!!!

#Construct a 'vector.' There are the numbers from 1 to 3152. we EXPANDING OBSERVATIONS!!!!
values<-rep(1:nrow(patt),count) 
# we reproduce each number-1,2,3,4,5,6....3152 by the values in the 'count'..'count' has 3152 rows as well..

#creating new dataset with rows coming from 'values'(subsetting) but the original data would be 'patt'.
rawdat<-patt[values,] # ULTIMATE BASE DATASET!!!the delicious part turn into... 
dim(rawdat)
#we made this because of the asshole 'count'!!!!!!
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,



#Load the arules library
library(arules)

# Put the data into transactions format
transdat<-as(rawdat,"matrix")
transdat<-as(transdat,"transactions"); inspect(transdat)

rules<-apriori(transdat,parameter=list(support=0.3,minlen=2))
rules<-sort(rules,by=c("lift")) # why by=c('lift')???????
inspect(rules)

# Reversing the question by interchanging 1 and 0. ???????????????????????ADDING INVERSION?????????????????????????????
rawdat<-cbind(rawdat,1-rawdat)
colnames(rawdat)[17:32]<-paste("N",16:1,sep="")
head(rawdat)
transdat<-as(rawdat,"matrix")
transdat<-as(transdat,"transactions"); inspect(transdat)

rules<-apriori(transdat,parameter=list(support=0.7, minlen=2))
inspect(rules)





##NLTCS_02#######################################################################################################################
dat<-read.table('C:/Users/Minkun/Dropbox/Minkun/Semester 2/30270_Data Mining/LAB/NLTCS.txt', head=T) #note!!! head=T 

head(dat)
tail(dat)
#sum of COUNT(column) is the total no. of elderly people!!! 'dat' dataset is already sorted!
#Forexample,wecanseethat 3853people have no disabilties, 4 are disabled at eating only(Y1),etc. and 660 people are disabled 
#at everything. This way of storing the data is efficient because there are 2^16=65536 possible 
#patterns of disabilties whereas only 3152 of these happen in the data

sum(dat[, 17])
sum(dat$COUNT)

#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
#get the data into 'transaction' format so that we can apply association rule analysis.
# 1> construct a matrix (where each row corresponds to a person and their disabilties. This matrix will have (21574,16) 
# 2> turn this matrix into transaction format for processing by arules.
M <- nrow(dat)
indices <- 1:M
counts <- dat$COUNT; counts #extracting the column 'COUNT' from the dataset.
##Construct a 'vector' where the row number of each pattern is recorded the number of times that the pattern arises.
rowindices <- rep(indices, counts); rowindices # index...! how many? as many as 'counts' 

##creating a matrix and drop the columns we dont need and reorder the columns and coerce this dataframe into a matrix! 
nltcsframe <- dat[rowindices, ]
nltcsframe <- dat[, -c(17,18)]
nltcsframe <- dat[, 16:1]
nltcsmatrix <- as.matrix(nltcsframe)
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

## turn the data into the transaction format for the arules command...
nltcs <- as(nltcsmatrix, 'transactions'); inspect(nltcs)

##JUST check the data....joint table!! very useful!!!!
table(nltcsmatrix[, 7], nltcsmatrix[, 8]) #very useful cross tabulation!!!!!
table(nltcsmatrix[,7]) #show the no. of people suffering from Y7 and being free from Y7
#Get a table of the outcome for each disability. useful 'apply()' to get row or column statistics!
apply(nltcsmatrix, 2, table) #(matrix, margin, function)? margin: 1 (row) / 2 (column); func: compute what? 

#JUST check the distribution....
##Let's look at the distribution of the number of disabilities per person. First, store the number of disabilties per person 
#and then compute some summaries
disabilitycount <- apply(nltcsmatrix, 1, sum)
hist(disabilitycount)
summary(disabilitycount)






















#--homework---------------------------------------------------------------------------------------------------------------------




#START--------------------------------------------------------------------------------------------------------------------------
#Data were collected recording votes in the Irish parliament (D´ailEireann), prior to the general election, in early 2016.
#Extra details of the votes can be found at http://www.oireachtas.ie/parliament/ 
#and the data are for the votes on January 14th-28th.

library(arules)

#get ready
load("C:/Users/Minkun/Desktop/r_practice/+++++++++++School R/LAB_data mining/data/dailvotes.Rdata")
class(votes)
head(votes)
dim(votes)

#YES#
#get the data into an appropriate binary format (1's and 0's) before analysis.
#votenew <- 1*(votes=='y'); votenew#

#HINT#
#the matrix is not a numeric binary matrix. Once you have converted your data into 1 and 0 if you do a summary(data) you 
#probably see that the matrix is reading the columns in as factor variables and is not a numeric matrix.
#To convert the matrix as a numeric matrix, use the command 'as.numeric' on each column in the dataset. 
#So for example I have converted the columns of votes into 1 and 0 here.
for(i in 1:ncol(votes)){
  votes[which(votes[,i]=="n"),i]<-0
  votes[which(votes[,i]=="a"),i]<-0
  votes[which(votes[,i]=="y"),i]<-1 
}
#convert it to a binary numeric matrix
#vote_Y<-apply(votes,2,function(x)as.numeric(x))
vote_Y <- 1*(votes=='y'); vote_Y #try..

colnames(vote_Y) <- paste('Y', 1:23, sep=''); vote_Y
transvote_Y <- as(vote_Y, 'transactions'); inspect(transvote_Y)

#We get many more rules so we may wish to consider changing the parameter settings. 
# We mine all assocation rules with support greater than 0.3 and confidence greater than?
fit<-apriori(transvote_Y, parameter=list(support=0.3, minlen=1, maxlen=10)) #maxlen means total max no. of ingredients!!!
fit<-sort(fit,by="support")
inspect(fit)


#------------------------------------------------------------------------------------------------------xxxxx
#NO#
#No2# w/o abstentious

for(i in 1:ncol(votes)){
  votes[which(votes[,i]=="n"),i]<-1
  votes[which(votes[,i]=="a"),i]<-1
  votes[which(votes[,i]=="y"),i]<-0 
}

#convert it to a binary numeric matrix
#vote_N <-apply(votes,2,function(x)as.numeric(x)); vote_N
vote_N <- 1*(votes=='n'); vote_N #try!!

colnames(vote_N) <- paste('N', 1:23, sep=''); vote_N
transvote_N <- as(vote_N, 'transactions'); inspect(transvote_N)

# We mine all assocation rules with support greater than 0.2 and confidence greater than 0.7
fit<-apriori(transvote_N, parameter=list(support=0.3, minlen=3)) #maxlen means total max no. of ingredients!!!
fit<-sort(fit,by="support")
inspect(fit)

#--------------------------------------------------------------------------------------------------failure......why???
#Abstentious#
#vote_Ab <- 1*(votes=='a'); vote_Ab
#colnames(vote_Ab) <- paste('oth', 1:23, sep=''); vote_Ab
#transvote_Ab <- as(vote_Ab, 'transactions'); inspect(transvote_Ab)
#---------------------------------------------------------------------------------------------------------------------------
#final
all<-merge(transvote_Y,transvote_N); inspect(all)
fit<-apriori(all, parameter=list(support=0.8, confidence=0.8, minlen=2, maxlen=4))
fit<-sort(fit,by="support")
inspect(fit)





