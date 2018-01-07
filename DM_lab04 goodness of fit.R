
#lab04 - Goodness of fit

#data: birth weight of new born babies

#The glm() function is probably the most reliable function for 0²3tting a logistic regression module but the multinom() function
#(within the nnet package) is also useful. The multinom() function can also be used to 0²3t a multinomial logistic regression model. 

#'low' indicator of birth weight less than 2.5kg 
#'age' mother's age in years 
#'lwt' mother's weight in pounds at last menstrual period 
#'race' mother's race (1= white, 2 = black, 3 = other) /////////////////c
#'smoke' smoking status during pregnancy (1=Yes, 0=No) /////////////////c
#'ptl' number of previous premature labours (0, 1, 2, ...) 
#'ht' history of hypertension (1=Yes, 0=No) ////////////////////////////c
#'ui' presence of uterine irritability (1=Yes, 0=No) ///////////////////c
#'ftv' number of physician visits during the ???rst trimester (0, 1, 2, ...)
#'bwt' baby weight? so save it for later..

library(MASS)
data("birthwt")
names(birthwt)
head(birthwt)

#Ensure that "any categorical variables" are coded as factors!! what about bwt?
birthwt$race = as.factor(birthwt$race)
birthwt$smoke = as.factor(birthwt$smoke)
birthwt$ht = as.factor(birthwt$ht)
birthwt$ui = as.factor(birthwt$ui)

fit <- glm(low~age+lwt+race+smoke+ptl+ht+ui+ftv, data=birthwt, family = 'binomial'); summary(fit) #????????family=???
#family= tells R that this model has a logistic regression problem...glm...manipulating response variable...

#A generalization of linear regression allowing for nonlinear relationships via a "link function" and for the variance
#of the response to depend on the predicted value. (Not to be confused with "general linear model" which extends the ordinary
#linear model to general covariance structure and multivariate response.)

#You may notice the following section of the logistic regresion output. 
Null deviance: 234.67 on 188 degrees of freedom #(n-1)
Residual deviance: 201.28 on 179 degrees of freedom #(n-p)
AIC: 221.28 
#Null.D vs Residual.D 
#The null deviance shows "how well the response is predicted" by the model with nothing but an intercept.
#The residual deviance shows "how well the response is predicted" by the model when the predictors are included. 

##From your example, it can be seen that the deviance goes up by 3443.3 when 22 predictor variables are added 
#(note: degrees of freedom = no. of observations ¨C no. of predictors) . 
#This increase in deviance is evidence of a significant lack of fit. i.e. the model is getting rubbish..

#These are the "deviance values" for a model with no predictor variables and all of the predictor variables. 
#If we compare the deviance values, we ???nd a di???erence of 234.67-201.28=33.39. 
#The models have a di???erence of 188 df vs 179 df= 9 parameters.(n-0) - (n-9) = 9 
#We can look up a "??2 9" table to test if the full model is better than the model with no predictors. 
#We ???nd that P{??2 9 > 33.39} = 0.0001, so the full model is better.


#Let LL = log(likelihood)

#Null Deviance = 2(LL(Saturated Model) - LL(Null Model)) on df = df_Sat - df_Null

#Residual Deviance = 2(LL(Saturated Model) - LL(Proposed Model)) df = df_Sat - df_Proposed

#------------------------------------------------------------------------------------------------------------------------------
#The goal of modeling in general can be seen as using the smallest number of parameters to explain the most about your response.
#To figure out how many parameters to use you need to look at the benefit of adding one more parameter. If an extra parameter 
#explains a lot (produces high deviance) from your smaller model, then you need the extra parameter.
#The theory tell us that the deviance is chi squared with degrees of freedom equal to the "difference" of parameters between your 
#two models.

#The "Saturated Model" is a model that assumes each data point has its own parameters (which means you have n parameters 
#to estimate.)
#The "Null Model" assumes the exact "opposite", in that is assumes one parameter for all of the data points, which means you 
#only estimate 1 parameter.
#The "Proposed Model" assumes you can explain your data points with p parameters + an intercept term, so you have p+1 parameters.

#If your Null Deviance is really "SMALL", it means that the Null Model explains the data pretty well. Likewise with your Residual 
#Deviance. What does really small mean? If your model is "good" then your Deviance is approx Chi^2 with (df_sat - df_model) 
#degrees of freedom.

#If you want to compare your Null model with your Proposed model, then you can look at 
# (Null Deviance - Residual Deviance) approx Chi^2 with (df-Proposed) - (df-Null) = (n-(p+1))-(n-1)=p

# you should see that the degrees of freedom reported on the Null are always higher than the degrees of freedom reported 
#on the Residual. That is because again, 
#Null Deviance df = Saturated df - Null df = n-1 Residual Deviance df = Saturated df - Proposed df = n-(p+1)





#We can also use the residual deviance to test whether the null hypothesis is true (i.e. Logistic regression model 
#provides an adequate fit for the data). This is possible because the deviance is given by the chi-squared value at 
#a certain degrees of freedom. In order to test for significance, we can find out associated p-values using the below 
#formula in R: p-value = 1 - pchisq(deviance, degrees of freedom)
#Using the above values of residual deviance and DF, you get a p-value of approximately zero showing that there is a 
#significant lack of evidence to support the null hypothesis.
>1 - pchisq(4589.4, 1099)
[1] 0



#-------------------------------------------------------------------------------------------------------------------------------
#Look at predicted probabilities ?
pred <- predict(fit); head(pred, 10) #the fitted value from the model, but it is the values inside the EXP(....) term. ie "log odd." 
pred <- predict(fit, type = 'response'); head(pred, 10) #the fitted value from the model, and the final probability ie the 
#type of prediction is 'response'

#intervals? We can look at the odds for each term as well the raw coe???cients...
#Extract the coefficients of the model 
beta<-coef(fit)
# Compute the odds 
exp(beta)
# Compute a 95% confidence interval for beta & odds #Extract the coefficients & standard errors. 
summ<-summary(fit)
# Compute confidence limits for beta 
#qt()is a t-statisitics..
betaLB<-summ$coef[,1] - qt(0.975,summ$df.residual)*summ$coef[,2] 
betaUB<-summ$coef[,1] + qt(0.975,summ$df.residual)*summ$coef[,2]

#Store coefficients & confidence limits in matrix (BETA) 
BETA<-cbind(betaLB,beta,betaUB)
#Compute odds & confidence limits for odds 
exp(BETA)



#Obtaining the residuals from a logistic regression is straightforward.
#Plot the predicted values versus the deviance residuals.
plot(pred, residuals(fit, type = 'deviance')) #what does it mean? 

#Plot the predicted values versus the pearson residuals.
plot(pred, residuals(fit, type = 'pearson')) #what does it mean? find outliers..



# GOODNESS OF FIT ###############################################################################################################
#We will use the HLtest() command in the # binomTools # package to complete the Hosmer-Lemeshow goodness-of-ﬁt test.
install.packages("binomTools")
library(binomTools)

##The HLtest() command needs the logistic regression output to be put into Rsq...
HLtest(Rsq(fit))

#The Pearson test can be implemented using the X2GOFtest command..

##The X2GOFtest() command needs the logistic regression output to be put into Rsq...
X2GOFtest(Rsq(fit))

#The deviance goodness of ﬁt test-statistic is given in the output from the glm() command. 
summary(fit) # ﬁnd the ”Residual Deviance” value.


# Performance ###################################################################################################################

#There is a very popular package called # ROCR # that can be used to compute many of the performance measures
install.packages("ROCR")
library(ROCR)

##Take the predicted probabilities from earlier and the observed response patterns and pass them 
#through the prediction() command..
predobj = prediction(pred, birthwt$low)

# Complete the TPR and FPR for the model (as the treshold varies)...
# Many other performance measures can be computed using the performance() command.
?performance
perf = performance(predobj, 'tpr', 'fpr')

#this is the ROC curve..
plot(perf)



