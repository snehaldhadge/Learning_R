setwd("~/Documents/GitHub/Learning_R/")
library(psych)
library(ggplot2)
library(corrplot)
wine_data <- read.csv('winequality-red.csv')
head(wine_data)

#Viewing the spread of the data
multi.hist(wine_data)

#correlation plot between columns
cor.plot(wine_data)

#scatter Matrix of variables
plot(wine_data)

corrplot(cor(wine_data))

#count number of samples for each quality
table(wine_data$quality)

# There is not any predictors with very high correlation


#Changing Quality to 2 class
#1--wine quality > 6 (Good)
#0--wine quality<=6 (Bad)
wine_data$review <- ifelse(wine_data$quality>6,1,0)

#wine_data <- wine_data %>% 
#  mutate(review=if_else(quality>6,'Good','Bad')) 

#checking for any NA values
is.na(wine_data)

#count number of samples for each review
table(wine_data$review)
#here data is not balances
#  0    1 
#1382  217 

#Logistic regression to classify model
glm.fit <- glm(wine_data$review~.-quality,data=wine_data,family=binomial)
summary(glm.fit)
#pH,free.sulfur.dioxide,citric.acid is not significant predictor
#volatile.acidity,chlorides,total.sulfur.dioxide,density have negative effect
#Deviance is a measure of goodness of fit of a generalized linear model
#the null deviance and the residual deviance. The null deviance shows how
#well the response variable is predicted by a model that includes only the
#intercept (grand mean).
# The residual deviance shows how well the response is predicted by the model 
# when the predictors are included.
# Fisher’s scoring algorithm is a derivative of Newton’s method for solving 
# maximum likelihood problems numerically.
#The Akaike Information Criterion (AIC) provides a method for assessing the 
#quality of your model through comparison of related models.  It’s based on 
#the Deviance, but penalizes you for making the model more complicated.  
#Much like adjusted R-squared, it’s intent is to prevent you from including
# irrelevant predictors.
resid(glm.fit) 
coef(glm.fit)
# Null deviance: 1269.92  on 1598  degrees of freedom
#Residual deviance:  870.86  on 1587  degrees of freedom
#AIC: 894.86
#AIC::the less information a model loses, the higher the quality of that model.

#Hosmer-Lemeshow Goodness of Fit
library(ResourceSelection)
hoslem.test(wine_data$review, fitted(glm.fit))
#here p-value is greater than 0.26 which is greater than 0.05
#There is no difference between model and the observed data
#

#Lets use only significant predictors
glm.fit <- glm(wine_data$review~.-quality-pH-free.sulfur.dioxide-citric.acid,data=wine_data,family=binomial)
summary(glm.fit)
resid(glm.fit) 
coef(glm.fit)
hoslem.test(wine_data$review, fitted(glm.fit))
#Null deviance: 1269.92  on 1598  degrees of freedom
#Residual deviance:  872.08  on 1590  degrees of freedom
#AIC: 890.08



#LDA
library(MASS)
lda.fit<- lda(wine_data$review~.-quality,data=wine_data,CV=TRUE)
lda.fit
#      0    1
#0   1318   64
#1   135   82
#True poistive rate is (1318+82)/(1318+135+64+82)=  1400/1599 = 87.5
#


lda.fit.values <- predict(lda.fit)
ldahist(lda.fit.values$x[,1],g=wine_data$review)

