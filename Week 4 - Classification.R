##Classification

setwd("/Users/snehalilawe/Desktop/Spring 2020/MIS-749-BA/Week2 R-Intro")

#will use book's package
library(ISLR)
library(ggplot2)
library(ggthemes)
library(gridExtra)


#Default datasets
#make ISLR package is loaded so dataset is available
str(Default)

#load dataframe to environment from ISLR Package
data(Default)

##ggplot cheat sheet
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf 

#using quickplot feature of ggplot2, ggplot allow for deep customization, 
#but qplot will build most common plots
qplot(x=balance, y=income, color=default, pch=default, data=Default)
#from above non default daya way higher than ND so imbalance
table(Default$default)

#balance
bal.plot <- qplot(x=default, y=balance, geom="boxplot",color=default, data=Default)
#Income
inc.plot <- qplot(x=default, y=income, geom="boxplot",color=default,data=Default)

#in above plot added in bal.plot and inc.plot and now display side by side
#produce plot with two columns
grid.arrange(bal.plot, inc.plot, ncol=2)


#return to classification
#what happen if we try and predict default
#lets make a dummy variable out of default
#as.numeric converts lavels to numeric representation (No=1, Yes=2)
#we subtract 1 to make default yes =1 and no =0
def.dummy <- as.numeric(Default$default)-1

def.lm <- lm(def.dummy ~ balance,data=Default)
summary(def.lm)
#here intercept is -ve
#lets look at the predictions
hist(def.lm$fitted.values)
#here model predicted few values less than 0 and values more than 1


#using default plots add regression line
plot(def.dummy ~ Default$balance)
abline(def.lm)

##lets fit a simple logistic regression
##don't need to recode the factor
### here we dont need to convert default to numberic logistic knows its factor and does that
def.loglm <- glm(default ~ balance, data=Default, family=binomial) 
summary(def.loglm) #model summary
coef(def.loglm) #coefficents
exp(coef(def.loglm))

#plot the logistic regression curve
plot(def.dummy ~ Default$balance)
#predict using logistic model,type=resp giv raw prob value, add=True--draw on existing plot and color blue
curve(predict(def.loglm,data.frame(balance=x),type="resp"),add=TRUE, col="blue")


#lets calculate manually probability of default given balance of 1000
exp(-10.6513+0.0055*1000) / (1 + exp(-10.6513+0.0055*1000))


#estimate the probabilty of default with 1000 or 2000 dollars balance
newdata <- data.frame(balance=c(1000,2000))
#type=response gives output as probability
predict(def.loglm, newdata, type="response") #get probabilities

#just student dummy variables
def.loglm2 <- glm(default ~ student, data=Default, family=binomial) 
summary(def.loglm2) #model summary
coef(def.loglm2) #coefficents

#predict probability on student or not student
newdata <- data.frame(student=c("Yes","No"))
predict(def.loglm2, newdata, type="response") #get probabilities


#fit multivariate logistic regression
def.loglm3 <- glm(default ~ balance + income + student, data=Default, family=binomial) 

summary(def.loglm3)
round(coef(def.loglm3), 6)



#linear discriminant analysis
library(MASS)

def.lda <- lda(default ~ balance + income + student, data=Default )
def.lda
plot(def.lda)

predict(def.lda,Default,type="Response")

#first 10 rows
predict(def.lda,Default[1:10,],type="Response")
#here for first 10 probab is No which is  similiar to actual data


