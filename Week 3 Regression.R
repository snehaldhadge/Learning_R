## Linear Regression

setwd("/Users/snehalilawe/Desktop/Spring 2020/MIS-749-BA/Week3/")

ad <- read.csv("Advertising.csv")

#review data structure
str(ad)
names(ad)
head(ad)

#remove extra number column
ad$X <- NULL

names(ad)

#look at relationship of data with a more advanced version of scatterplot matrix
#diagnol contains density of distribution with rug, regression line (green) and lowes line (red)
library(car)
scatterplotMatrix(ad)
pairs(ad)


#simple linear regression of sales on TV
#formula notation DV ~ IV, 
tv.lm <- lm(Sales ~ TV, data=ad)


#lets visualize fitted line
plot(ad$Sales ~ ad$TV)
abline(tv.lm, col="red")
#abline adds the line


#review model fit, 
summary(tv.lm)

#What is SE of slope?
##confidence interval
confint(tv.lm) #defaults 95%, X-2*SE to X +2*SE*

#extract values from model summary
#calculate t value, simple example extracting these values can be useful
coef(summary(tv.lm))
coef(summary(tv.lm))[2] #estimate of slope
coef(summary(tv.lm))[2,2] #second coefficent std error
coef(summary(tv.lm))[2,4] #extract p value of TV


#to automatical create charts and reports
coef(summary(tv.lm))[2] / coef(summary(tv.lm))[2,2]

#hypothesis test H0 rejected for TV

#residual standard error
summary(tv.lm)

#review sum of squares break down
anova(tv.lm)

#calculate R^2 by dividing fraction of variance explained by TV model
3314.6 / (3314.6 + 2102.5)



#fit model
sales.lm <- lm(Sales ~ TV + Radio, data=ad)
summary(sales.lm)


#multiple linear regression
#check correlation of predictiors
cor(ad) #newspaper and radio moderate correlate
round(cor(ad),2) #lets round to 2 digits
#corr higher than 0.7 is problematic as it influences

#visualize correlations
install.packages("corrgram")
library(corrgram)
corrgram(ad) #basic

#with additional details
corrgram(ad, lower.panel=panel.shade,
         upper.panel=panel.pie)

#is at least one predictor useful?
summary(sales.lm)
#Significant F-stat, sig improvement from null model

#selecting important variables
#will cover later, but can look initially for correlated to Y
#all correlation to residuals (useds in stepwise regression)
#cbind combines column with dataframe, added residual from tv linear model
cor(cbind(ad,tv.lm$residuals))
corrgram(cbind(ad,tv.lm$residuals))

#qualitiative data
credit <- read.csv("Credit.csv")
str(credit)
credit$X <- NULL
pairs(credit)

#factors converted to dummy variables
gender.lm <- lm(Income ~ Gender, data=credit)

#notice male is baseline and female =1
summary(gender.lm) 

#more than two levels
#check how dummy values will be coded for comparison
contrasts(credit$Ethnicity)
ethnicity.lm <- lm(Income ~ Ethnicity, data=credit)
summary(ethnicity.lm) #baseline is African American, and coefficients are difference from this

#interactions effects
install.packages("scatterplot3d")
library(scatterplot3d)

#basic 3d scatter plot of 3 variables
scatterplot3d(ad$Sales, ad$TV, ad$Radio)


#fit model for sales - interaction
salesint.lm <- lm(Sales ~ TV + Radio + TV*Radio, data=ad)
summary(salesint.lm)

#fit interaction on factor interaction
summary(lm(Income ~ Balance + Student, data=credit)) #no interaction common slope different 
summary(lm(Income ~ Balance * Student, data=credit)) #different slope for student/not student, different

#non-linear
#auto data
#this dataset uses ? for empty values for some reason, notice na.strings
auto <- read.csv("Auto.csv", na.strings="?")
str(auto)

#plot
plot(auto$mpg ~ auto$horsepower)

#basic regression
auto.lm <- lm(mpg ~ horsepower, data=auto)
summary(auto.lm)
abline(auto.lm, col="blue") #write basic regression line

#fit quadratic versioni with horsepower squared
#need to uses I function to interpet power symbol correctly
auto.lm2 <- lm(mpg ~ horsepower + I(horsepower^2), data=auto)
summary(auto.lm2)
anova(auto.lm, auto.lm2) #compare models, significant improvement of model 2


#Let's try a practical example, predicting TESLA Stock price
##BONUS CONTENT#################################
library(quantmod)

#let's download the last year of closing pricess for tsla
t <- getSymbols("TSLA", from = "2018-2-5", env=NULL) 
chartSeries(t)
#let's create a
# dependant variable of tomorrow's clossing price by lagging the data one day
t$Close.Tomorrow <- lag(t$TSLA.Close, -1)

#by default t is a time series object, lets convert to data frame
t.df <- data.frame(t)
t.df$Date <- index(t) #get date from rowname/index field and make a column



head(t.df)
str(t.df)

#let's just take a look at TSLA prices
#lets plot this stock
plot(y=t.df$TSLA.Close, x=t.df$Date, type='l', ylab="Closing Price")

#let's calculate some technical indicators
##higher value price might inc
t.df$RSI10<- RSI(t.df$TSLA.Close, 10)  #relative strength index 10 day

#averages
t.df$EMA10 <- EMA(t.df$TSLA.Close, 10)  #10 day moving average
t.df$EMA200 <- EMA(t.df$TSLA.Close, 200)
t.df$EMA50 <- EMA(t.df$TSLA.Close, 50)
t.df$VolEMA50 <- EMA(t.df$TSLA.Volume, 50) #50 day average volume

#range of high and low
t.df$TSLA.range <- t.df$TSLA.High - t.df$TSLA.Low

head(t.df)
#has sme NA data so remove those
#we over sampled above so we can calculate 200 day moving averages will trim to just last year
t.df <- t.df[503-253:503,]  #trime to last 250 trading days and lets analyze


t.lm <- lm(Close.Tomorrow ~ TSLA.Close + TSLA.range + TSLA.Volume +
             RSI10 + EMA10 + EMA50 + EMA200 + VolEMA50,data=t.df)

summary(t.lm)
tail(t.df)
#we'll explore predicting with a model like this later in the semester



