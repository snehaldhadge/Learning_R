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

dev.off()
#count number of samples for each quality
table(wine_data$quality)

#Distribution of Wine Data
ggplot(wine_data,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

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
#There are more bad reviews than Good Reviews

#Plotting this ditribution
ggplot(wine_data,aes(x=review,fill=factor(review)))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(0,1,1))+
  ggtitle("Distribution of Good/Bad Red Wines")+
  theme_classic()

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


lda.fit.values <- predict(lda.fit,wine_data)
ldahist(lda.fit.values$x[,1],g=wine_data$review)


#Using RandomForest
#Baseline Random Forest Model
library(randomForest)
RF_wine<-randomForest(factor(review)~.-quality,wine_data,ntree=150)
RF_wine
#The overall accuracy for the model 91.6% 

importance    <- importance(RF_wine)
importance

#Lets divide the data into Train and Test Set
library(caret)
set.seed(1)

wine_data$review <- as.factor(wine_data$review)

inTrain <- createDataPartition(wine_data$review, p=.9, list = F)

train <- wine_data[inTrain,]

valid <- wine_data[-inTrain,]
#Random Forest on the train data
RF_wine<-randomForest(review~.-quality,train,ntree=150)
#Use Validation data to test
newdata = valid[,!colnames(valid) %in% c("review")]
rf_result <- predict(RF_wine, newdata)
#Lets compare the results
confusionMatrix(rf_result, valid$review)
#Accuracy is 94%


#Using XGBoost
#

# xgboost
library(xgboost)
data.train <- xgb.DMatrix(data = data.matrix(train[, !colnames(valid) %in% c("review","quality")]), label = train$review)

data.valid <- xgb.DMatrix(data = data.matrix(valid[, !colnames(valid) %in% c("quality","review")]))

parameters <- list(
  
  # General Parameters
  
  booster            = "gbtree",      
  
  silent             = 0,           
  
  # Booster Parameters
  
  eta                = 0.08,              
  
  gamma              = 0.7,                 
  
  max_depth          = 8,                
  
  min_child_weight   = 2,            
  
  subsample          = .9,                 
  
  colsample_bytree   = .5,                
  
  colsample_bylevel  = 1,          
  
  lambda             = 1,    
  
  alpha              = 0,       
  
  # Task Parameters
  
  objective          = "multi:softmax",   # default = "reg:linear"
  
  eval_metric        = "merror",
  
  num_class          = 7,
  
  seed               = 1               # reproducability seed
  
  , tree_method = "hist"
  
  , grow_policy = "lossguide"
  
)



xgb_model <- xgb.train(parameters, data.train, nrounds = 100)

xgb_pred <- predict(xgb_model, data.valid)



confusionMatrix(as.factor(xgb_pred-1), valid$review)

#here accuracy is almost 95%


###SVM
###
# svm
library(e1071)
set.seed(1)

svm_model <- svm(review~.-quality,train)

svm_result <- predict(svm_model, newdata = valid[,!colnames(valid) %in% c("review")])



confusionMatrix(svm_result, valid$review)
#Here accuracy is 91% which is less than bgboost and RF
#
#
