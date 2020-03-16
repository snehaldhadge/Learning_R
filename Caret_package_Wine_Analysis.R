library(caret)
data(iris)
library(AppliedPredictiveModeling)
transparentTheme(trans=.4)
dev.off()

#ScatterPlot
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

#ScatterPlot with ellipse
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

wine_data <- read.csv('winequality-red.csv')
wine_data$review <- ifelse(wine_data$quality>6,'Good','Bad')

train <- createDataPartition(wine_data$review,p=.7,list=FALSE)
train_data <- wine_data[train,]
test_data <- wine_data[-train,]

table(train_data$review)
#Bad Good 
#968  152 
##data is unbalanced
table(test_data$review)
ctrl <- trainControl(method="cv",number=10,summaryFunction = twoClassSummary,
                     classProbs = T,savePredictions = T)
library(e1071)
#Logisticregression
set.seed(190)
d.log <- train(review~.-quality,data=wine_data,family="binomial",method="glm",metric="ROC", trControl=ctrl)
summary(d.log)
d.log
#ROC        Sens       Spec     
#0.8706762  0.9638202  0.3365801
confusionMatrix(d.log$pred$pred, d.log$pred$obs)
#the accuracy was Accuracy : 0.8787  and sensitivity is good:0.9638


#LDA
lda.log <- train(review~.-quality,data=wine_data,family="binomial",method="lda",metric="ROC", trControl=ctrl)
lda.log
#ROC        Sens       Spec    
#0.8765171  0.9529559  0.374026
#ROC is 0.8765 is better than Logistic Regression but the Sensitivity score is better 
#in Logistic Regresion
confusionMatrix(lda.log$pred$pred, lda.log$pred$obs)
#Accuracy : 0.8743  is slightly low than Logistic Regresison

#QDA
qda.log <- train(review~.-quality,data=wine_data,family="binomial",method="qda",metric="ROC", trControl=ctrl)
qda.log
#ROC        Sens      Spec     
#0.8538474  0.890684  0.5534632
#QDA does not perform well
getTrainPerf(qda.log)

confusionMatrix(qda.log$pred$pred, qda.log$pred$obs)
#Accuracy : 0.8449 is less than Logistic Regression and LDA

#KNN
knn.log <-  train(review~.-quality,data=wine_data, method="knn", metric="ROC", trControl=ctrl, tuneLength=10) #let caret decide 10 best parameters to search
knn.log
#k=7 gave best ROC curve
plot(knn.log)
confusionMatrix(knn.log$pred$pred, knn.log$pred$obs)
#Accuracy :Accuracy : 0.8629  better than qda
#

#comparing the mode

d.models <- list("logit"=d.log, "lda"=lda.log, "qda"=qda.log,
"knn"=knn.log)
d.resamples = resamples(d.models)
#plot performance comparisons
bwplot(d.resamples, metric="ROC") 
bwplot(d.resamples, metric="Sens") #predicting default dependant on threshold
#but here it chosed sens vs ROC
##variance spread out high is not good like lda,qda
bwplot(d.resamples, metric="Spec") 





