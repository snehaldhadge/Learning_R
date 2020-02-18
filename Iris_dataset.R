library(datasets)
library(nnet)
library(dplyr)

data(iris)
str(iris)

#count number of each species 
table(iris$Species)

#use only 2 species
iris.small <- filter(iris, Species %in% c("virginica", "versicolor"))

#logistic Regression
lm.fit <- glm(Species~.,data=iris.small,family = binomial)
summary(lm.fit)
sample_irisdata <- tibble(Sepal.Length=6.4, Sepal.Width=2.8, Petal.Length=4.6, Petal.Width=1.8)
#Predict
predict(lm.fit, sample_irisdata, type="response")
#plant has 30% of being virginica

#sepal Length has highest p-value so we can remove that predictor
lm.fit1 <- glm(Species~Sepal.Width+Petal.Length+Petal.Width,data=iris.small,family = binomial)
summary(lm.fit1)
lr_data <- data.frame(predictor=lm.fit1$linear.predictors, prob=lm.fit1$fitted.values, Species=iris.small$Species)
ggplot(lr_data, aes(x=predictor, y=prob, color=Species)) + geom_point()

ggplot(lr_data, aes(x=predictor, fill=Species)) + geom_density(alpha=.5)
predict(lm.fit, sample_irisdata, type="response")
#plant has 30% of being virginica
plant <- tibble( Sepal.Width=3.3, Petal.Length=5.2, Petal.Width=2.3)
predict(lm.fit1, plant, type="response")
#plant has 99% chance of being virginica

glm.fit <- multinom(Species~.,data=iris.small)
summary(glm.fit)
predict(glm.fit , iris.small[50:60,])
iris.small[50:60,]

unique(iris.small$Species)
table(iris.small$Species)

plant1 <- tibble( Sepal.Length=5.8,Sepal.Width=2.6, Petal.Length=4.0, Petal.Width=1.2)
lda.fit <- lda(Species~.,data=iris)
plot(lda.fit)
predict(lda.fit,sample_irisdata)
predict(lda.fit,plant1)



