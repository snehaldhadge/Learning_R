setwd("~/Documents/GitHub/Learning_R")
library(psych)
library(ggplot2)
library(corrplot)
wine_data <- read_csv('winequality-red.csv')
head(wine_data)

#Viewing the spread of the data
multi.hist(wine_data)

#correlation plot between columns
cor.plot(wine_data)

#scatter Matrix of variables
plot(wine_data)

corrplot(cor(wine_data))

table(wine_data$quality)

wine_data$review <- ifelse(wine_data$quality>6,1,0)

#wine_data <- wine_data %>% 
#  mutate(review=if_else(quality>6,'Good','Bad')) 

is.na(wine_data)
  
glm.fit <- glm(wine_data$review~.-quality,data=wine_data)
summary(glm.fit)

