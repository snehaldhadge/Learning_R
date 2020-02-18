# Introduction to R


setwd("/Users/snehalilawe/Desktop/Spring 2020/MIS-749-BA/Week2 R-Intro")

# Basic Commands
#create vectors (c function)
x <- c(1,3,2,5)
x
x <-  c(1,6,2)
x
y <-  c(1,4,3)

v <- c(1:10)

w <- c(15:24)

nv <- v *pi

nw <- w * pi

#count elements
length(x)
length(y)

x+y # needs to be of same length
ls() #see objects in enviornment (same as rstudio tab Environment)

#remove objects from enviornment. Needed sometimes when you are using lots of memory
rm(x,y)
ls()

#remove everything from memory same as restarting R
rm(list=ls())

#working with more than 1 dimenson you create a matrix
?matrix
x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2) #2 row and 2 columns
x
x <- matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE) #if you want it to be constructed by row instead of column

#math operations on matrix are similar to vector
sqrt(x)
x^2

#generating random variables and some basic descrive statistics

#basic simulation create function: y = 50 + x
###rnorm random normal distri gives data that is nrmally distri mean=0 and STD=1
set.seed(20)
#x will be same numbers each time (reproducible)
x <- rnorm(50) #defaults to mean of 0 and sd 1
summary(x)
sd(x)
hist(x)


library(psych)
describe(x)

#lets create y as a function of x, with different mean and SD

y <- x+rnorm(50,mean=50,sd=.1)
summary(y)
hist(y)

#lets look at relationship
cor(x,y) #correlation 
plot(y ~ x) #scatterplot
#add a label
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")

#first simple linear regression
xy.lm <- lm(y ~ x)
summary(xy.lm) #summarize the lm object

predict(xy.lm,x)
#notice the residual standard error our estimate of irreducible error

#we can extract variables from model object
names(xy.lm)
xy.lm$fitted.values #predictive y
xy.lm$residuals # actual y - predicted y

#we can verify by subtracting predicted y from 
y-xy.lm$fitted.values #values of y we predicted

#average squared prediction error MSE
mean(xy.lm$residuals^2)


write.csv(x, "x.csv")

saveRDS(xy.lm, "xlmmodel.rdata") #save obj of model
xycopy <- readRDS(file = "xlmmodel.rdata")
summary(xycopy)
#####################IMPORTING your own data##########################
#lets read in some data
#if you set your working directory at the top with setwd()

getwd() #show you current directory R will default to

# import a csv file of the total annual sales for each customer
sales <- read.csv("yearly_sales.csv",
                  stringsAsFactors=F) #read csv file text file into memory

# examine the imported dataset
class(sales) #basic object is data.frame
head(sales)

str(sales)

names(sales)
sales$genderf <- factor(sales$gender)

sales$numbers <-1:10000

#log to normallize the data
sales$lsales_total <- log(sales$sales_total)


hist(sales$sales_total)
hist(sales$lsales_total)
summary(sales)

library(psych)
#drescriptive of sales_total by gender
describeBy(sales$sales_total, sales$gender)

sales[,1]

#we don't need cust_id lets subset
names(sales) #custid is 1 of 4 columns
names(sales[,-1])  #row, column means omit column 1
names(sales[,2:4]) #include only column 2:4 same 
names(sales[,-1])

names(sales)

#subsetting if you want to split by row values
#there are few ways to do this, but we will start with base R way
#return only females in the data
#notice the double equal size! that is for equivalence, = alone is reserved for assignment
sales[sales$gender=="F",] #using subset notation
subset(sales, gender=="F") #using subset function

#can create a new data.frame from this
sales.F <- sales[sales$gender=="F",] #Females
sales.M <- sales[sales$gender=="M",] #Males

head(sales.M)

#suupose we decide orders over 10 are outliers and we would like to omit them
summary(sales$num_of_orders)
boxplot(sales$num_of_orders)

#create new data.frame removing all records with order < 11
sales.lessorders <- sales[sales$num_of_orders<11,] #only orders less than11
summary(sales.lessorders$num_of_orders)
boxplot(sales.lessorders$num_of_orders)


#since we have no use for custid we can remove it by saving over sales object
sales <- sales[,-1]
names(sales)

#lets visualize the data group by gender
boxplot(sales$lsales_total ~ sales$gender)
hist(sales$num_of_orders)
plot(sales$gender)

#look at relationship between all data
pairs(sales[,-c(3:4)]) #gives correlation here we omit non numberic columns

# plot num_of_orders vs. sales
plot(sales$num_of_orders,sales$sales_total,
     main="Number of Orders vs. Sales")

#lets conduct a linear regression, notice we specify data.frame
results <- lm(sales_total ~ num_of_orders, data=sales)
summary(results)

#we can add this regression line to the plot
plot(sales$num_of_orders,sales$sales_total,
     main="Number of Orders vs. Sales")

abline(results) #paint the line on the plot created above

#add lowess line, local regression line Lower f value is less smoth more flexible
##this is lowest regression fit
##f is flexibility
lines(lowess(sales$sales_total ~ sales$num_of_orders, f=.5), col="blue")

#green is more flexible
lines(lowess(sales$sales_total ~ sales$num_of_orders,f=.001), col="green")

#we can run the regression on subseted data.frame we created earlier
#just change the data argument
result.lessorders <- lm(sales_total ~ num_of_orders, data=sales.lessorders)
summary(result.lessorders)

#so we can look at some options for categorical data lets simulate customer type variable (A and B)
#lets simulate two qualitative variables
sales$cust.type<-sample(c("A","B"), 10000, replace=T, prob=c(.7, .3))  #customer A is 70% likely B 30%

#make sure r knows its a factor
sales$cust.type <- factor(sales$cust.type)



#lets create cross table of cust type
table(sales$cust.type)
plot(sales$cust.type, Xlab="Customer Type", ylab="Count")

#lets look at gender
table(sales$gender)

#lets compare gender and cust.type
gcust.table<-table(sales$gender, sales$cust.type) #it matches 70/30 random distribution
gcust.table

#chi-square test on gender vs cust.type
chisq.test(gcust.table) # they are independant 


#some graphcis of qualitative data
barplot(gcust.table, col=c("red","blue")) #goes by order of levels
sales$gender #female first then male
#let's paint a legend on this plot
legend("topright",legend=c("Female","Male"), fill=c("red", "blue"))

#create a mosaic plot
plot(sales$gender ~ sales$cust.type, col=c("red","blue")) #mosaic plot

,stringsAsFactors=F

titanic <- read.csv("titanic.csv")
str(titanic)
summary(titanic)
boxplot(titanic$Survived ~ titanic$Pclass)

table(titanic$Sex,titanic$Survived)

pclass.table <- table(titanic$Pclass,titanic$Survived)
pclass.table/sum(pclass.table)* 100

pairs(titanic)

table(titanic$Sex,titanic$Survived,titanic$Pclass)
