print("hello")
d <- read.csv("yearly_sales.csv")

summary(d)

str(d) #structure of d

table(d$gender)

d$index <- 1:10000

view(d)

hist(d$sales_total)

#creating new column

d$sales_total.log <- log(d$sales_total)

hist(d$sales_total.log)


write.csv(d,"salesdata-updated.csv")


e.data <- d
model <- lm(sales_total ~ num_of_orders,data=d)

levels <- c ( "Wow", "Good","Bad" )

ratings <- c( "Bad", "Bad", "Wow" )

f <- factor(ratings,levels)
f

levels[1:3]
