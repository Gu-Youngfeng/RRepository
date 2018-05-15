##############################################
# This R file is used to compare the diff results between CART in R and Python

# step1: read result csv from R and Python, then plot them in one figure
# read result csv from R

# png(file="SQL.png")

data.r <- read.csv("data/SQL-R.csv") # modify #
data.r <- data.r[order(data.r$actual),] # sort by actual attribute
predict1 <- data.r$predict
actual1 <- data.r$actual

windows(width=16,height=4)
par(mfrow=c(1,2))

plot(actual1, type="l", col="green", xlab="configuration index", ylab="performance", main="(a)Performance Prediction Using R")
lines(predict1, type="l", col="blue")

# read result csv from Python
data.python <- read.csv("data/SQL-Python.csv") # modify #
data.python <- data.python[order(data.python$actual),] # sort by actual attribute
actual2 <- data.python$actual
predict2 <- data.python$predict

plot(actual2, type="l", col="green", xlab="configuration index", ylab="performance", main="(b)Performance Prediction Using Python")
lines(predict2, type="l", col="red")

# step2: calculate their mean relative errors

mean.re <- function(actual, predict){
	return (abs(actual - predict)/actual)
}

sum1 <- 0
for(index in nrow(data.r)){
	sum1 <- sum1 + mean.re(data.r$actual[index], data.r$predict[index])
}
acc1 <- 1 - (sum1)/nrow(data.r)

sum2 <- 0
for(index in nrow(data.python)){
	sum2 <- sum2 + mean.re(data.python$actual[index], data.python$predict[index])
}
acc2 <- 1 - (sum2)/nrow(data.python)

cat("[R acc]:", acc1, "[P acc]:", acc2)

# dev.off()