##############################################
# This R file is used to compare the diff results between CART in R and Python

# step1: read result csv from R and Python, then plot them in one figure
# read result csv from R
data.r <- read.csv("data/HSMGP-R.csv")
predict1 <- data.r$predict
actual1 <- data.r$actual

plot(sort(actual1), type="l", col="green")
lines(sort(predict1), type="l", col="blue")

# read result csv from Python
data.python <- read.csv("data/HSMGP-Python.csv")
actual2 <- data.python$actual
predict2 <- data.python$predict

lines(sort(predict2), type="l", col="red")

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