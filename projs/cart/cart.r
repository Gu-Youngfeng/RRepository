# CART Implmentation in R ###############################
#
# this file provides the general approach for,
# (1) k-fold cross validation
# (2) parameter tuning in CART classification

library(rpart)
library(caret)

# library(rattle)

round.down <- function(samples){
	return (round(samples/10+1/2))
}

mean.re <- function(actual, predict){
	return (abs(actual - predict)/actual)
}

# build CART
buildCart <- function(train){
	# step 1: stop criteria setting
	min.split <- 4
	min.bucket <- 2
	
	samples <- nrow(train)
	if(samples <= 100){
		min.bucket <- round.down(samples)
		min.split <- 2*min.bucket
	}else{
		min.split <- round.down(samples)
		min.bucket <- min.split/2
	}
	if(min.split<4){
		min.split <- 4
	}
	if(min.bucket<2){
		min.bucket <- 2
	}
	# cat("[min.split]:", min.split, ", [min.bucket]:", min.bucket, "\n")

	features.size <- length(colnames(train)) - 1
	if(features.size >= 30){
		features.size <- 30
	}
	
	# step 2: model building
	outCart <- rpart(
		Performance ~ ., # modify #
		train,
		method = "anova",
		control = rpart.control(
				minsplit = min.split, 
				minbucket = min.bucket,
				maxdepth = features.size,
				cp=0,
				usesurrogate=0,
				maxsurrogate=0)
		)
	
	# return model
	# plot(outCart)
	# text(outCart)
	return (outCart)
}

# evaluate CART
evaluateCART <- function(train, test){
	outCart <- buildCart(train)
	results <- predict(
		outCart, 
		test
	)
	return (results)
	
}

# for each dataset
forDataset <- function(path){
	data <- read.csv(path, header=TRUE, sep=",")
	set.seed(0)
	folds <- createFolds(y=data$Performance, k=10) # modify #
	# folds is a list of vector, folds[[i]] means selected index in the i-th fold.

	for(kf in 1:10){
		
		# cat("### The ", kf, "-th Prediction Result ###\n")
		train <- data[-folds[[kf]],]
		test <- data[folds[[kf]],]

		actuals <- data[folds[[kf]],40] # modify #
		#print(data[folds[[kf]],40])

		predicts <- evaluateCART (train, test)
		# print(nrow(test))
		# sum <- 0.0
		for(i in 1:nrow(test)){
			# sum <- sum + mean.re(actuals[i], predicts[[i]])
			# cat(" actuals:", actuals[i], "predicted:", predicts[[i]], "\n")
			output.file <- file("my.txt", "a")
			lines <- paste(actuals[i], ",", predicts[[i]])
			writeLines(lines, output.file)
			close(output.file)
		}
		# acc <- 1-sum/nrow(test)
		# print(acc)
	}
	
}



# main body #############################################

forDataset("SQL_AllMeasurements.csv")
# train <- read.csv("HSMGP_num.csv")
# outCart <- buildCart(train)
# plot(outCart)
# text(outCart)

# print(train)
# print(summary(train))
# print(nrow(train))

# print(round(3.14))
# print(2.4/5)