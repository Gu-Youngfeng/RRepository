# CART Implmentation in R ###############################
library(rpart)
library(caret)
# library(rattle)

round.down <- function(samples){
	return (round(samples/10+1/2))
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
	cat("[min.split]:", min.split, ", [min.bucket]:", min.bucket, "\n")
	
	# step 2: model building
	outCart <- rpart(
		AverageTimePerIteration ~ .,
		train,
		method = "anova",
		control = rpart.control(
				minsplit = min.split, 
				minbucket = min.bucket,
				cp=0
				)
		)
	
	# return model
	
	plot(outCart)
	text(outCart)
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
	folds <- createFolds(y=data$AverageTimePerIteration, k=500)
	# folds is a list of vector, folds[[i]] means selected index in the i-th fold.

	actuals <- data[folds[[1]],15]
	#print(data[folds[[1]],15])

	train <- data[-folds[[1]],]
	test <- data[folds[[1]],]

	predicts <- evaluateCART (train, test)
	print(length(predicts))
	for(i in 1:length(predicts)){
		cat("predicted:", predicts[[i]], " actuals:", actuals[i], "\n")
	}
}



# main body #############################################

forDataset("HSMGP_num.csv")

#print(train)
#print(summary(train))
#print(nrow(train))

#print(round(3.14))
#print(2.4/5)