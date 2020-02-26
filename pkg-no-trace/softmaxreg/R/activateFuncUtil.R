sigmoid<-function(x) {
	# Sigmoid function
	y = 1/(1+exp(-x)) 
	return (y)
}

sigmoidDeri<-function(x){
	# Sigmoid Derivative Function
	y = sigmoid(x)*(1-sigmoid(x))
	return (y)
}

tanhFunc<-function(x) {
	y = tanh(x)
	return (y)
}

tanhDeri<-function(x) {
	y = 1 - (tanh(x))^2
	return (y)
}

ReLuFunc<-function(x) {
	num = length(x)
	y = rep(0, num)
	for (i in 1:num) {
		y[i] = max(0,x[i])
	}
	return (y)
}

ReLuDeri<-function(x) {
	num = length(x)
	y = rep(0.0 , num)
	for (i in 1:num) {
		if (x[i] > 0) {
			y[i] = 1
		} else if(x[i] < 0) {
			y[i] = 0
		} else {
			y[i] = NA
		}
	}
	return (y)
}

softmaxFunc<-function(x){
	# SoftMax Transfer Function
	nDim = length(x) 
	res = rep(0.0, nDim)
	for (i in 1:nDim) {
		xDiff = x - x[i]
		sumExpVect = sum(exp(xDiff))
		res[i] = 1/sumExpVect
	}
	
	if (is.nan(sum(res))) { # exploding there is na or Inf in the vect due to exp function
		res = rep(0.0, nDim)
		maxIdx = which(x == max(x))[1]
		res[i] = 1.0
	}
	return (res)
}

activate<-function(x,funName) {
	if (tolower(funName) == 'sigmoid') {
		res = matrix(sigmoid(x))
	} else if (tolower(funName) == 'tanh') {
		res = matrix(tanhFunc(x))
	} else if (tolower(funName) == 'relu') {
		res = matrix(ReLuFunc(x))
	} else {
		res = "function name not found"
	}
	return (res)
}

activateDeri<-function(x,funName) {
	if (tolower(funName) =='sigmoid') {
		res = matrix(sigmoidDeri(x))
	} else if (tolower(funName) == 'tanh') {
		res = matrix(tanhDeri(x))
	} else if (tolower(funName) == 'relu') {
		res = matrix(ReLuDeri(x))
	} else {
		res = "function name not found"
	}
	return (res)
}

isInteger <- function(x){
	test <- all.equal(x, as.integer(x), check.attributes = FALSE)
	if(test == TRUE){ 
		return(TRUE) 
	}
	else { 
		return(FALSE) 
	}
}

addList<-function(listA, listB){
	list_new = list()
	numA = length(listA)
	numB = length(listB)
	if (numA == numB) {
		for (i in 1:numA) {
			list_new = c(list_new, list(listA[[i]] + listB[[i]]))
		}
	}
	return (list_new)
}

subtractList<-function(listA, listB){
	list_new = list()
	numA = length(listA)
	numB = length(listB)
	if (numA == numB) {
		for (i in 1:numA) {
			list_new = c(list_new, list(listA[[i]] - listB[[i]]))
		}
	}
	return (list_new)
}

squareList<-function(listA){
	list_new = list()
	num = length(listA)
	for (i in 1:num) {
		list_new = c(list_new, list(listA[[i]] * listA[[i]]))
	}
	return (list_new)
}

rootList<-function(listA){
	list_new = list()
	num = length(listA)
	for (i in 1:num) {
		list_new = c(list_new, list(sqrt(listA[[i]])))
	}
	return (list_new)
}

divideList<-function(listA, divideNum){
	num = length(listA)
	list_new = c()
	for (i in 1:num) {
		curList = listA[[i]]/divideNum
		list_new = c(list_new, list(curList))
	}
	return (list_new)
}

divideListByList<-function(listA, listB) {
	numA = length(listA)
	numB = length(listB)
	list_new = c()
	if (numA != numB) {
		return
	} else {
		for (i in 1:numA) {
			curList = listA[[i]]/listB[[i]]
			list_new = c(list_new, list(curList))
		}
	}
	return (list_new)
}

multiplyList<-function(listA, multiplier){
	num = length(listA)
	list_new = c()
	for (i in 1:num) {
		curList = listA[[i]] * multiplier
		list_new = c(list_new, list(curList))
	}
	return (list_new)
}

multiplyListByList<-function(listA, listB){
	numA = length(listA)
	numB = length(listB)
	list_new = c()
	if (numA != numB) {
		return
	} else {
		for (i in 1:numA) {
			curList = listA[[i]] * listB[[i]]
			list_new = c(list_new, list(curList))
		}
	}
	return (list_new)
}


dividePara<-function(param, divideNum){
	newParam = param
	# W B
	nLayer = length(param$W)
	for (i in 1:nLayer) {
		newParam$W[[i]] = newParam$W[[i]]/divideNum
		newParam$B[[i]] = newParam$B[[i]]/divideNum
	}
	return (newParam)
}






