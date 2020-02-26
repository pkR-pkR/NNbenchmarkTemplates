#### Define S4 Class
softmax<-setClass("softmax",
	slots = c(weights ="list",
	data = "list",
	K = "numeric",
	iteration = "numeric",
	loss = "numeric",
	fitted.values = "matrix",
	convergence = "logical",
	type = "character",
	funName = "character"
	)
)

setMethod("$", "softmax",function(x, name)
{
	slot(x, name)
})

softmaxProp<-function(xVect, w, b) {
	# softmax layer
	outVect = w %*% xVect + matrix(b)
	softmaxVect = softmaxFunc(outVect)
	res = list(linearOut = matrix(outVect), activatedOut = matrix(softmaxVect))
	return (res)
}

forwardProp<-function(xVect, W, B, funName) {
	yPred = c()
	m = length(W) # m layNum
	outVecList = list() # net vector
	aVectList = list() # activated vector

	if (m == 1) { # No hidden layer
		propRes = softmaxProp(xVect, W[[1]], B[[1]])
		yPred = propRes$activatedOut
		outVecList = c(outVecList, list(propRes$linearOut))
		aVectList = c(aVectList, list(propRes$activatedOut))

	} else { # hidden layer 1:m
		curAVect = xVect
		for (l in 1:(m-1)) {
			wl = W[[l]]
			bVect = B[[l]]
			curOutVect = wl %*% matrix(curAVect) + matrix(bVect)
			curAVect = activate(curOutVect,funName)
			outVecList = c(outVecList,list(curOutVect))
			aVectList = c(aVectList,list(curAVect))
		}
		# final softmax layer
		propRes = softmaxProp(curAVect, W[[m]], B[[m]])
		yPred = propRes$activatedOut
		outVecList = c(outVecList, list(propRes$linearOut))
		aVectList = c(aVectList, list(propRes$activatedOut))
	}
	res = list(yPred = yPred, outVect = outVecList, aVect = aVectList)
	return (res)
}

forwardPropParallel<-function(idx, x, W, B, funName){
	res = c(forwardProp(xVect = as.numeric(x[idx, ]), W, B, funName)$yPred)
	return (res)
}

initPara<-function(nDim, hidden, K, rang, isZero = FALSE){
	# Set Randomness Seed
	set.seed(Sys.time())
	# nDim input dimension
	W=list()
	B=list()
	node = c(nDim, hidden, K)
	m = length(hidden) + 1 # number of hidden layer + 1

	if (isZero == TRUE) {
		for (l in 1:m) {
			curW = matrix(rep(0.0, node[l+1] * node[l]), ncol = node[l])
			curb = matrix(rep(0.0, node[l+1]), ncol = 1)
			W = c(W, list(curW))
			B = c(B, list(curb))
		}
	} else {
		for (l in 1:m) {
			curW = matrix(runif(node[l+1] * node[l], -rang, rang), ncol = node[l])
			curb = matrix(runif(node[l+1], -rang, rang), ncol = 1)
			W = c(W, list(curW))
			B = c(B, list(curb))
		}
	}
	res = list(W = W, B = B)
	return (res)
}

convertClass2Matrix<-function(target){
	targetClass = as.factor(target)
	num = length(target)
	mat = matrix(rep(0, num * length(levels(targetClass))), ncol = length(levels(targetClass)))
	mat[(1:num) + num*(unclass(targetClass)-1)] = 1
	dimnames(mat) = list(names(targetClass), levels(targetClass))
	return (mat)
}

convertList2Matrix<-function(vecList){
	nObs = length(vecList)
	nDim = length(vecList[[1]])
	mat = matrix(rep(0.0, nObs * nDim), ncol = nDim)
	for (i in 1:nObs) {
		mat[i,] = vecList[[i]]
	}
	return (mat)
}

calcLoss<-function(yMat, yFitMat, type, param, L2, penalty){
	loss = 0.0
	if (type == "class") {
		loss = softmaxLoss(yMat, yFitMat)
	} else if (type == "raw") {
		loss = MSELoss(yMat, yFitMat)
	}
    # Check if L2 Regularization is added
    if (L2 == TRUE) {
        loss = loss + addL2LossPenalty(param, penalty)
    }
	return (loss)
}

softmaxLoss<-function(yMat, yFitMat){
	lossTotal = 0.0
	nObs = dim(yMat)[1]
	nClass = dim(yMat)[2]
	for (i in 1:nObs) {
		curY = yMat[i,] # binary 0,1
		curYFit = yFitMat[i,]
		# curYFit[which(curYFit ==0.0)]= 1e-10 # remove 0
		lossTotal = lossTotal - sum(curY * log(curYFit))
	}
	loss = lossTotal/nObs
	return (loss)
}

getLogValue<-function(yMat){
	log_yMat = log(yMat)
	for (i in 1:dim(log_yMat)[1]) {
		curVect = as.numeric(log_yMat[i,])
		curVect[is.infinite(curVect)] = 0.0 # ingore 
		log_yMat[i,] = curVect
	}
	return (log_yMat)
}

MSELoss<-function(yMat, yFitMat){
	lossTotal = 0.0
	nObs = dim(yMat)[1]
	nClass = dim(yMat)[2]
	for (i in 1:nObs) {
		curY = yMat[i,] # double
		curYFit = yFitMat[i,]
		lossTotal = lossTotal + (1/2) * sum((curY-curYFit)^2)
	}
	loss = lossTotal/nObs
	return (loss)
}

copyShape<-function(matList, multiplier = 0.0){
	newList = list()
	for (i in 1:length(matList)) {
		newMat = matList[[i]] * multiplier
		newList = c(newList, list(newMat))
	}
	return (newList)
}
# newParam = gradientDescent(x, yMat, yFitMat, hidden, funName, curParam, rate, type)

#### L2 Regularization
addL2LossPenalty<-function(param, penalty){
	W = param$W
    loss = 0.0
    for (i in 1:length(W)) {
        loss = loss + sum(W[[i]]^2)
    }
    loss = (1/2) * penalty * loss
    return (loss)
}

addL2GradPenalty<-function(param, penalty){
    W = param$W
    m = length(W)
    dWL2 = vector("list", m)
    for (i in 1:m) {
        dWL2[[i]] = W[[i]] * penalty
    }
    return (dWL2)
}

gradientDescent<-function(param, x, yMat, yFitMat, funName, type, samp, algorithm, rate, cache, cache_delta, L2, penalty, v){
	# calculate cur gradient
	curGrad = calcGrad(samp, param, x, yMat, yFitMat, funName, type, L2, penalty)
	# update cache and parameters
	resList = updatePara(param, curGrad, algorithm, rate, cache, cache_delta, v)
	return (resList)
}

updatePara<-function(param, gradient, algorithm, rate, cache, cache_delta, v){
	res = c()
	cacheNew = c()
	W = param$W
	B = param$B
	m = length(W) # layer number
	W_new = vector("list",m)
	B_new = vector("list",m)
	delta_W = vector("list",m)
	delta_B = vector("list",m)
	dW = gradient$dW
	dB = gradient$dB
	
	if (tolower(algorithm) == "sgd") { # vanilla stochastic gradient descent
		for (k in 1:m) {
			W_new[[k]] = W[[k]] - rate * dW[[k]]
			B_new[[k]] = B[[k]] - rate * dB[[k]]
		}
		res = list(W = W_new, B = B_new, cache = c())
	} else if (tolower(algorithm) == "adagrad") {
		# formular cache += dx**2
		cacheW = addList(cache$dW, squareList(gradient$dW)) # historical sum of squared dW
		cacheB = addList(cache$dB, squareList(gradient$dB)) # historical sum of squared dB
		cacheNew = list(dW = cacheW, dB = cacheB)
		eps = 1e-4
		for (k in 1:m) {
			W_new[[k]] = W[[k]] - (rate/(sqrt(cacheW[[k]]) + eps)) * dW[[k]]
			B_new[[k]] = B[[k]] - (rate/(sqrt(cacheB[[k]]) + eps)) * dB[[k]]
		}
		res = list(W = W_new, B = B_new, cache = cacheNew)
	} else if (tolower(algorithm) == "rmsprop") {
		# formular cache = decay_rate * cache + (1 - decay_rate) * dx**2
		decay_rate = 0.90
		eps = 1e-4
		cacheW = addList(multiplyList(cache$dW, decay_rate), # historical sum of squared dW
					multiplyList(squareList(gradient$dW), (1 - decay_rate)))
		cacheB = addList(multiplyList(cache$dB, decay_rate), # historical sum of squared dB
					multiplyList(squareList(gradient$dB), (1 - decay_rate)))
		cacheNew = list(dW = cacheW, dB = cacheB)
		for (k in 1:m) {
			W_new[[k]] = W[[k]] - (rate/(sqrt(cacheW[[k]]) + eps)) * dW[[k]]
			B_new[[k]] = B[[k]] - (rate/(sqrt(cacheB[[k]]) + eps)) * dB[[k]]
		}
		res = list(W = W_new, B = B_new, cache = cacheNew)
	} else if (tolower(algorithm) == "momentum") {
		# cat("Cur Learning Algorithm", algorithm, '\n')
		mu = 0.95
		# v is a list storing velocity of parameter W and B
		vW = subtractList(multiplyList(v$W, mu), multiplyList(dW, rate)) # v = mu * v - rate * dx
		W_new = addList(W, vW) # W = W + v
		vB = subtractList(multiplyList(v$B, mu), multiplyList(dB, rate))
		B_new = addList(B, vB)
		vNew = list(W = vW, B = vB)
		res = list(W = W_new, B = B_new, v = vNew)
		
	} else if (tolower(algorithm) == "nag") { ## Nesterov Momentum Algorithm
        # cat("Cur Learning Algorithm", algorithm, '\n')
        mu = 0.95
        vW_prev = v$W # storage previous vW
        vW = subtractList(multiplyList(v$W, mu), multiplyList(dW, rate)) # v = mu * v - learning_rate * dx
        # x += -mu * v_prev + (1 + mu) * v
        W_new = addList(W, addList(multiplyList(vW_prev, (-1) * mu), multiplyList(vW, (1 + mu)))) 
        vB_prev = v$B # storage previous vB
        vB = subtractList(multiplyList(v$B, mu), multiplyList(dB, rate))
        B_new = addList(B, addList(multiplyList(vB_prev, (-1) * mu), multiplyList(vB, (1 + mu))))
        vNew = list(W = vW, B = vB)
        res = list(W = W_new, B = B_new, v = vNew)
	} else if (tolower(algorithm) == "adadelta") {
		decay_rate = 0.99
		eps = 1e-8
		
		cacheW = addList(multiplyList(cache$dW, decay_rate), multiplyList(squareList(gradient$dW), (1 - decay_rate)))
		cacheB = addList(multiplyList(cache$dB, decay_rate), multiplyList(squareList(gradient$dB), (1 - decay_rate)))
		cacheNew = list(dW = cacheW, dB = cacheB)
		
		# (-1) * (RMS_deltaW_prev/RMS_dW) * dW
		for (k in 1:m) {
			delta_W[[k]] = (-1) * (sqrt(cache_delta$deltaW[[k]])+ eps)/(sqrt(cacheW[[k]]) + eps)
			delta_B[[k]] = (-1) * (sqrt(cache_delta$deltaB[[k]])+ eps)/(sqrt(cacheB[[k]]) + eps)
		}
		# delta_W = multiplyList(multiplyListByList(divideListByList(RMS_deltaW_prev, RMS_dW), dW), -1)
		# delta_B = multiplyList(multiplyListByList(divideListByList(RMS_deltaB_prev, RMS_dB), dB), -1)
		W_new = addList(W, delta_W)
		B_new = addList(B, delta_B)
		
		# update cache_delta
		cache_delta = list(deltaW = addList(multiplyList(cache_delta$deltaW, decay_rate), multiplyList(squareList(delta_W), (1 - decay_rate))),
			deltaB = addList(multiplyList(cache_delta$deltaB, decay_rate), multiplyList(squareList(delta_B), (1 - decay_rate))))
		
		res = list(W = W_new, B = B_new, cache = cacheNew, cache_delta = cache_delta)
	} else {
        cat("Algorithm Not Found, try one of 'moment', 'nag', 'adagrad', 'rmsprop', 'adadelta', 'adam'", '\n')
    }
	return (res)
}

# Get Mini-Batch Random Samples
getRandomBatch<-function(nSample, batch_size){
    if (nSample <= batch_size) {
        batch_size = nSample
    }
    v = c(1:nSample)
    set.seed(Sys.time())
    r = sample(v)
    idx = 1
    rand = list()
    while(idx < nSample) {
        curBatch = r[idx:min(idx + batch_size -1, nSample)]
        rand = c(rand, list(curBatch))
        idx = idx + batch_size
    }
    return (rand)
}

calcGrad<-function(samp, param, x, yMat, yFitMat, funName, type, L2, penalty){
	W = param$W
	B = param$B
	nDim = dim(x)[2]
	nObs = dim(x)[1]
	dW = copyShape(W, 0)
	dB = copyShape(B, 0)
	m = length(W)
	deltaList = vector("list",m)
	
	# Final Layer m
	delta_m = c()
	#eps = 1e-8 # update 0 to small eps
	#curYMat = yMat
	#curYFitMat = yFitMat
	#curYMat[which(curYMat==0)] = eps
	#curYFitMat[which(curYFitMat==0)] = eps
	# log_yMat = getLogValue(curYMat)
	# log_yFitMat = getLogValue(curYFitMat)
	
	for (i in 1:length(samp)) {
		idx = samp[i]
		# Final Layer delta, Loss/net
		if (type == "class") {
			delta_m = matrix(yFitMat[idx,] - yMat[idx,]) # layer m softmax layer error term
		} else if (type == "raw") {
			delta_y = c(yMat[idx,] - yFitMat[idx,])
			delta_m = (-1) * delta_y * (yFitMat[idx,] * (1 - yFitMat[idx,])) # element multiplication
		}
		
		xVect = as.numeric(x[idx,])
		outVect = forwardProp(xVect, W, B, funName)$outVect
		aVect = forwardProp(xVect, W, B, funName)$aVect
		
		if (m == 1) {
			# calc delta
			deltaList[[m]] = delta_m
			# calc gradient dW, dB
			dW[[m]] = dW[[m]] + deltaList[[m]] %*% matrix(xVect, nrow = 1)
			dB[[m]] = dB[[m]] + deltaList[[m]]
		} else if (m > 1) {
			# calc delta Layer m 
			deltaList[[m]] = delta_m
			curDelta = delta_m
			for (k in (m-1):1) {
				nDim = dim(outVect[[k]])[1]
				diagMat = matrix(rep(0,nDim*nDim),nrow = nDim)
				diag(diagMat) = c(activateDeri(outVect[[k]], funName))
				curDelta = diagMat %*% t(W[[k+1]]) %*% curDelta
				deltaList[[k]]= curDelta
			}
			# clac gradient Layer m
			dW[[m]] = dW[[m]] + deltaList[[m]] %*% matrix(aVect[[m-1]], nrow = 1)
			dB[[m]] = dB[[m]] + deltaList[[m]]
			# clac gradient Layer m-1 to 1
			for (k in (m-1):1) {
				if (k > 1) {
					dW[[k]] = dW[[k]] + deltaList[[k]] %*% matrix(aVect[[k-1]], nrow = 1)
					dB[[k]] = dB[[k]] + deltaList[[k]]
				} else {
					dW[[k]] = dW[[k]] + deltaList[[k]] %*% matrix(xVect, nrow = 1)
					dB[[k]] = dB[[k]] + deltaList[[k]]
				}
			}
		}
	}
	dW = divideList(dW, length(samp)) # mean dW over cur Batch sample number
	dB = divideList(dB, length(samp)) # mean dB over cur Batch sample number
    # Checking L2 Regularization Term
    if (L2 == TRUE) {
        dW = addList(dW, addL2GradPenalty(param, penalty))
    }
	res = list(dW = dW, dB = dB)
	return (res)
}

checkGradMode<-function(gradList, threshold) { # Vanishing or exploding gradient
	gradW = gradList$dW
	gradB = gradList$dB
	paramVect = c()
	for (i in 1:length(gradW)) {
		paramVect = c(paramVect, c(gradW[[i]]))
		paramVect = c(paramVect, c(gradB[[i]]))
	}
	
	# Checking if Norm Vanish or Explode
	vectMode = sqrt(sum(paramVect^2))
	if (vectMode > threshold) {
		# cat("Gradient Clipped to Threshold","\n")
		newGrad = dividePara(gradList, (vectMode/threshold))
	} else {
		newGrad = gradList
	}
	return(newGrad)
}

calcInfoCriteria<-function(object, method){
    # object is a object returned by softmaxreg function
    li = 1.0 # likelihood
    k = 0 # number of parameters
    x = object$data$x
    nObs = dim(x)[1]
    type = object$type
    yFitMat = object$fitted.values
    # Caculate li and k
    if (tolower(type)=="class") {
        yMat = convertClass2Matrix(as.factor(as.character(object$data$y)))
        for (i in 1:nObs) { # yMat is one hot vector
            li = li * (yFitMat[i,][which(yMat[i,]==1)]) # multiplication of likelihood
        }
        names(li)<-c() # remove names
        wts = object$weights
        W = wts$W
        B = wts$B
        nLayer = length(W)
        for (j in 1:nLayer) {
            k = k + length(W[[j]])
            k = k + length(B[[j]])
        }
    } else if (tolower(type)=="raw") {
        
    }
    ic = 0.0
    if (tolower(method)=="aic"){
        aic = -2 * log(li) + 2 * k
        ic = aic
    } else if (tolower(method)=="bic") {
        bic = -2 * log(li) + log(nObs) * k
        ic = bic
    }
    return (ic)
}

trainModel<-function(x, y, hidden, funName, maxit, rang, type, algorithm, rate, L2, penalty, threshold, batch, echo=FALSE){
	if(echo)
	  cat("Softmax Regression model training started","\n")
	
	K = c()
	if (type == "class") {
		x = as.matrix(x)
		yMat = convertClass2Matrix(as.factor(as.character(y)))
		K = dim(yMat)[2]
	} else {
		x = as.matrix(x)
		yMat = as.matrix(y)
		K = dim(yMat)[2]
	}
	
	converged = FALSE
	minLoss = 1e10
	loss = c()
	# threshold = 1e-4
	nObs = dim(x)[1] # x input matrix: nobs * nDim
	nDim = dim(x)[2]
	idx = c(1:nObs)
	optimParam = c()
	curParam = initPara(nDim, hidden, K, rang, isZero = FALSE) # seed
	cache = list(dW = copyShape(curParam$W,0.0), dB = copyShape(curParam$B,0.0)) # cache variable: historical sum of squared gradient dx
    cache_delta = list(deltaW = copyShape(curParam$W,0.0), deltaB = copyShape(curParam$B,0.0)) # cache_delta variable: historical sum of squared delta_X
	v = list(W = copyShape(curParam$W,0.0), B = copyShape(curParam$B,0.0)) # velocity for momentum update algorithm
    
	yFitMat = c()
	iteration = c() # Iteration
	
	for (k in 1:maxit) {
        rand = getRandomBatch(nObs, batch)
        sampFin = 0 # number of sample finished
        if(echo) cat("Loop number: ", k , "\n")
        for (j in 1:length(rand)) { # iteration over mini-batches
            # Mini-Batch samples
            samp = rand[[j]]
            sampFin = sampFin + length(samp)
            
            curFitList = lapply(idx, FUN = forwardPropParallel, x, W = curParam$W, B = curParam$B, funName)
            yFitMat = convertList2Matrix(curFitList)
            colnames(yFitMat) = colnames(yMat)
            curLoss = calcLoss(yMat, yFitMat, type, curParam, L2, penalty)
            loss = c(loss, curLoss)
            if(echo) cat("Complete sample number: ", sampFin ," Current Loss Function Value ", curLoss," Best Loss Function Value ", minLoss, "\n")
            if (curLoss < threshold) { # Converged
                converged = TRUE
                minLoss = curLoss
                optimParam = curParam
                iteration = k
                break
            } else {
                if (curLoss < minLoss) {
                    minLoss = curLoss
                    optimParam = curParam
                }
            }
            curRes = gradientDescent(curParam, x, yMat, yFitMat, funName, type, samp, algorithm, rate, cache, cache_delta, L2, penalty, v)
            cache = curRes$cache # update cache
            cache_delta = curRes$cache_delta # update cache delta
            v = curRes$v # update velocity
            newParam = list(W = curRes$W, B = curRes$B) # Update Parameters
            # newParam = checkGradMode(newParam, threshold = 10) # Gradient Explosion
            curParam = newParam
        }
        iteration = k
	}
	mySoftmax = softmax(weights = optimParam, data = list(x = x, y = y), K = K, loss = loss,
		fitted.values = yFitMat, iteration = iteration, convergence = converged, type = type, funName = funName)
	return (mySoftmax)
}
