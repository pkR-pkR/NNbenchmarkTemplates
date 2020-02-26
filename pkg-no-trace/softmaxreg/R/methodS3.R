# S3 Generic methods
predict.softmax<-function(object, newdata, ...) {
    param = object$weights
    type = object$type
    funName = object$funName
    nObs = dim(newdata)[1]
    idx = c(1:nObs)
    curFitList = lapply(idx, FUN = forwardPropParallel, newdata, W = param$W, B = param$B, funName)
    yFitMat = convertList2Matrix(curFitList)
    yFit = c()
    if (type == "class") {
        cl = colnames(yFitMat)
        for (i in 1:nObs) {
            yFit = c(yFit, which(yFitMat[i,]==max(yFitMat[i,]))[1])
        }
    } else if (type == "raw") {
        yFit = yFitMat
    }
    return (yFit)
}

summary.softmax<-function(object, ...){
    z <- object
    minLoss <- min(z$loss)
    iteration <- z$iteration
    convergence <- z$convergence
    wts <- z$weights
    W <- wts$W
    x <- z$data$x
    y <- z$data$y
    K <- z$K
    node<-c(dim(x)[2])
    for (i in 1:length(W)) {
        node = c(node, dim(W[[i]])[1])
    }
    aic <- AIC(z)
    bic <- BIC(z)
    
    cat("Softmax Regression Model",'\n')
    cat('\n')
    cat("Node Number of Each Layer :",paste(node, collapse = '-'),'\n')
    cat('\n')
    cat("Class Number", K,":",paste(unique(as.character(y)),collapse = ', '),'\n')
    cat('\n')
    cat("Iteration Number:", iteration, '\n')
    cat('\n')
    cat("Minimum Loss:", minLoss, '\n')
    cat('\n')
    cat('AIC:', aic, '\n')
    cat('\n')
    cat('BIC:', bic, '\n')
    cat('\n')
    cat("Model Parameter:",'\n')
    ans = list(W = wts$W, B = wts$B)
    class(ans) <- "summary.softmax"
    return (ans)
}

#### softmaxReg default and formula method
softmaxReg<-function(x, ...){
    UseMethod("softmaxReg")
}

softmaxReg.default<-function(x, y, hidden = c(), funName = 'sigmoid', maxit = 3000, rang = 0.1, type = "class", algorithm = "rmsprop", rate = 0.05,
        L2 = FALSE, penalty = 1e-4, threshold = 1e-4, batch = 50, ...){
    argsCheck = hasArg(x) && hasArg(y)
    softmax_model = c()
    if (argsCheck == FALSE) {
        cat ('Possible parameter missing: x, y',"\n")
        return
    } else {
        softmax_model = trainModel(x, y, hidden, funName, maxit, rang , type, algorithm, rate, L2, penalty, threshold, batch)
    }
    return (softmax_model)
}

softmaxReg.formula<-function (formula, data, hidden = c(), funName = 'sigmoid', maxit = 3000, rang = 0.1, type = "class", algorithm = "rmsprop", rate = 0.05,
        L2 = FALSE, penalty = 1e-4, threshold = 1e-4, batch = 50,...){
    
    argsCheck = hasArg(formula) && hasArg(data)
    softmax_model = c()
    if (argsCheck == FALSE) {
        cat ('Possible parameter missing: formula, data',"\n")
        return
    } else {
    cat('Formula: ', format(formula),'\n') # print formula
    mf <- model.frame(formula, data=data)
    Terms <- attr(mf, "terms")
    vars <- as.character(attr(Terms, "variables"))[-1]
    depVars <- as.character(vars[-1]) # Dependent Variables
    response <- vars[attr(Terms, "response")] # index of response var
    resVars <- c() # Response Variables
    if (type == "raw") { # Checking if formula is correct
        if (grepl("cbind", response)) { ## formula pass the check reponse : cbind(y1,y2,...)
            response <- gsub("\\s","", response)
            response <- gsub("cbind","", response)
            response <- gsub("\\(","", response)
            response <- gsub("\\)","", response)
            resVars <- as.character(unlist(strsplit(response, ",")))
            if (length(resVars)==1) {
                cat("Error Info: Type 'raw' input formula should have more than 1 response variable.", "\n")
                return
            }
        } else {
            cat("Error Info: Type 'raw' input formula should have form 'cbind(y1,y2,...)~x1 + x2 + ...'.", "\n")
            return
        }
    } else if (type == "class") {
        if (grepl("\\+", response) || grepl(",", response)) {
            cat("Error Info: Type 'class' input formula should have only 1 response variable", "\n")
            return
        } else { ## formula pass the check
            resVars <- as.character(response)
        }
    }
    cat(length(depVars)," dependent Variables:", paste(depVars, sep=","), '\n')
    cat(length(resVars)," response Variables:", paste(resVars, sep=","), '\n')
    dframe <- as.data.frame(data)
    x <- dframe[, depVars]
    y <- dframe[, resVars]
    softmax_model = trainModel(x, y, hidden, funName, maxit, rang , type, algorithm, rate, L2, penalty, threshold, batch)
    }
    return (softmax_model)
}

AIC.softmax<-function(object, ...){
    aic = calcInfoCriteria(object, method = "AIC")
    return (aic)
}

BIC.softmax<-function(object, ...){
    bic = calcInfoCriteria(object, method = "BIC")
    return (bic)
}
