nnet.method <- "none"
hyperParams.nnet <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.nnet <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    NNreg <- nnet::nnet(x, y, size = neur, linout = TRUE, maxit = hyper_params$iter, trace=hyper_params$trace)
    return(NNreg)
}
NNpredict.nnet <- function(object, x, ...)
    predict(object, newdata=x)
NNclose.nnet <- function()
  if("package:nnet" %in% search())
    detach("package:nnet", unload=TRUE)
nnet.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
