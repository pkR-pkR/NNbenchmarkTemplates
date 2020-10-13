nlsr.method <- "none"
hyperParams.nlsr <- function(...) {
    return (list(iter = maxit2ndorder, sdnormstart = 0.1))
}
NNtrain.nlsr <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    start <- round(rnorm(NNparam, sd = hyper_params$sdnormstart), 4)
    names(start)  <- paste0("b", 1:NNparam)
    
    NNreg <- nlsr::nlxb(NNfullformula, data = dataxy, start = start, control = list(femax = hyper_params$iter))
    return(NNreg)
}
NNpredict.nlsr <- function(object, x, ...)
  as.numeric(predict(object, x))
NNclose.nlsr <- function()
  if("package:nlsr" %in% search())
    detach("package:nlsr", unload=TRUE)
nlsr.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
