MachineShop.method <- "none"
hyperParams.MachineShop <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE, linout=TRUE))
}
NNtrain.MachineShop <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    trace <- hyper_params$trace
    maxit <- hyper_params$iter
    linout <- hyper_params$linout #linearoutpputunit
    myNN <- MachineShop::NNetModel(size = neur, linout = linout, maxit = maxit,
                                   trace=trace)
    MachineShop::fit(formula, data = dataxy, model = myNN)
    
}
NNpredict.MachineShop <- function(object, x, ...)
    as.numeric(predict(object, newdata=x, type="response"))
NNclose.MachineShop <- function()
  if("package:MachineShop" %in% search())
    detach("package:MachineShop", unload=TRUE)
MachineShop.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
