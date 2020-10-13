rminer.method <- "none"
hyperParams.rminer <- function(...) {
    return (list(task="reg", iter=maxit2ndorder))
}
NNtrain.rminer <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    rminer::fit(formula, data = dataxy, model = "mlp", task = hyper_params$task, 
                                        size = neur, maxit = hyper_params$iter)
}
NNpredict.rminer <- function(object, x, ...)
   as.numeric(rminer::predict(object, newdata=as.data.frame(x)))
NNclose.rminer <- function()
  if("package:rminer" %in% search())
    detach("package:rminer", unload=TRUE)
rminer.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
