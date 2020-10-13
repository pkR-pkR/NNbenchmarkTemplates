validann.method <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
hyperParams.validann <- function(optim_method, ...) {
    
    if (optim_method == "Nelder-Mead")  {iter <- maxit1storderB} 
    if (optim_method == "BFGS")         {iter <- maxit2ndorder}
    if (optim_method == "CG")           {iter <- maxit1storderA}
    if (optim_method == "L-BFGS-B")     {iter <- maxit2ndorder}
    if (optim_method == "SANN")         {iter <- maxit1storderA}
  
    out <- list(iter = iter)
    return (out)
}
NNtrain.validann <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    method <- hyper_params$method
    
    NNreg <- validann::ann(x, y, size = neur, 
                           method = optim_method, maxit = iter)
    
    return (NNreg)
}
NNpredict.validann <- function(object, x, ...)
  predict(object, x)
NNclose.validann <- function()
  if("package:validann" %in% search())
    detach("package:validann", unload=TRUE)
validann.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
