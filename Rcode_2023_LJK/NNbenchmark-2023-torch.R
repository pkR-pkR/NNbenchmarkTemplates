#install.packages("torch")
library(torch)
mRef153

maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000


#see examples in brulee:::linear_reg_fit_imp



hyperParams.torch <- function(optim_method, ...) 
{
  iter <- maxit1storderA
  rate <- switch(optim_method, 
                 "adadelta" = 1e-3,
                 "adagrad" = 1e-2,
                 "adam" = 1e-3,
                 "asgd" = 1e-2,
                 "lbfgs" = 1,
                 "rmsprop" = 1e-2,
                 "rprop" = 1e-2,
                 "sgd" = 1e-2,
                 1e-2)
  #learning rate
  rho <- 0.9 #for adadelta
  betas <- c(0.9, 0.99) #for adam
  out <- list(iter = iter, rate=rate, rho=rho, betas=betas)
  return(out)
}
NNtrain.torch <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) 
{
  hyper_params <- do.call(hyperParams.torch, list(optim_method, ...))
  maxit <- hyper_params$iter
  
  #set model
  mynn <- torch::nn_sequential(
    #layer 1
    torch::nn_linear(ncol(x), hidden_neur),
    #layer 2
    torch::nn_linear(hidden_neur, 1)
  )
  #set optimization algo
  myoptimizer <- switch(optim_method,
    "adadelta" = torch::optim_adadelta(mynn$parameters, lr = hyper_params$rate,
                                rho = hyper_params$rho),
    "adagrad" = torch::optim_adagrad(mynn$parameters, lr = hyper_params$rate),
    "adam" = torch::optim_adam(mynn$parameters, lr = hyper_params$rate,
                        betas = hyper_params$betas),
    "asgd" = torch::optim_asgd(mynn$parameters, lr = hyper_params$rate),
    "lbfgs" = torch::optim_lbfgs(mynn$parameters, lr = hyper_params$rate, max_iter=maxit),
    "rmsprop" = torch::optim_rmsprop(mynn$parameters, lr = hyper_params$rate),
    "rprop" = torch::optim_rprop(mynn$parameters, lr = hyper_params$rate),
    "sgd" = torch::optim_sgd(mynn$parameters, lr = hyper_params$rate)
  )
  #set loss
  myloss <- torch::nn_mse_loss("mean")
  #perform training
  for(i in 1:maxit)
  {
    myoptimizer$zero_grad()
    y_pred <- mynn(x)
    loss <- myloss(y_pred, y)
    loss$backward() #?
    myoptimizer$step()
  }
  return(mynn)
}
    
hypar <- hyperParams.torch("adadelta")
NNtrain.torch(mRef153[, grep("y", colnames(mRef153), invert = TRUE)], mRef153[,"y"],
              NULL, NULL,
              3, "adadelta", hypar)

#https://brulee.tidymodels.org/reference/brulee_linear_reg.html
brulee:::linear_reg_fit_imp(mRef153[, grep("y", colnames(mRef153), invert = TRUE)], mRef153[,"y"])
brulee::brulee_linear_reg(mRef153[, grep("y", colnames(mRef153), invert = TRUE)], mRef153[,"y"])

brulee:::brulee_linear_reg.matrix
brulee:::brulee_linear_reg_bridge
hardhat:::mold.matrix
hardhat:::run_mold.default_xy_blueprint
hardhat:::default_xy_blueprint
hardhat:::new_default_xy_blueprint
hardhat:::new_xy_blueprint
hardhat:::new_blueprint
