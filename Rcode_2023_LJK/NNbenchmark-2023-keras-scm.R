cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark)
require(keras)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/keras"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/keras/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(keras)
keras.method <- c("adadelta", "adagrad", "adam", "adamax","nadam","rmsprop", "sgd")
hyperParams.keras <- function(optim_method, ...) {
  
  hidden_activation = "tanh"
  iter <- maxit1storderB
  if(optim_method == "adadelta"){
    lr <- 1
  } else{
    lr <- 0.1
  }
  
  out <- list(hidden_activation = hidden_activation, iter = iter, lr = lr)
  return (out)
}

NNtrain.keras <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  iter <- hyper_params$iter
  
  early_stop <- callback_early_stopping(monitor = "loss", patience = 20, restore_best_weights = TRUE, mode = "auto", min_delta = 0.0001)
  #should we have a higher min_delta
  
  hidden_activation <- hyper_params$hidden_activation
  lr <- hyper_params$lr
  
  if (optim_method == "adadelta")  { op <- optimizer_adadelta(lr = lr)}
  if (optim_method == "adagrad")   { op <- optimizer_adagrad(lr = lr)}
  if (optim_method == "adam")      { op <- optimizer_adam(lr = lr)}
  if (optim_method == "adamax")    { op <- optimizer_adamax(lr = lr)}
  if (optim_method == "nadam")     { op <- optimizer_nadam(lr = lr)}
  if (optim_method == "rmsprop")   { op <- optimizer_rmsprop(lr = lr)}
  if (optim_method == "sgd")       { op <- optimizer_sgd(lr = lr)}
  
  model <- keras_model_sequential() %>%
    layer_dense(units = hidden_neur, activation = hidden_activation, input_shape = ncol(x)) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = op,
    metrics = list("mean_absolute_error")
  )
  
  historylog <- model %>% fit(x, y, epochs = iter, verbose = 0, callbacks = list(early_stop))
  
  return (model)
}  

NNpredict.keras <- function(object, x, ...)
{
  object %>% predict(x)
}
NNclose.keras <- function()
{
  if("package:keras" %in% search())
    detach("package:keras", unload=TRUE)
}

keras.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "keras", pkgfun = "fit", keras.method,
                           prepareZZ.arg = keras.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
  
)
print(t1)
if(length(useless) > 0)
{
  print(head(useless))
  print(tail(useless))
}
#}
