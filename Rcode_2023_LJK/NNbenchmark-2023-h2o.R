cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(h2o)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/h2o"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/h2o/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(h2o)
h2o.method <- "first-order"
hyperParams.h2o <- function(optim_method, ...) {
  
  hidden_activation = "Tanh"
  iter <- maxit1storderB
  rate <- 0.01
  stopping_rounds <- 500
  stopping_tolerance <- 1e-5
  distribution <- "gaussian"
  
  out <- list(hidden_activation = hidden_activation, iter = iter, rate=rate, 
              stopping_rounds=stopping_rounds, stopping_tolerance=stopping_tolerance,
              distribution=distribution)
  
  return (out)
}
NNtrain.h2o <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  hidden_activation <- hyper_params$hidden_activation
  iter <- hyper_params$iter
  rate <- hyper_params$rate
  stopping_rounds <- hyper_params$stopping_rounds
  stopping_tolerance <- hyper_params$stopping_tolerance
  distribution <- hyper_params$distribution
  if(class(dataxy) != "H2OFrame")
    dataxy <- h2o::as.h2o(dataxy)
  
  NNreg <-   h2o::h2o.deeplearning(y = "y",
                                   training_frame = dataxy,
                                   overwrite_with_best_model = TRUE, 
                                   standardize = FALSE,
                                   activation = hidden_activation,
                                   adaptive_rate = FALSE,
                                   rate = rate,
                                   hidden = hidden_neur,
                                   epochs = iter,
                                   train_samples_per_iteration = -1,
                                   initial_weight_distribution = "Normal",
                                   initial_weight_scale = 0.1,
                                   loss = "Quadratic",
                                   distribution = distribution,
                                   stopping_rounds = stopping_rounds,
                                   stopping_metric = "RMSE",
                                   stopping_tolerance = stopping_tolerance,
                                   seed = as.integer(runif(1)*10000000),
                                   verbose = FALSE
  )
  return (NNreg)
}
NNpredict.h2o <- function(object, x, ...)
{
  predictions <- h2o::h2o.predict(object, newdata=h2o::as.h2o(x))
  as.data.frame(predictions)$predict
}
NNclose.h2o <- function()
{
  h2o::h2o.shutdown(FALSE)
  if("package:h2o" %in% search())
    detach("package:h2o", unload=TRUE)
}
NNstart.h2o <- function()
{
  require("h2o", character.only = TRUE)
  h2o::h2o.init()
  h2o::h2o.no_progress()
}
h2o.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, h2o.method, "NNtrain.h2o", "hyperParams.h2o", "NNpredict.h2o", 
                                   NNsummary, "NNclose.h2o", "NNstart.h2o", h2o.prepareZZ, nrep=5, echo=FALSE, doplot=FALSE,
                                   pkgname="h2o", pkgfun="deeplearning", rdafile=TRUE, odir=odir)
    
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "h2o", pkgfun = "h2o.deeplearning", h2o.method,
                           prepareZZ.arg = h2o.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
  
)
print(t1)
#}


