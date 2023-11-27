cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "LibPath", "Version", "Built")])

require(NNbenchmark, lib.loc = "/home/dutangc/Rpersolib")
require(elmNNRcpp, lib.loc = "/home/dutangc/Rpersolib")

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/elmNNRcpp"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/elmNNRcpp/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(elmNNRcpp)
elmNNRcpp.method <- "extremeML"
hyperParams.elmNNRcpp <- function(optim_method, ...) {
  
  moorep_pseudoinv_tol <- 0.01
  wt_init <- "normal_gaussian"
  hidden_activation <- "tansig"
  
  out <- list(wt_init = wt_init, hidden_activation = hidden_activation, 
              moorep_pseudoinv_tol=moorep_pseudoinv_tol) 
  return (out)
}
NNtrain.elmNNRcpp <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  wt_init <- hyper_params$wt_init
  hidden_activation <- hyper_params$hidden_activation
  moorep_pseudoinv_tol <- hyper_params$moorep_pseudoinv_tol
  
  NNreg <- elmNNRcpp::elm_train(x, y, 
                                nhid = hidden_neur, 
                                actfun=hidden_activation, 
                                init_weights = wt_init, 
                                bias = TRUE, 
                                moorep_pseudoinv_tol = moorep_pseudoinv_tol, 
                                verbose = FALSE,
                                seed = as.integer(runif(1)*10000000))
  return (NNreg)
}
NNpredict.elmNNRcpp <- function(object, x, ...)
  elmNNRcpp::elm_predict(elm_train_object = object, newdata = x, normalize = FALSE)
NNclose.elmNNRcpp <- function()
  if("package:elmNNRcpp" %in% search())
    detach("package:elmNNRcpp", unload=TRUE)
elmNNRcpp.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

## --------------------------------------------------------------------------------

if(FALSE)
{
  t1 <- system.time(
    res <- train_and_predict_1data(1, elmNNRcpp.method, "NNtrain.elmNNRcpp", "hyperParams.elmNNRcpp", "NNpredict.elmNNRcpp", 
                                   NNsummary, "NNclose.elmNNRcpp", NA, elmNNRcpp.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                                   pkgname="elmNNRcpp", pkgfun="elm_train", rdafile=TRUE, odir=odir)
  )
  print(t1)
}

## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  res <- trainPredict_1pkg(1:12, pkgname = "elmNNRcpp", pkgfun = "elm_train", elmNNRcpp.method,
                           prepareZZ.arg = elmNNRcpp.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)
)
print(t1)
#}


