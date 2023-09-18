## ----message=FALSE, warning=FALSE------------------------------------------------
library(NNbenchmark)
library(kableExtra)
options(scipen = 999)


## --------------------------------------------------------------------------------
NNdataSummary(NNdatasets)


## --------------------------------------------------------------------------------
if(dir.exists("D:/GSoC2020/Results/2020run04/"))
{  
  odir <- "D:/GSoC2020/Results/2020run04/"
}else if(dir.exists("~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"))
{  
  odir <- "~/Documents/recherche-enseignement/code/R/NNbenchmark-project/NNtempresult/"
}else
  odir <- "~"

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(ANN2)
ANN2.method <- c("sgd", "adam", "rmsprop")
hyperParams.ANN2 <- function(optim_method, ...) {
    
    if (optim_method == "sgd")     { iter <- maxit1storderA; lr <- 0.01} 
    if (optim_method == "adam")    { iter <- maxit1storderA; lr <- 0.01} 
    if (optim_method == "rmsprop") { iter <- maxit1storderA; lr <- 0.01} 
    
    out <- list(iter = iter, lr = lr)
    return(out)
}

NNtrain.ANN2 <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...){
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    
    NNreg <- ANN2::neuralnetwork(X = x, y = y, 
                           val.prop = 0, 
                           standardize = FALSE, 
                           hidden.layers = hidden_neur, 
                           regression = TRUE,
                           loss.type = "squared",
                           n.epochs = iter,
                           optim.type = optim_method,
                           learn.rates = lr,
                           verbose = FALSE,
                           random.seed = as.integer(runif(1)*10000000))
    
   return (NNreg)
}
NNpredict.ANN2 <- function(object, x, ...)
    as.numeric(predict(object, x)$predictions)    
NNclose.ANN2 <- function()
  if("package:ANN2" %in% search())
    detach("package:ANN2", unload=TRUE)
ANN2.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

if(FALSE)
res <- train_and_predict_1data(1, ANN2.method, "NNtrain.ANN2", "hyperParams.ANN2", "NNpredict.ANN2", 
                               NNsummary, "NNclose.ANN2", NA, ANN2.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="ANN2", pkgfun="neuralnetwork", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "ANN2", pkgfun = "neuralnetwork", ANN2.method,
  prepareZZ.arg = ANN2.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "sgd")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "adam")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "rmsprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

