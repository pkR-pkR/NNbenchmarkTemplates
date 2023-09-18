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

#library(RSNNS)
RSNNS.method <- c("Rprop","BackpropBatch","BackpropChunk","BackpropMomentum",
                  "BackpropWeightDecay","Quickprop","SCG","Std_Backpropagation")
hyperParams.RSNNS <- function(optim_method, ...) {
    
    if (optim_method == "Std_Backpropagation")   {iter <- maxit1storderA} 
    if (optim_method == "BackpropBatch")         {iter <- maxit1storderB}
    if (optim_method == "BackpropChunk")         {iter <- maxit1storderA}
    if (optim_method == "BackpropMomentum")      {iter <- maxit1storderA}
    if (optim_method == "BackpropWeightDecay")   {iter <- maxit1storderA}
    if (optim_method == "Rprop")                 {iter <- maxit1storderA}
    if (optim_method == "Quickprop")             {iter <- maxit1storderB}
    if (optim_method == "SCG")                   {iter <- maxit1storderA}

    out <- list(iter = iter)
    return(out)
}
NNtrain.RSNNS <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    if (optim_method == "Std_Backpropagation" || optim_method == "BackpropBatch"){
          NNreg <- RSNNS::mlp(x, y, size = hidden_neur, learnFunc = optim_method, learnFuncParams = c(0.1), #lr 
                        maxit = hyper_params$iter, linOut = TRUE)
    } else {
          NNreg <- RSNNS::mlp(x, y, size = hidden_neur, learnFunc = optim_method,
                        maxit = hyper_params$iter, linOut = TRUE)
    }
    
    return (NNreg)
}
NNpredict.RSNNS <- function(object, x, ...)
  predict(object, x)
NNclose.RSNNS <- function()
  if("package:RSNNS" %in% search())
    detach("package:RSNNS", unload=TRUE)
RSNNS.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)


if(FALSE)
res <- train_and_predict_1data(1, RSNNS.method, "NNtrain.RSNNS", "hyperParams.RSNNS", "NNpredict.RSNNS", 
                               NNsummary, "NNclose.RSNNS", NA, RSNNS.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="RSNNS", pkgfun="mlp", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "RSNNS", pkgfun = "mlp", RSNNS.method,
  prepareZZ.arg = RSNNS.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "Rprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "BackpropBatch")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "BackpropChunk")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "BackpropMomentum")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,5,], c(3,1), min), caption = "BackpropWeightDecay")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,6,], c(3,1), min), caption = "Quickprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,7,], c(3,1), min), caption = "SCG")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,8,], c(3,1), min), caption = "Std_Backpropagation")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

