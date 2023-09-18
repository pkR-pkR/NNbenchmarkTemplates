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

#library(neuralnet)
neuralnet.method <- c("slr", "sag", "rprop-", "rprop+", "backprop")
hyperParams.neuralnet <- function(method, ...) {
    return(list(iter=maxit1storderC, threshold=0.2, linear.output=TRUE))
}
NNtrain.neuralnet <- function(x, y, dataxy, formula, hidden_neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(method, ...))
    
    if(method == "backprop") {learningrate <- 0.001} else {learningrate <- NULL}
    
    neuralnet::neuralnet(formula = formula, data = dataxy, hidden = hidden_neur, algorithm=method,
                        threshold=hyper_params$threshold, linear.output=hyper_params$linear.output,
						            stepmax = hyper_params$iter, startweights = NULL, act.fct = "tanh",
						            learningrate = learningrate)
}
NNpredict.neuralnet <- function(object, x, ...)
  as.numeric(predict(object, newdata = x))

NNclose.neuralnet <- function()
  if("package:neuralnet" %in% search())
    detach("package:neuralnet", unload=TRUE)
neuralnet.prepareZZ <- list(xdmv = "d", ydmv = "d", zdm = "d", scale = TRUE)

if(FALSE)
res <- train_and_predict_1data(1, neuralnet.method, "NNtrain.neuralnet", "hyperParams.neuralnet", "NNpredict.neuralnet", 
                               NNsummary, "NNclose.neuralnet", NA, neuralnet.prepareZZ, nrep=2,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="neuralnet", pkgfun="neuralnet", rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "neuralnet", pkgfun = "neuralnet", neuralnet.method,
  prepareZZ.arg = neuralnet.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "slr")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "sag")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "rprop-")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "rprop+")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,5,], c(3,1), min), caption = "backprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

