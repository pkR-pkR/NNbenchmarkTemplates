## ----child = 'NNbenchmark-2020-AMORE.Rmd'----------------------------------------

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

#library(AMORE)
hyperParams.AMORE <- function(optim_method, ...) {
    
    if (!is.element(optim_method, c("ADAPTgd", "ADAPTgdwm", "BATCHgd", "BATCHgdwm"))) stop("Invalid Parameters.")
    if (optim_method == "ADAPTgd") {iter <- maxit1storderA; lr <- 0.01; momentum <- 0; hidden_activation <- "tansig"} 
    if (optim_method == "ADAPTgdwm") {iter <- maxit1storderA; lr <- 0.01; momentum <- 0.8; hidden_activation <- "sigmoid"} 
    if (optim_method == "BATCHgd") {iter <- maxit1storderB; lr <- 0.1; momentum <- 0; hidden_activation <- "tansig"}
    if (optim_method == "BATCHgdwm") {iter <- maxit1storderB; lr <- 0.1; momentum <- 0.8; hidden_activation <- "tansig"}

    out <- list(iter = iter, lr = lr, momentum = momentum, hidden_activation = hidden_activation)
    
    return (out)
}

NNtrain.AMORE <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    momentum <- hyper_params$momentum
    hidden_activation <- hyper_params$hidden_activation
    
    net_structure <- AMORE::newff(n.neurons = c(ncol(x), hidden_neur, 1), 
                                  learning.rate.global = lr, 
                                  error.criterium = "LMS", 
                                  hidden.layer = hidden_activation, 
                                  method = optim_method, 
                                  momentum.global = momentum)
    
    NNreg <- AMORE::train(net = net_structure, 
                          P = x, 
                          T = y, 
                          error.criterium = "LMS", 
                          report = FALSE, 
                          n.shows = iter, 
                          show.step = 1)
    return (NNreg)
}
NNpredict.AMORE <- function(object, x, ...)
    AMORE::sim.MLPnet(object$net, x)    
NNclose.AMORE <- function()
  if("package:AMORE" %in% search())
    detach("package:AMORE", unload=TRUE)
AMORE.method <- c("ADAPTgd", "ADAPTgdwm", "BATCHgd", "BATCHgdwm")
AMORE.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)

if(FALSE)
res <- train_and_predict_1data(1, AMORE.method, "NNtrain.AMORE", "hyperParams.AMORE", "NNpredict.AMORE", 
                               NNsummary, "NNclose.AMORE", NA, AMORE.prepareZZ, nrep=2, echo=TRUE, 
                               doplot=FALSE, echoreport=0,
                               pkgname="AMORE", pkgfun="train", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14, fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "AMORE", pkgfun = "train", AMORE.method,
  prepareZZ.arg = AMORE.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "ADAPTgd")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "ADAPTgdwm")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "BATCHgd")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "BATCHgdwm")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-ANN2.Rmd'-----------------------------------------

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



## ----child = 'NNbenchmark-2020-automl.Rmd'---------------------------------------

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

#library(automl)
automl.method <- c("trainwgrad_RMSprop","trainwgrad_adam", "trainwpso") #"trainwgrad_Momentum"
hyperParams.automl <- function(optim_method, ...) {
#  if (optim_method == "trainwgrad_Momentum")     {beta2 <- 0.9; beta2 <- 0   ; iter <- maxit1storderA; lr = 0.01; optim_method = "trainwgrad"}
  if (optim_method == "trainwgrad_RMSprop")      {beta1 <- 0  ; beta2 <- 0.99; iter <- maxit1storderA; lr = 0.01; optim_method = "trainwgrad"}
  if (optim_method == "trainwgrad_adam")         {beta1 <- 0.9; beta2 <- 0.99; iter <- maxit1storderA; lr = 0.01; optim_method = "trainwgrad"}
  if (optim_method == "trainwpso")               {beta1 <- NULL; beta2 <- NULL; iter <- maxit1storderA; lr = NULL}
  
  return(list(beta1 = beta1, beta2 = beta2, iter = iter, lr = lr, hidden_activation = 'tanh', optim_method = optim_method))
}
NNtrain.automl <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
  
  hyper_params <- do.call(hyperParams, list(optim_method, ...))
  
  beta1 <- hyper_params$beta1 ; beta2 <- hyper_params$beta2
  iter <- hyper_params$iter ; lr <- hyper_params$lr
  hidden_activation <- hyper_params$hidden_activation
  
  # NNreg <- automl::automl_train(Xref = x, Yref = y)
  
  NNreg <- automl::automl_train_manual(Xref = x, Yref = y,
                                       hpar = list(modexec = hyper_params$optim_method,
                                                   beta1 = beta1, beta2 = beta2,
                                                   numiterations = iter, learningrate = lr,
                                                   layersshape = c(hidden_neur, 0),
                                                   layersacttype = c(hidden_activation, ""),
                                                   verbose = FALSE,
                                                   seed = as.integer(runif(1)*10000000)))

    return (NNreg)
}
NNpredict.automl <- function(object, x, ...)
    automl::automl_predict(model=object, X=x)
NNclose.automl <- function()
  if("package:automl" %in% search())
    detach("package:automl", unload=TRUE)
automl.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

if(FALSE)
res <- train_and_predict_1data(1, automl.method, "NNtrain.automl", "hyperParams.automl", "NNpredict.automl", 
                               NNsummary, "NNclose.automl", NA, automl.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="automl", pkgfun="automl_train_manual", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "automl", pkgfun = "automl_train_manual", automl.method,
  prepareZZ.arg = automl.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "trainwgrad_RMSprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "trainwgrad_adam")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "trainwpso")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-brnn.Rmd'-----------------------------------------

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

#library(brnn)
brnn.method <- "Gauss-Newton"
hyperParams.brnn <- function(optim_method, ...) {
    return(list(iter = maxit2ndorder))
}
NNtrain.brnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams.brnn, list(brnn.method))
    iter <- hyper_params$iter
    
    NNreg <- brnn::brnn(x, y, neur, normalize = FALSE, epochs = iter, verbose = FALSE)
    return (NNreg)
}
NNpredict.brnn <- function(object, x, ...)
    predict(object, x)
NNclose.brnn <- function()
  if("package:brnn" %in% search())
    detach("package:brnn", unload=TRUE)
brnn.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, brnn.method, "NNtrain.brnn", "hyperParams.brnn", "NNpredict.brnn", 
                               NNsummary, "NNclose.brnn", NA, brnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="brnn", pkgfun="brnn", csvfile=TRUE, rdafile=TRUE, odir=odir)
if(FALSE)
res <- trainPredict_1mth1data(1, brnn.method[1], "NNtrain.brnn", "hyperParams.brnn", "NNpredict.brnn", 
                               NNsummary, brnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="brnn", pkgfun="brnn", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "brnn", pkgfun = "brnn", brnn.method,
  prepareZZ.arg = brnn.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-CaDENCE.Rmd'--------------------------------------

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

#library(CaDENCE)
CaDENCE.method <- c("optim", "psoptim", "Rprop")
hyperParams.CaDENCE <- function(optim_method, ...) {
  
    if (optim_method == "optim")    {iter <- maxit2ndorder;  epsilon <- 0.01} 
    if (optim_method == "psoptim")  {iter <- maxit1storderA; epsilon <- 0.01}
    if (optim_method == "Rprop")    {iter <- maxit1storderA; epsilon <- 0.01}
    
    out <- list(iter = iter, epsilon = epsilon)
    return (out)
}

NNtrain.CaDENCE <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    iter <- hyper_params$iter ; epsilon <- hyper_params$epsilon

    NNreg <- CaDENCE::cadence.fit(x = x, y = y, 
                                iter.max = iter, 
                                n.hidden = neur, 
                                hidden.fcn = tanh, 
                                method = optim_method, 
                                n.trials = 1, 
                                trace = 0, 
                                maxit.Nelder = 1, 
                                f.cost = cadence.cost,
                                distribution = list(density.fcn = dnorm,
                                                    parameters = c("mean", "sd"),
                                                    parameters.fixed = NULL,
                                                    output.fcns = c(identity, exp)),
                                epsilon = epsilon)
    return(NNreg)
}
NNpredict.CaDENCE <- function(object, x, ...)
    CaDENCE::cadence.predict(x = x, fit = object)[,1]
NNclose.CaDENCE <- function()
  if("package:CaDENCE" %in% search())
    detach("package:CaDENCE", unload=TRUE)
CaDENCE.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)

if(FALSE)
res <- trainPredict_1data(1, CaDENCE.method, "NNtrain.CaDENCE", "hyperParams.CaDENCE", "NNpredict.CaDENCE", 
                               NNsummary, "NNclose.CaDENCE", NA, CaDENCE.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="CaDENCE", pkgfun="cadence.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "CaDENCE", pkgfun = "cadence.fit", CaDENCE.method,
  prepareZZ.arg = CaDENCE.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "optim")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "psoptim")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "Rprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-caret.Rmd'----------------------------------------

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

#library(caret)
caret.method <- "none"
hyperParams.caret <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.caret <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    NNreg <- caret::avNNet(x, y, repeats = 3, size = neur, linout = TRUE, trace=hyper_params$trace, bag = TRUE, maxit = maxit2ndorder)
    return(NNreg)
}
NNpredict.caret <- function(object, x, ...)
    predict(object, newdata=x)
NNclose.caret <- function()
  if("package:caret" %in% search())
    detach("package:caret", unload=TRUE)
caret.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, caret.method, "NNtrain.caret", "hyperParams.caret", "NNpredict.caret", 
                               NNsummary, "NNclose.caret", NA, caret.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="caret", pkgfun="caret", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "caret", pkgfun = "avNNet", caret.method,
  prepareZZ.arg = caret.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-deepdive.Rmd'-------------------------------------

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

#library(deepdive)
deepdive.method <- c("gradientDescent","momentum","rmsProp","adam")
hyperParams.deepdive <- function(optim_method, ...) {
    
    if (optim_method == "gradientDescent")  {maxiter <- maxit1storderB; eta = 0.4} 
    if (optim_method == "momentum")         {maxiter <- maxit1storderB; eta = 0.8}
    if (optim_method == "rmsProp")          {maxiter <- maxit1storderA; eta = 0.8}
    if (optim_method == "adam")             {maxiter <- maxit1storderA; eta = 0.8}
  
    out <- list(iter = maxiter, method = optim_method, modelType = "regress", eta = eta, print = 1000)
    return (out)
}
NNtrain.deepdive <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, NNfullformula, NNparam,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    NNreg <- deepdive::deepnet(x = x, y = y, c(neur), modelType = hyper_params$modelType,
                     iterations = hyper_params$iter, eta=hyper_params$eta, 
                     optimiser=hyper_params$method, printItrSize=hyper_params$print,
                     normalise = FALSE)
    
    return (NNreg)
}
NNpredict.deepdive <- function(object, x, ...)
  predict.deepnet(object,newData=x)$ypred
NNclose.deepdive <- function()
  if("package:deepdive" %in% search())
    detach("package:deepdive", unload=TRUE)
deepdive.prepareZZ <- list(xdmv = "d", ydmv = "d", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, deepdive.method, "NNtrain.deepdive", "hyperParams.deepdive", "NNpredict.deepdive", 
                               NNsummary, "NNclose.deepdive", NA, deepdive.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="deepdive", pkgfun="ann", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "deepdive", pkgfun = "deepnet", deepdive.method,
  prepareZZ.arg = deepdive.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
kable(apply(res[,,1,], c(3,1), min), caption = "gradientDescent")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "momentum")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "rmsProp")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "adam")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-deepnet.Rmd'--------------------------------------

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

#library(deepnet)
deepnet.method <- "BP"
hyperParams.deepnet <- function(optim_method, ...) {

    iter <- maxit1storderA
    lr <- 0.8
    dropout <- 0
    momentum <- 0.95
    hidden_activation <- "sigm"

    out <- list(iter = iter, lr = lr, momentum = momentum, hidden_activation = hidden_activation, dropout = dropout)
    
    return (out)
}

NNtrain.deepnet <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    dropout <- hyper_params$dropout
    momentum <- hyper_params$momentum
    hidden_activation <- hyper_params$hidden_activation
    dropout <- hyper_params$dropout

    NNreg <- deepnet::nn.train(x = x, y = y, 
                               hidden = c(hidden_neur), 
                               activationfun = hidden_activation, 
                               learningrate = lr, 
                               output = 'linear', 
                               numepochs = iter, 
                               hidden_dropout = dropout, 
                               momentum = momentum)
    return (NNreg)
}
NNpredict.deepnet <- function(object, x, ...)
    deepnet::nn.predict(nn = object, x = x)
NNclose.deepnet <- function()
  if("package:deepnet" %in% search())
    detach("package:deepnet", unload=TRUE)
deepnet.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- train_and_predict_1data(1, CaDENCE.method, "NNtrain.deepnet", "hyperParams.deepnet", "NNpredict.deepnet", 
                               NNsummary, "NNclose.deepnet", NA, deepnet.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="deepnet", pkgfun="nn.train", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "deepnet", pkgfun = "nn.train", deepnet.method,
  prepareZZ.arg = deepnet.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-elmNNRcpp.Rmd'------------------------------------

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
if(FALSE)
res <- train_and_predict_1data(1, elmNNRcpp.method, "NNtrain.elmNNRcpp", "hyperParams.elmNNRcpp", "NNpredict.elmNNRcpp", 
                               NNsummary, "NNclose.elmNNRcpp", NA, elmNNRcpp.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="elmNNRcpp", pkgfun="elm_train", rdafile=TRUE, odir=odir)




## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "elmNNRcpp", pkgfun = "elm_train", elmNNRcpp.method,
  prepareZZ.arg = elmNNRcpp.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-ELMR.Rmd'-----------------------------------------

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

ELMR.method <- "extremeML"
hyperParams.ELMR <- function(optim_method, ...) {

    hidden_activation <- "sig"
    size_first_block <- 50
    size_each_chunk <- 50
    
    out <- list(hidden_activation = hidden_activation,
                size_first_block=size_first_block, size_each_chunk=size_each_chunk)
    return (out)
}
NNtrain.ELMR <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    hidden_activation <- hyper_params$hidden_activation
    size_each_chunk <- hyper_params$size_each_chunk
    size_first_block <- hyper_params$size_first_block
    
    #OSelm_train.formula() call OSelm_training()
    NNreg <- ELMR::OSelm_train.formula(formula = formula,
                                       data = dataxy,
                                       Elm_type = "regression",
                                       nHiddenNeurons = hidden_neur,
                                       ActivationFunction = hidden_activation,
                                       N0 = size_first_block, Block = size_each_chunk)

    return (NNreg)
}
NNpredict.ELMR <- function(object, x, xy)
    ELMR::predict_elm(model = object, test = xy)$predicted
NNclose.ELMR <- function()
  if("package:ELMR" %in% search())
    detach("package:ELMR", unload=TRUE)
ELMR.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- train_and_predict_1data(1, ELMR.method, "NNtrain.ELMR", "hyperParams.ELMR", "NNpredict.ELMR", 
                               NNsummary, "NNclose.ELMR", NA, ELMR.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="ELMR", pkgfun="OSelm_train", rdafile=TRUE, odir=odir)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "ELMR", pkgfun = "OSelm_train.formula", ELMR.method,
  prepareZZ.arg = ELMR.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-EnsembleBase.Rmd'---------------------------------

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

#library(EnsembleBase)
EnsembleBase.method <- "none"
hyperParams.EnsembleBase <- function(optim_method, ...) {
    out <- list(iter = maxit2ndorder, decay = 0)
    return (out)
}
NNtrain.EnsembleBase <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams.EnsembleBase, list(EnsembleBase.method))
    
    iter <- hyper_params$iter ; decay <- hyper_params$decay
    
    NNreg <- EnsembleBase::Regression.Batch.Fit(make.configs("nnet", config.df = expand.grid(decay=decay,size=c(neur),maxit=iter)), formula, dataxy, ncores = 1)
    
    return (NNreg)
}
NNpredict.EnsembleBase <- function(object, x, ...)
    predict(object, x)
NNclose.EnsembleBase <- function()
  if("package:EnsembleBase" %in% search())
    detach("package:EnsembleBase", unload=TRUE)
EnsembleBase.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, EnsembleBase.method, "NNtrain.EnsembleBase", "hyperParams.EnsembleBase", "NNpredict.EnsembleBase", 
                               NNsummary, "NNclose.EnsembleBase", NA, EnsembleBase.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="EnsembleBase", pkgfun="EnsembleBase", csvfile=TRUE, rdafile=TRUE, odir=odir)
if(FALSE)
res <- trainPredict_1mth1data(1, EnsembleBase.method[1], "NNtrain.EnsembleBase", "hyperParams.EnsembleBase", "NNpredict.EnsembleBase", 
                               NNsummary, EnsembleBase.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="EnsembleBase", pkgfun="EnsembleBase", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "EnsembleBase", pkgfun = "Regression.Batch.Fit", EnsembleBase.method,
  prepareZZ.arg = EnsembleBase.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-h2o.Rmd'------------------------------------------

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

if(FALSE)
res <- train_and_predict_1data(1, h2o.method, "NNtrain.h2o", "hyperParams.h2o", "NNpredict.h2o", 
                               NNsummary, "NNclose.h2o", "NNstart.h2o", h2o.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="h2o", pkgfun="deeplearning", rdafile=TRUE, odir=odir)




## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "h2o", pkgfun = "h2o.deeplearning", h2o.method,
  prepareZZ.arg = h2o.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-keras.Rmd'----------------------------------------

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

if(FALSE)
res <- train_and_predict_1data(1, keras.method, "NNtrain.keras", "hyperParams.keras", "NNpredict.keras", 
                               NNsummary, "NNclose.keras", NA, keras.prepareZZ, nrep=2, echo=TRUE, doplot=FALSE,
                               pkgname="keras", pkgfun="fit", rdafile=TRUE, odir=odir, echoreport=2)




## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "keras", pkgfun = "fit", keras.method,
  prepareZZ.arg = keras.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "adadelta")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "adagrad")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "adam")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "adamax")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,5,], c(3,1), min), caption = "nadam")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,6,], c(3,1), min), caption = "rmsprop")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,7,], c(3,1), min), caption = "sgd")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))




## ----child = 'NNbenchmark-2020-MachineShop.Rmd'----------------------------------

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

#library(MachineShop)
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
if(FALSE)
res <- trainPredict_1data(1, MachineShop.method, "NNtrain.MachineShop", "hyperParams.MachineShop", "NNpredict.MachineShop", 
                               NNsummary, "NNclose.MachineShop", NA, MachineShop.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="MachineShop", pkgfun="fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "MachineShop", pkgfun = "fit", MachineShop.method,
  prepareZZ.arg = MachineShop.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-minpack.lm.Rmd'-----------------------------------

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

#library(minpack.lm)
minpack.lm.method <- "none"
hyperParams.minpack.lm <- function(...) {
    return (list(iter=maxit2ndorder))
}
NNtrain.minpack.lm <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))

    minpack.lm::nlsLM(NNfullformula, data = dataxy,
                      control = list(maxiter = hyper_params$iter))
}
NNpredict.minpack.lm <- function(object, x, ...)
  predict(object, newdata=as.data.frame(x))
NNclose.minpack.lm <- function()
  if("package:minpack.lm" %in% search())
    detach("package:minpack.lm", unload=TRUE)
minpack.lm.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, minpack.lm.method, "NNtrain.minpack.lm", "hyperParams.minpack.lm", "NNpredict.minpack.lm", 
                               NNsummary, "NNclose.minpack.lm", NA, minpack.lm.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="minpack.lm", pkgfun="nlsLM", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "minpack.lm", pkgfun = "nlsLM", minpack.lm.method,
  prepareZZ.arg = minpack.lm.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-monmlp.Rmd'---------------------------------------

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

#library(monmlp)
monmlp.method <- c("BFGS", "Nelder-Mead")
hyperParams.monmlp <- function(optim_method, ...) {
    if (optim_method == "BFGS")         {iter <- maxit2ndorder}
    if (optim_method == "Nelder-Mead")  {iter <- maxit1storderB} 
    return (list(iter=iter, silent=TRUE, scale=TRUE))
}
NNtrain.monmlp <- function(x, y, dataxy, formula, neur, optim_method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    iter <- hyper_params$iter
    scale <- hyper_params$scale
    silent <- hyper_params$silent
    
    monmlp::monmlp.fit(x, y, hidden1 = neur, scale.y = scale, silent=silent,
                         method = optim_method, iter.max = iter)
}
NNpredict.monmlp <- function(object, x, ...)
  as.numeric(monmlp::monmlp.predict(x, weights=object))
NNclose.monmlp <- function()
  if("package:monmlp" %in% search())
    detach("package:monmlp", unload=TRUE)
monmlp.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, monmlp.method, "NNtrain.monmlp", "hyperParams.monmlp", "NNpredict.monmlp", 
                               NNsummary, "NNclose.monmlp", NA, monmlp.prepareZZ, nrep=2,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="monmlp", pkgfun="monmlp.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "monmlp", pkgfun = "monmlp.fit", monmlp.method,
  prepareZZ.arg = monmlp.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = TRUE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "BFGS")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "Nelder-Mead")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))




## ----child = 'NNbenchmark-2020-neuralnet.Rmd'------------------------------------

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



## ----child = 'NNbenchmark-2020-nlsr.Rmd'-----------------------------------------

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

#library(nlsr)
nlsr.method <- "none"
hyperParams.nlsr <- function(...) {
    return (list(iter = maxit2ndorder, sdnormstart = 0.1))
}
NNtrain.nlsr <- function(x, y, dataxy, formula, neur, method, hyperParams, NNfullformula, NNparam, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    start <- round(rnorm(NNparam, sd = hyper_params$sdnormstart), 4)
    names(start)  <- paste0("b", 1:NNparam)
    
    NNreg <- nlsr::nlxb(NNfullformula, data = dataxy, start = start, control = list(femax = hyper_params$iter))
    return(NNreg)
}
NNpredict.nlsr <- function(object, x, ...)
  as.numeric(predict(object, x))
NNclose.nlsr <- function()
  if("package:nlsr" %in% search())
    detach("package:nlsr", unload=TRUE)
nlsr.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, nlsr.method, "NNtrain.nlsr", "hyperParams.nlsr", "NNpredict.nlsr", 
                               NNsummary, "NNclose.nlsr", NA, nlsr.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="nlsr", pkgfun="nlxb", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "nlsr", pkgfun = "nlxb", nlsr.method,
  prepareZZ.arg = nlsr.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-nnet.Rmd'-----------------------------------------

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

#library(nnet)
nnet.method <- "none"
hyperParams.nnet <- function(...) {
    return (list(iter=maxit2ndorder, trace=FALSE))
}
NNtrain.nnet <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    NNreg <- nnet::nnet(x, y, size = neur, linout = TRUE, maxit = hyper_params$iter, trace=hyper_params$trace)
    return(NNreg)
}
NNpredict.nnet <- function(object, x, ...)
    predict(object, newdata=x)
NNclose.nnet <- function()
  if("package:nnet" %in% search())
    detach("package:nnet", unload=TRUE)
nnet.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, nnet.method, "NNtrain.nnet", "hyperParams.nnet", "NNpredict.nnet", 
                               NNsummary, "NNclose.nnet", NA, nnet.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="nnet", pkgfun="nnet", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "nnet", pkgfun = "nnet", nnet.method,
  prepareZZ.arg = nnet.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-qrnn.Rmd'-----------------------------------------

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

#library(qrnn)
qrnn.method <- "none"
hyperParams.qrnn <- function(optim_method, ...) {
    
    iter <- maxit2ndorder
    init.range = c(-0.1, 0.1, -0.1, 0.1)
  
    out <- list(iter = iter, init.range=init.range)
    return (out)
}
NNtrain.qrnn <- function(x, y, dataxy, formula, neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    NNreg <- qrnn::qrnn.fit(x, y, n.hidden = neur, 
                     iter.max = hyper_params$iter, n.trials = 1,
                     init.range = hyper_params$init.range, trace=FALSE)
    
    return (NNreg)
}
NNpredict.qrnn <- function(object, x, ...)
  qrnn::qrnn.predict(x, object)
NNclose.qrnn <- function()
  if("package:qrnn" %in% search())
    detach("package:qrnn", unload=TRUE)
qrnn.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, qrnn.method, "NNtrain.qrnn", "hyperParams.qrnn", "NNpredict.qrnn", 
                               NNsummary, "NNclose.qrnn", NA, qrnn.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="qrnn", pkgfun="qrnn.fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "qrnn", pkgfun = "qrnn.fit", qrnn.method,
  prepareZZ.arg = qrnn.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-radiant.model.Rmd'--------------------------------

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

#library(radiant.model)
radiant.model.method <- "none"
hyperParams.radiant.model <- function(...) {
    return (list(iter=maxit2ndorder, type="regression", decay=0))
}
NNtrain.radiant.model <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
    
    hyper_params <- do.call(hyperParams, list(...))
    
    radiant.model::nn(dataxy, rvar = "y", evar = attr(terms(formula), "term.labels"),
                      type = hyper_params$type, size = neur, maxit = hyper_params$iter,
                      decay = hyper_params$decay)
    
}
NNpredict.radiant.model <- function(object, x, ...)
   predict(object, pred_data=as.data.frame(x))$Prediction
NNclose.radiant.model <- function()
  if("package:radiant.model" %in% search())
    detach("package:radiant.model", unload=TRUE)
radiant.model.prepareZZ <- list(xdmv = "m", ydmv = "m", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, radiant.model.method, "NNtrain.radiant.model", "hyperParams.radiant.model", "NNpredict.radiant.model", 
                               NNsummary, "NNclose.radiant.model", NA, radiant.model.prepareZZ, nrep=5,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="radiant.model", pkgfun="nn", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "radiant.model", pkgfun = "nn", radiant.model.method,
  prepareZZ.arg = radiant.model.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-rminer.Rmd'---------------------------------------

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

#library(rminer)
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
if(FALSE)
res <- trainPredict_1data(1, rminer.method, "NNtrain.rminer", "hyperParams.rminer", "NNpredict.rminer", 
                               NNsummary, "NNclose.rminer", NA, rminer.prepareZZ, nrep=2,
                               echo=TRUE, doplot=FALSE, echoreport=0,
                               pkgname="rminer", pkgfun="fit", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "rminer", pkgfun = "fit", rminer.method,
  prepareZZ.arg = rminer.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-RSNNS.Rmd'----------------------------------------

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



## ----child = 'NNbenchmark-2020-snnR.Rmd'-----------------------------------------

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

snnR.method <- "none"
hyperParams.snnR <- function(optim_method, ...) {
    out <- list(iter = maxit2ndorder)
    return (out)
}
NNtrain.snnR <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams,...) {
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    NNreg <- snnR::snnR(x, y, nHidden = as.matrix(hidden_neur), 
                 iteramax = hyper_params$iter, verbose=FALSE)
    return (NNreg)
}
NNpredict.snnR <- function(object, x, ...)
  predict(object, x)
NNclose.snnR <- function()
  if("package:snnR" %in% search())
    detach("package:snnR", unload=TRUE)
snnR.prepareZZ <- list(xdmv = "m", ydmv = "v", zdm = "d", scale = TRUE)


if(FALSE)
res <- train_and_predict_1data(1, simpleNeural.method, "NNtrain.snnR", "hyperParams.snnR", "NNpredict.snnR", 
                               NNsummary, "NNclose.snnR", NA, simpleNeural.prepareZZ, nrep=5, echo=TRUE, doplot=TRUE,
                               pkgname="snnR", pkgfun="snnR", rdafile=TRUE, odir=odir, echoreport=1)



## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "snnR", pkgfun = "snnR", snnR.method,
  prepareZZ.arg = snnR.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-traineR.Rmd'--------------------------------------

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

#library(traineR)
traineR.method <- "none"
hyperParams.traineR <- function(...) {
    return (list(iter=maxit2ndorder))
}
NNtrain.traineR <- function(x, y, dataxy, formula, neur, method, hyperParams, ...) {
  hyper_params <- do.call(hyperParams, list(...))
  NNreg <- traineR::train.nnet(formula = formula, data = dataxy, size = neur, maxit = hyper_params$iter, linout = TRUE)
  return(NNreg)
}
NNpredict.traineR <- function(object, x, dataxy, ...){
  object$fitted.values 
}
NNclose.traineR <- function()
  if("package:traineR" %in% search())
    detach("package:traineR", unload=TRUE)
traineR.prepareZZ <- list(xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
if(FALSE)
res <- trainPredict_1data(1, traineR.method, "NNtrain.traineR", "hyperParams.traineR", "NNpredict.traineR", 
                               NNsummary, "NNclose.traineR", NA, traineR.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="traineR", pkgfun="traineR", csvfile=TRUE, rdafile=TRUE, odir=odir)
    


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "traineR", pkgfun = "train.nnet", traineR.method,
  prepareZZ.arg = traineR.prepareZZ, nrep = nrep, doplot = TRUE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)



## --------------------------------------------------------------------------------
#print(res)
kable(t(apply(res, c(1,4), min)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(t(apply(res, c(1,4), median)))%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



## ----child = 'NNbenchmark-2020-validann.Rmd'-------------------------------------

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

#library(validann)
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
if(FALSE)
res <- trainPredict_1data(1, validann.method, "NNtrain.validann", "hyperParams.validann", "NNpredict.validann", 
                               NNsummary, "NNclose.validann", NA, validann.prepareZZ, nrep=5, echo=TRUE, doplot=FALSE,
                               pkgname="validann", pkgfun="ann", csvfile=TRUE, rdafile=TRUE, odir=odir)


## ---- message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14----
res <- trainPredict_1pkg(1:12, pkgname = "validann", pkgfun = "ann", validann.method,
  prepareZZ.arg = validann.prepareZZ, nrep = nrep, doplot = FALSE,
  csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE)


## --------------------------------------------------------------------------------
#print(res)
kable(apply(res[,,1,], c(3,1), min), caption = "Nelder-Mead")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,2,], c(3,1), min), caption = "BFGS")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,3,], c(3,1), min), caption = "CG")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,4,], c(3,1), min), caption = "L-BFGS-B")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
kable(apply(res[,,5,], c(3,1), min), caption = "SANN")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


