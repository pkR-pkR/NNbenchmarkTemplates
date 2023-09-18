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

