cat("Setting before computation\n")
print(.libPaths())
print(getwd())
print(installed.packages()[, c("Package", "Version", "Built")])

require(NNbenchmark)
require(Buddle)

print(sessionInfo())

## --------------------------------------------------------------------------------
#output
odir <- "/home/dutangc/codepourdahu/NNbench/output/"
if(!dir.exists(odir))
  dir.create(odir)
odir <- "/home/dutangc/codepourdahu/NNbench/output/Buddle"
if(!dir.exists(odir))
  dir.create(odir)
odir <- paste0("/home/dutangc/codepourdahu/NNbench/output/Buddle/", Sys.Date())
if(!dir.exists(odir))
  dir.create(odir)

nrep <- 5
maxit2ndorder  <-    200
maxit1storderA <-   1000
maxit1storderB <-  10000
maxit1storderC <- 100000

#library(ANN2)
Buddle.method <- c("SGD", "Momentum", "AdaGrad", "Adam", "Nesterov", "RM-Sprop")
hyperParams.Buddle <- function(optim_method, ...) 
{
    iter <- maxit1storderA
    lr <- 0.01
    init.weight <- 0.1
    out <- list(iter = iter, lr = lr, init.weight=init.weight)
    return(out)
}

NNtrain.Buddle <- function(x, y, dataxy, formula, hidden_neur, optim_method, hyperParams, ...)
{
    
    hyper_params <- do.call(hyperParams, list(optim_method, ...))
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    w <- hyper_params$init.weight
    
    NNreg <- Buddle::TrainBuddle(formula.string = formula,
                           data=dataxy,
                           train.ratio=1,
                           arrange = 0,
                           batch.size = NROW(dataxy)-1,
                           total.iter = iter,
                           hiddenlayer = hidden_neur,
                           init.weight = w,
                           batch.norm = FALSE,
                           lr = lr,
                           drop = FALSE,
                           activation = rep("TanH", length(hidden_neur)),
                           optim = optim_method,
                           type="Regression",
                           rand.eff = FALSE,
                           disp=FALSE)
    
   return (NNreg)
}
NNpredict.Buddle <- function(object, x, ...)
{
  #get appropriate components
  fitW <- object[["lW"]]
  fitb <- object[["lb"]]
  fitpar <- object[["lParam"]]
  
  Buddle::FetchBuddle(x, fitW, fitb, fitpar)$predicted  
}
NNclose.Buddle <- function()
  if("package:Buddle" %in% search())
    detach("package:Buddle", unload=TRUE)
Buddle.prepareZZ <- list(xdmv = "d", ydmv = "m", zdm = "d", scale = TRUE)


## --------------------------------------------------------------------------------
#if(FALSE)
#{
t1 <- system.time(
  useless <- capture.output(res <- trainPredict_1pkg(1:12, pkgname = "Buddle", pkgfun = "TrainBuddle", Buddle.method,
                           prepareZZ.arg = Buddle.prepareZZ, nrep = nrep, doplot = FALSE,
                           csvfile = TRUE, rdafile = TRUE, odir = odir, echo = FALSE))
)
print(t1)
if(length(useless) > 0)
{
  print(head(useless))
  print(tail(useless))
}
#}
