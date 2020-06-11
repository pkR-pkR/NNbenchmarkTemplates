

## ===================================================================
## 2020-06-05 NNbenchmark TEMPLATE FOR CaDENCE + BFGS
##            Authors: PATRICE KIENER + SALSABILA MAHDI + AKSHAJ VERMA
##            (REQUIRES at least NNbenchmark_2.2)
## ===================================================================
library(NNbenchmark)
options(scipen = 9999)
options("digits.secs" = 3)


## ===========================================
## SELECT THE PACKAGE USED FOR THE TRAINING
## SOME PACKAGES ISSUE WARNINGS: ACCEPT OR NOT
## ===========================================
library(CaDENCE)
options(warn = 0)  # warnings are printed (default)
# options(warn = -1) # warnings are not printed


## =====================================================
## SET YOUR WORKING DIRECTORY (CSV FILES ARE SAVED HERE)
## COMMENT pdf() FOR A STANDARD PLOT
## UNCOMMENT pdf() TO RECORD ALL PLOTS IN A PDF FILE
## =====================================================
setwd("D:/GSoC2020/Results/2019run03") ; getwd()
# setwd("D:/WindowsDir") ; getwd()
# setwd("~/LinuxDir") ; getwd()

rm(list=ls(all=TRUE))
# pdf()


## ====================================================================
## AVAILABLE DATASETS IN NNbenchmark (u = UNIVARIATE, m = MULTIVARIATE)
## ====================================================================
NNdataSummary(NNdatasets)
names(NNdatasets)
# [1] "mDette"    "mFriedman" "mIshigami" "mRef153"   "uDmod1"    "uDmod2"   
# [7] "uDreyfus1" "uDreyfus2" "uGauss1"   "uGauss2"   "uGauss3"   "uNeuroOne"


## ===============================================================
## SELECT ONE DATASET OR UNCOMMENT THE LOOP TO RUN ALL DATASETS
## IF THE LOOP IS ACTIVATED, YOU CAN RUN THIS FULL PAGE IN EXTENSO
## ===============================================================
# dset   <- "uDmod1"
for (dset in names(NNdatasets)) {
  
  
  ## AUTO
  ds     <- NNdatasets[[dset]]$ds
  Z      <- NNdatasets[[dset]]$Z
  neur   <- NNdatasets[[dset]]$neur
  nparNN <- NNdatasets[[dset]]$nparNN
  fmlaNN <- NNdatasets[[dset]]$fmlaNN
  donotremove  <- c("dset", "dsets", "ds", "Z", "neur", "TF", "nruns", "timer",
                    "donotremove", "donotremove2")
  donotremove2 <- c("dset", "dsets") 
  
  
  
  ## ===================================================
  ## SELECT THE FORMAT REQUIRED BY THE PACKAGE/ALGORITHM
  ## d = data.frame, m = matrix, v = vector/numeric
  ## ATTACH THE OBJECTS CREATED (x, y, Zxy, ... )
  ## ===================================================
  ZZ <- prepareZZ(Z, xdmv = "m", ydmv = "m", scale = TRUE)
  attach(ZZ)
  # ls(ZZ)
  # ls()
  
  
  
  ## =================================================
  ## nruns    => SELECT THE NUMBER OF INDEPENDANT RUNS
  ## maxiter  => SELECT THE MAX NUMBER OF ITERATIONS
  ## TF       => PLOT (TRUE/FALSE) THE RESULTS
  ## stars    => EVALUATION OF THE PACKAGE::ALGORITHMS
  ## params   => comment: maxiter/lr AS CHARACTER
  ## printmsg => PRINT timeR DURING THE TRAINING
  ## =================================================
  nruns   <- 10
  method  <- "Rprop"
  maxiter <- 5000
  TF      <- TRUE 
  stars   <- ""
  params  <- "maxiter = 5000"
  descr   <- paste(dset,  "CaDENCE:Rprop", sep = "_")
  
  
  timer    <- createTimer()
  printmsg <- FALSE
  
  
  ## AUTO
  plotNN(xory, y0, uni, TF, main = descr)
  Ypred  <- list()
  Rmse   <- numeric(length = nruns)
  Mae    <- numeric(length = nruns)
  for(i in 1:nruns){
    event      <- paste0(descr, sprintf("_%.2d", i))
    timer$start(event)
    #### ADJUST THE FOLLOWING LINES TO THE PACKAGE::ALGORITHM
    #### DO NOT MODIFY THE <error> LINE IN tryCatch() 
    NNreg      <- tryCatch(
      CaDENCE::cadence.fit(x = x, y = y, 
                    iter.max = maxiter, 
                    n.hidden = neur, 
                    hidden.fcn = tanh, 
                    method = method, 
                    n.trials = 1, 
                    trace = 0, 
                    maxit.Nelder = 1, 
                    f.cost = cadence.cost,
                    distribution = list(density.fcn = dnorm,
                                        parameters = c("mean", "sd"),
                                        parameters.fixed = NULL,
                                        output.fcns = c(identity, exp))),
      error = function(y) {lm(y ~ 0, data = Zxy)}
    )
    y_pred     <- tryCatch(
      ym0 + ysd0 * CaDENCE::cadence.predict(x = x, fit = NNreg)[,1],
      error = function(NNreg) rep(ym0, nrow(Zxy))
    )
    ####
    Ypred[[i]] <- y_pred
    Rmse[i]    <- funRMSE(y_pred, y0)
    Mae[i]     <- funMAE(y_pred, y0)
    timer$stop(event, RMSE = Rmse[i], MAE = Mae[i], stars = stars, 
               params = params, printmsg = printmsg)
    lipoNN(xory, y_pred, uni, TF, col = i)
  }
  best <- which(Rmse == min(Rmse, na.rm = TRUE))[1] ; best ; Rmse[[best]]
  lipoNN(xory, Ypred[[best]], uni, TF, col = 4, lwd = 4)
  getTimer(timer)
  
  
  
  
  ## ===========================================================
  ## PLOT THE MODEL WITH THE BEST RMSE
  ## (NOT NECESSARY THE BEST MODEL: WE WILL NOT DISCUSS IT HERE)
  ## ===========================================================
  best ; Rmse[[best]]
  plotNN(xory, y0, uni, TF, main = descr)
  lipoNN(xory, Ypred[[best]], uni, TF, col = 4, lwd = 4)
  
  
  
  ## ================
  ## SAVE THE RESULTS
  ## ================
  add2csv(getTimer(timer), file = paste0(dset, "-results.csv"))
  
  
  
  ## ===================================================
  ## DETACH ZZ
  ## REMOVE OBJECTS NOT PROTECTED BY donotremove IN ls() 
  ## END OF THE LOOP + CLOSE PLOT DEVICES
  ## ===================================================
  detachNN(donotremove, donotdetach = "")
  # ls()
  # search()
  
}
# dev.off()



## ===================================================
## REMOVE OBJECTS NOT PROTECTED BY donotremove IN ls() 
## REMOVE PACKAGES NOT INCLUDED IN donotdetach
## ===================================================
clearNN(donotremove, donotdetach = NULL)
rm(list=ls(all=TRUE))
ls()
search()


## ===========================
## PRINT THE (HIDDEN) WARNINGS
## ===========================
summary(warnings())
options(warn = 0)    # warnings are printed (default)


## END
## END


