## ----message=FALSE, warning=FALSE----------------------------------------
library(NNbenchmark)
library(ANN2)

library(stringr)
library(dplyr)
library(kableExtra)


## ------------------------------------------------------------------------
options(scipen = 9999)
options("digits.secs" = 2)
timer  <- createTimer(verbose = FALSE)


## ------------------------------------------------------------------------
NNdataSummary(NNdatasets)


## ------------------------------------------------------------------------
hyperParams <- function(optim_method) {
    
    if (!is.element(optim_method, c("sgd", "adam", "rmsprop"))) stop("Invalid Parameters.")
    if (optim_method == "sgd") { iter <- 4000; lr <- 0.001} 
    if (optim_method == "adam") { iter <- 3000; lr <- 0.007} 
    if (optim_method == "rmsprop") { iter <- 3000; lr <- 0.005} 
    
    params <- paste0("method=", optim_method, "_", "lr=", lr, "_", "iter=", iter)
    
    out <- list(iter = iter, lr = lr, params = params)
    
    return (out)
}



NNtrain <- function(x, y, hidden_neur, optim_method) {
    
    hyper_params <- hyperParams(optim_method)
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    
    NNreg <- neuralnetwork(X = x, y = y, 
                           val.prop = 0, 
                           standardize = FALSE, 
                           hidden.layers = c(hidden_neur), 
                           regression = TRUE,
                           loss.type = "squared",
                           n.epochs = iter,
                           optim.type = optim_method,
                           learn.rates = lr,
                           verbose = FALSE,
                           random.seed = as.integer(runif(1)*10000000))
    
    return (NNreg)
}


## ----fig.height=5, fig.width=14, message=FALSE, warning=FALSE------------
#for (dset in names(NNdatasets)) {
for (dset in 1) {    

    ## =============================================
    ## EXTRACT INFORMATION FROM THE SELECTED DATASET
    ## =============================================
    ds     <- NNdatasets[[dset]]$ds
    Z      <- NNdatasets[[dset]]$Z
    neur   <- NNdatasets[[dset]]$neur
    nparNN <- NNdatasets[[dset]]$nparNN
    fmlaNN <- NNdatasets[[dset]]$fmlaNN
    donotremove  <- c("dset", "dsets", "ds", "Z", "neur", "TF", "nrep", "timer",
                      "donotremove", "donotremove2")
    donotremove2 <- c("dset", "dsets") 



    ## ===================================================
    ## SELECT THE FORMAT REQUIRED BY THE PACKAGE/ALGORITHM
    ## d = data.frame, m = matrix, v = vector/numeric
    ## ATTACH THE OBJECTS CREATED (x, y, Zxy, ... )
    ## ===================================================
    ZZ     <- prepareZZ(Z, xdmv = "d", ydmv = "v", zdm = "d", scale = TRUE)
    attach(ZZ)

    ## =============================================
    ## SELECT THE PACKAGE USED FOR TRAINING
    ## nrep => SELECT THE NUMBER OF INDEPENDANT RUNS
    ## iter => SELECT THE MAX NUMBER OF ITERATIONS
    ## TF   => PLOT THE RESULTS
    ## =============================================

    
    nrep   <- 10
    TF     <- TRUE 

    method <- c("sgd", "adam", "rmsprop")
        
    for (m in method) {
        
        descr  <- paste(dset, "ANN2::neuralnetwork", m, sep = "_")

        ## AUTO
        Ypred  <- list()
        Rmse   <- numeric(length = nrep)
        Mae    <- numeric(length = nrep)
    
        for(i in 1:nrep){
            event      <- paste0(descr, sprintf("_%.2d", i))
            timer$start(event)
            #### ADJUST THE FOLLOWING LINES TO THE PACKAGE::ALGORITHM
            cat(event, "\n")
            
            hyper_params <- hyperParams(optim_method = m)

            NNreg      <- tryCatch(
                            NNtrain(x = x, y = y, hidden_neur = neur, optim_method = m),
                            error = function(y) {lm(y ~ 0, data = Zxy)}
                          )     
			print(str(NNreg))                          
            y_pred     <- tryCatch(
                            ym0 + ysd0*predict(NNreg, x)$predictions,
                            error = ym0
                          )     
            ####
            Ypred[[i]] <- y_pred
            Rmse[i]    <- funRMSE(y_pred, y0)
            Mae[i]     <- funMAE(y_pred, y0)
            timer$stop(event, RMSE = Rmse[i], MAE = Mae[i], params = hyper_params$params, printmsg = FALSE)
            cat("\n")
        }
        best <- which(Rmse == min(Rmse, na.rm = TRUE))[1]
        best ; Rmse[[best]]
        
        ## ================================================
        ## PLOT ALL MODELS AND THE MODEL WITH THE BEST RMSE
        ## par OPTIONS CAN BE IMPROVED FOR A BETTER DISPLAY
        ## ================================================
        op <- par(mfcol = c(1,2))
        plotNN(xory, y0, uni, TF, main = descr)
        for (i in 1:nrep) lipoNN(xory, Ypred[[i]], uni, TF, col = i, lwd = 1)
        
        plotNN(xory, y0, uni, TF, main = descr)
        lipoNN(xory, Ypred[[best]], uni, TF, col = 4, lwd = 4)
        par(op)
    }


## ===========================
## DETACH ZZ - END OF THE LOOP
## ===========================
#    detach(ZZ)
}


## ------------------------------------------------------------------------
dfr0 <- getTimer(timer) 

dfr  <- data.frame(
    ds_pkg.fun_algo = stringr::str_sub(dfr0[ ,1], 1, -4),
    run     = stringr::str_sub(dfr0[ ,1], -2, -1),
    dfr0[, c("RMSE","MAE")],
    dataset = stringr::str_replace_all(stringr::str_extract(dfr0[, 1], pattern = "^\\w*_"), fixed("_"), ""),
    method = stringr::str_replace_all(stringr::str_extract(dfr0[, 1], pattern = "_\\w*_"), fixed("_"), ""),
    Elapsed = round(dfr0[ ,4], 5),
    params = dfr0$params
)


dfr %>%
    select(-c(dataset, method))


## ------------------------------------------------------------------------
dfr %>%
    group_by(dataset, method) %>%
    summarise(minRMSE = min(RMSE), meanRMSE = mean(RMSE), meanTime = mean(Elapsed)) %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    collapse_rows(columns = 1:2, valign = "top")


## ------------------------------------------------------------------------
clearNN(donotremove)

