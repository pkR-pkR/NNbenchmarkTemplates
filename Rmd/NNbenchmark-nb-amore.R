## ----message=FALSE, warning=FALSE----------------------------------------
library(NNbenchmark)
library(AMORE)

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
    
    if (!is.element(optim_method, c("ADAPTgd", "ADAPTgdwm", "BATCHgd", "BATCHgdwm"))) stop("Invalid Parameters.")
    if (optim_method == "ADAPTgd") {iter <- 3500; lr <- 0.01; momentum <- 0; hidden_activation <- "tansig"} 
    if (optim_method == "ADAPTgdwm") {iter <- 4000; lr <- 0.009; momentum <- 0.8; hidden_activation <- "sigmoid"} 
    if (optim_method == "BATCHgd") {iter <- 7500; lr <- 0.01; momentum <- 0; hidden_activation <- "tansig"}
    if (optim_method == "BATCHgdwm") {iter <- 9000; lr <- 0.008; momentum <- 0.7; hidden_activation <- "tansig"}
    
    params <- paste0("method=", optim_method, "_lr=", lr, "_iter=", iter, "_momentum=", momentum, "_hidden_activation=", hidden_activation)
    
    out <- list(iter = iter, lr = lr, params = params, momentum = momentum, hidden_activation = hidden_activation)
    
    return (out)
}



NNtrain <- function(x, y, hidden_neur, optim_method) {
    
    hyper_params <- hyperParams(optim_method)
    
    iter <- hyper_params$iter
    lr <- hyper_params$lr
    momentum <- hyper_params$momentum
    hidden_activation <- hyper_params$hidden_activation
    
    net_structure <- AMORE::newff(n.neurons = c(ncol(x), hidden_neur, 1), 
                                  learning.rate.global = lr, 
                                  error.criterium = "LMS", 
                                  hidden.layer = hidden_activation, 
                                  method = method,
                                  momentum.global = momentum)
    
    NNreg <- AMORE::train(net = net_structure, 
                          P = x, T = y, 
                          error.criterium = "LMS", 
                          report = FALSE, 
                          n.shows = iter, 
                          show.step = 1)
    
    return (NNreg)
}


## ----fig.height=5, fig.width=14, message=FALSE, warning=FALSE------------
for (dset in names(NNdatasets)) {

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

    method <- c("ADAPTgd", "ADAPTgdwm", "BATCHgd", "BATCHgdwm")
        
    for (m in method) {
        
        descr  <- paste(dset, "AMORE::train", m, sep = "_")

        ## AUTO
        Ypred  <- list()
        Rmse   <- numeric(length = nrep)
        Mae    <- numeric(length = nrep)
    
        for(i in 1:nrep){
            event      <- paste0(descr, sprintf("_%.2d", i))
            timer$start(event)
            #### ADJUST THE FOLLOWING LINES TO THE PACKAGE::ALGORITHM
            
            hyper_params <- hyperParams(optim_method = m)

            NNreg      <- tryCatch(
                            NNtrain(x = x, y = y, hidden_neur = neur, optim_method = m),
                            error = function(y) {lm(y ~ 0, data = Zxy)}
                          )     
            y_pred     <- tryCatch(
                            ym0 + ysd0 * AMORE::sim.MLPnet(NNreg$net, x),
                            error = ym0
                          )     
            ####
            Ypred[[i]] <- y_pred
            Rmse[i]    <- funRMSE(y_pred, y0)
            Mae[i]     <- funMAE(y_pred, y0)
            timer$stop(event, RMSE = Rmse[i], MAE = Mae[i], params = hyper_params$params, printmsg = FALSE)
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
    detach(ZZ)
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

