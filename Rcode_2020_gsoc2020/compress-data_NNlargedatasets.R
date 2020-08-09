save(mWoodN1, compress = "xz", file = "D:/GSoC2020/NNbenchmark/data/bWoodN1.rda")
save(NNdatasets, compress = "xz", file = "D:/GSoC2020/NNbenchmark/data/NNdatasets.rda")
save(list = ls(all = TRUE), compress = "xz", file = "D:/GSoC2020/NNbenchmark/data/mNNdatasets.rda")

# Changes to mWoodN1
load("D:/GSoC2020/NNbenchmark/data/bWoodN1.rda")
names(mWoodN1) <- c("y","x1","x2","x3","x4","x5","x6")
mWoodN1 <- mWoodN1[,c("x1","x2","x3","x4","x5","x6","y")]
save(mWoodN1, compress = "xz", file = "D:/GSoC2020/NNbenchmark/data/bWoodN1.rda")

NNlargedatasets <- list(mWoodN1 = list(ds = "mWoodN1",
                           neur = 5, nparNN = 41, 
                           fmlaNN = formula(y ~ b1 + b2 * tanh(b3 + b4 * x1 + b5 * x2 + b6 * x3 + b7 * x4 + b8 * x5 + b9 * x6)
                                            + b10 * tanh(b11 + b12 * x1 + b13 * x2 + b14 * x3 + b15 * x4 + b16 * x5 + b17 * x6)
                                            + b18 * tanh(b19 + b20 * x1 + b21 * x22 + b22 * x3 + b23 * x4 + b24 * x5 + b25 * x6)
                                            + b26 * tanh(b27 + b28 * x1 + b29 * x2 + b30 * x3 + b31 * x4 + b32 * x5 + b33 * x6)
                                            + b34 * tanh(b35 + b36 * x1 + b37 * x2 + b38 * x3 + b39 * x4 + b40 * x5 + b41 * x6)),
                           Z = as.data.frame(mWoodN1)))
save(NNlargedatasets, compress = "xz", file = "D:/GSoC2020/NNbenchmark/data/NNlargedatasets.rda")
