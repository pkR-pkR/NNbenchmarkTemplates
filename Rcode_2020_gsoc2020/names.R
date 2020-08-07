setwd("D:/GSoC2020/08/Rmd_2020")
rmd <- list.files(pattern = '*.Rmd', recursive = T)
chunks <- paste0("```{r child = '", rmd, "'}\n```\n")
cat(chunks, sep = '\n')
"
```{r child = 'NNbenchmark-2020-AMORE.Rmd'}
```

```{r child = 'NNbenchmark-2020-ANN2.Rmd'}
```

```{r child = 'NNbenchmark-2020-automl.Rmd'}
```

```{r child = 'NNbenchmark-2020-BNN.Rmd'}
```

```{r child = 'NNbenchmark-2020-brnn.Rmd'}
```

```{r child = 'NNbenchmark-2020-CaDENCE.Rmd'}
```

```{r child = 'NNbenchmark-2020-caret.Rmd'}
```

```{r child = 'NNbenchmark-2020-deepdive.Rmd'}
```

```{r child = 'NNbenchmark-2020-deepnet.Rmd'}
```

```{r child = 'NNbenchmark-2020-elmNNRcpp.Rmd'}
```

```{r child = 'NNbenchmark-2020-ELMR.Rmd'}
```

```{r child = 'NNbenchmark-2020-EnsembleBase.Rmd'}
```

```{r child = 'NNbenchmark-2020-GMDHreg.Rmd'}
```

```{r child = 'NNbenchmark-2020-h2o.Rmd'}
```

```{r child = 'NNbenchmark-2020-keras.Rmd'}
```

```{r child = 'NNbenchmark-2020-MachineShop.Rmd'}
```

```{r child = 'NNbenchmark-2020-minpack.lm.Rmd'}
```

```{r child = 'NNbenchmark-2020-monmlp.Rmd'}
```

```{r child = 'NNbenchmark-2020-neuralnet.Rmd'}
```

```{r child = 'NNbenchmark-2020-nlsr.Rmd'}
```

```{r child = 'NNbenchmark-2020-nnet.Rmd'}
```

```{r child = 'NNbenchmark-2020-qrnn.Rmd'}
```

```{r child = 'NNbenchmark-2020-radiant.model.Rmd'}
```

```{r child = 'NNbenchmark-2020-rminer.Rmd'}
```

```{r child = 'NNbenchmark-2020-RSNNS.Rmd'}
```

```{r child = 'NNbenchmark-2020-snnR.Rmd'}
```

```{r child = 'NNbenchmark-2020-TrafficBDE.Rmd'}
```

```{r child = 'NNbenchmark-2020-traineR.Rmd'}
```

```{r child = 'NNbenchmark-2020-validann.Rmd'}
```

```{r child = 'NNbenchmark-2020-yager.Rmd'}
```

```{r child = 'NNbenchmark-B-try-generic-tpl.Rmd'}
```

```{r child = 'NNbenchmark-B-try-rank-tpl.Rmd'}
```

```{r child = 'NNbenchmark-try-generic-tpl.Rmd'}