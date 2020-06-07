GSoC2019.testedpkg <- c("AMORE" 	,"ANN2" 	,"appnn" 	,"autoencoder" 	,"automl", 	"BNN",
                        "brnn" 	,"Buddle" 	,"CaDENCE" 	,"cld2" 	,"cld3", 	"condmixt",
                        "DALEX2" 	,"DamiaNN" 	,"deepnet" 	,"deepNN" 	,"DNMF" 	,"elmNNrcpp",
                        "ELMR" 	,"EnsembleBase" 	,"evclass" 	,"gamlss.add" 	,"gcForest" 	,"GMDH",
                        "GMDH2" 	,"GMDHreg" 	,"grnn" 	,"h2o" 	,"hybridEnsemble" 	,"isingLenzMC",
                        "keras" 	,"kerasformula "	,"kerasR" 	,"leabRa" 	,"learNN", 	"LilRhino",
                        "monmlp" 	,"neural" 	,"neuralnet" 	,"NeuralNetTools" 	,"NlinTS", "nnet",
                        "nnetpredint" 	,"nnfor" 	,"onnx" 	,"OptimClassifier" 	,"OSTSC" 	,"pnn",
                        "polyreg" 	,"predictoR" 	,"qrnn" 	,"QuantumOps" 	,"quarrint" 	,"radiant.model",
                        "rasclass" 	,"rcane" 	,"rminer" 	,"rmn" 	,"RSNNS" 	,"ruta",
                        "simpleNeural" 	,"snnr" 	,"softmaxreg" 	,"Sojourn.Data" 	,"spnn" 	,"TeachNet",
                        "tensorflow" 	,"tfestimators" 	,"trackdem" 	,"TrafficBDE" 	,"validann")

install.packages(GSoC2019.testedpkg)

#BEWARE
# h2o needs Java JDK to be installed
# keras needs Python with package TensorFlow (as well as R package tensorflow), see keras:::keras_version()

