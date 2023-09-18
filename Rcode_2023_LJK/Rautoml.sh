#!/bin/bash
# -- Un nom pour le job --
#OAR -n R-NN-check-automl
# -- Description des ressources souhaites --
#OAR -l /nodes=1/core=1,walltime=00:25:00
# -- Le projet Perseus --
#OAR --project actuar
# -- notification --
#OAR --notify mail:christophe.dutang@grenoble-inp.fr

# -- lance guix et le profil --
source /applis/site/guix-start.sh
refresh_guix RNNbench

# -- install les paquets R, voir ?build, ?INSTALL --
echo "*** debut install paquets"
R CMD INSTALL /home/$USER/codepourdahu/NNbench/NNbenchmark_3.2.0.tar.gz -l /home/$USER/Rpersolib
R CMD INSTALL /home/$USER/codepourdahu/NNbench/automl_1.3.2.tar.gz -l /home/$USER/Rpersolib
echo "fin install paquets ***"

# -- lance les calculs --
echo "*** debut NN bench automl.R"
R CMD BATCH /home/$USER/codepourdahu/NNbench/automl-check.R  /home/$USER/codepourdahu/NNbench/automl.Routputs
echo "fin automl ***"
