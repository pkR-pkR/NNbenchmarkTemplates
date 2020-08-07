
# ============================
# A simple script for changing
# certain lines in templates
# ============================

library(xfun)

#####
# DATE
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "2020/07/09", replacement = "2020/07/09")

# DIRECTORY
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "D:/GSoC2020/Results/2020run02/", replacement = "D:/GSoC2020/Results/2020run02/")
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "nrep <- 10", replacement = "nrep <- 10")



#####

# DATE
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", pattern = "2020/07/09", replacement = "2020/02/08")

# DIRECTORY
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", pattern = "D:/GSoC2020/Results/2020run02/", replacement = "D:/GSoC2020/Results/2020run02/")

# CHANGE RCODE
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "r trainpredict, message=FALSE, warning=FALSE, results='hide'", 
         replacement = "r, message=FALSE, warning=FALSE, results='hide'")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "r, message=FALSE, warning=FALSE, results='hide'", 
         replacement = "r, message=FALSE, warning=FALSE, results='hide', fig.height=7, fig.width=14")

# CHANGE DATASETS
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "1:12, pkgname =", 
         replacement = "1:2, pkgname =")


#####

# DATE
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", pattern = "2020/07/12", replacement = "2020/06/08")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", pattern = "2020/06/08", replacement = "2020/08/07")

# DIRECTORY
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", pattern = "D:/GSoC2020/Results/2020run02/", replacement = "D:/GSoC2020/Results/2020run03/")

# CHANGE HEADINGS
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "# Environment and datasets", 
         replacement = "# Setup")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "## Environment", 
         replacement = "## Packages and options")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "# Dedicated functions by packages", 
         replacement = "")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "train/predict functions - arguments", 
         replacement = "trainPredict arguments - inputs")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "# Launch the NN package", 
         replacement = "# Launch package's trainPredict")

# CHANGE R CODE
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "1:2, pkgname =", 
         replacement = "1:12, pkgname =")
gsub_dir(dir = "D:/GSoC2020/08/Rmd_2020", 
         pattern = "1:2, pkgname =", 
         replacement = "1:12, pkgname =")