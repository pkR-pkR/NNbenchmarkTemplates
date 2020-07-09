
# ============================
# A simple script for changing
# certain lines in templates
# ============================

library(xfun)

# DATE
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "2020/07/09", replacement = "2020/07/09")

# DIRECTORY
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "D:/GSoC2020/Results/2020run02/", replacement = "D:/GSoC2020/Results/2020run02/")
gsub_dir(dir = "D:/GSoC2020/07/clean RMD 01", pattern = "nrep <- 10", replacement = "nrep <- 10")

