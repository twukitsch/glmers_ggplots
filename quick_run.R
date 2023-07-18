# SET WORKING DIRECTORY
setwd("C:/Users/kieri/Documents/ABHV")
getwd()

# RUN SETUP (download & load necessary packages, import data, subset & tweak data)
source("setup.R")

# RUN ETHANOL glmers
source("ethanol_glmers.R")

# RUN SUCROSE glmers
source("sucrose_glmers.R")

# RUN WATER glmers
source("water_glmers.R")

# RUN GRAPH SETUP
source("graph_setup.R")

# RUN ETHANOL graphs
source("ethanol_graphs.R")

# RUN SUCROSE graphs
source("sucrose_graphs.R")

# RUN SUCROSE graphs
source("water_graphs.R")

# SAVE WORKSPACE
save.image("ABHV_workspace.RData")