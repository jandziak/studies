
#' @description
#' Project configuration file.
#' 
#' Contains: 
#'  - setting of working directory (depenfing on user's comupter name)
#'  - loading all libraries used in the project
#'  - sourcing of all util scipts used in project



# ------------------------------------------------------------------------------
# Set working directory

computer.name <- Sys.info()["nodename"] 
if (computer.name == "marta-komputer") 
  wd.tmp <- "/home/martakarass/my-store/studies/PW/project-scoring-data"

setwd(wd.tmp)




# ------------------------------------------------------------------------------
# Load libraries

library(corrgram)
library(ggplot2)

#' To load `woe` package: 
#' 1. download woe-master.zip from https://github.com/tomasgreif/woe 
#' 2. unzip it
#' 3. install from source:
#' install.packages("/home/martakarass/Downloads/woe-master", repos = NULL, type="source")
library(woe)




# ------------------------------------------------------------------------------
# Source project util scripts 

source("./R/data_preprocessing_UTILS.R")