
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
if (computer.name == "MATHPASSION") 
  wd.tmp <- "C:/Users/Math/studia/studies/PW/project-scoring-data"
if (computer.name == "JIDZIAK-L") {
  wd.tmp <- "C:/Users/jidziak/Desktop/project-scoring-data"
}
setwd(wd.tmp)


# ------------------------------------------------------------------------------
# Load libraries

library(corrgram)
library(ggplot2)
require(stringr)

#' To load `woe` package: 
#' 1. download woe-master.zip from https://github.com/tomasgreif/woe 
#' 2. unzip it
#' 3. install from source:
#' install.packages("/home/martakarass/Downloads/woe-master", repos = NULL, type="source")
library(woe)
library(caret)
library(pROC)
library(smbinning)
library(reshape)
library(dplyr)
library(MASS)
library(pander)
library(stringr)
# to install under Ubuntu, follow: http://stackoverflow.com/questions/13403268/error-while-loading-rjava
library(rJava)
library(RWekajars)
library(RWeka)
library(FSelector)
library(fpc)
library(cluster)
library(mlbench)


# ------------------------------------------------------------------------------
# Source project util scripts 

source("./R/UTILS_data_preprocessing.R")

