
#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' The script performs preprocessing STEP 2. of German Credit dataset.
#' 
#' Includes:
#'  - 
#'  -
#'  -  



# -----------------------------------------------------------------------------
# READ DATA 

# Define tmp working direcotry based on computer name 
computer.name <- Sys.info()["nodename"] 
if (computer.name == "marta-komputer") 
  wd.tmp <- "/home/martakarass/my-store/studies/PW/project-scoring-data"

# Set working dir 
setwd(wd.tmp)

# Source script with functions to make preprocessing
source("./R/data_preprocessing_UTILS.R")




# Read data (numbers - except from response variable - are read as numeric)
gcredit <- 
  read.table("./data/german_data_processed1.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 7), rep("character", 14)))

# Relevel factors to keep original "order"
for(col.name in names(gcredit)) 
  gcredit[, col.name] <- gcredit.convert.to.factor(col.name, gcredit[, col.name])


# str(gcredit)
# head(gcredit)




# ------------------------------------------------------------------------------
# CELANING DATA 



#-----------------------------------------------
# Dealing with missing, corrupt and invalid data

names(gcredit)
# [1] "CHK_ACCT"                 "DURATION"                
# [3] "HISTORY"                  "PURPOSE"                 
# [5] "AMOUNT"                   "SAVINGS_ACCT"            
# [7] "EMLOYMENT"                "RATE_TO_DISP_INCOME"     
# [9] "STATUS_AND_SEX"           "OTHER_DEBTORS"           
# [11] "RESIDENCE"                "PROPERTY"                
# [13] "AGE"                      "OTHER_INSTALLMENT_PLANS" 
# [15] "HOUSING"                  "NUM_OF_THIS_BANK_CREDITS"
# [17] "JOB"                      "NUM_OF_MAINTAINED_PEOPLE"
# [19] "TELEPHONE"                "IS_FOREIGN_WORKER"       
# [21] "RES"



# Check unique factor values
for(name  in names(gcredit)){
  
  var.tmp <- gcredit[, name]
  message(name)
  
  if(is.factor(var.tmp)){
    print(unique(var.tmp))
  } else{
    print(summary(var.tmp))
  }
}
# Observation: there's no "NA" values or values we did not notice while 
# converting to factor



#-------------------------------------------------
# Mutating variables, defining defivative vriables 
# (for categirized variables)  

#install.packages("devtools")
library(devtools)
#install_github("riv","tomasgreif")
library(woe)




mosaicplot(table(gcredit$CHK_ACCT, gcredit$RES))
unique(gcredit$CHK_ACCT)


# DURATION
summary(gcredit$DURATION)
hist(gcredit$DURATION)
plot(density(gcredit$DURATION))

