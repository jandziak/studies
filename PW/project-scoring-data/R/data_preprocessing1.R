
#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' The script performs preprocessing STEP 1. of German Credit dataset. 
#' 
#' Includes: 
#'  - converting values of qualiative variables from "codes" to "human-readable" 
#'  - recoding respnse variable accoriding to the convention from 
#'    "An Introduction to Statistical Learning with Applications in R"
#'  - renaming dataset columns
#'  - reordering dataset columns
#'  - saving resulting dataset 



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

# Read raw data 
gcredit <- read.table("./data/german_data.txt")

# # Dataset overview  
# str(gcredit)
# head(gcredit)




# -----------------------------------------------------------------------------
# RECODE VALUES 


# Make copy of the dataset 
gcredit.cpy <- gcredit

# Define columns which are to be recoded 
rec.col.sgns <- c("V1", "V3", "V4", "V6", "V7", "V9", "V10", "V12", "V14", "V15",
              "V17", "V19", "V20")

# Recode columns 
for(col.sgn in rec.col.sgns){  
  # Recode values 
  col.tmp <- sapply(gcredit.cpy[, col.sgn], 
                    function(val) 
                      gcredit.recode.values(col.sgn, as.character(val)))
#   # Convert to factor 
#   gcredit.cpy[, col.sgn] <- gcredit.convert.to.factor(col.sgn, col.tmp)  
  message(paste0("Column: ", col.sgn, " values recoded."))
}



#' Response variable coding convention 
#' 
#' In our anlysis, we follow the convention from:
#' "An Introduction to Statistical Learning with Applications in R" by
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani
#' 
#' according to which we model probability of client's default. 
#' 
#' This means that we recode the response variable to have value: 
#'  - 1 for default ("bad") client
#'  - 0 for non-default ("good") client

# Recode response variable 
response.tmp <- sapply(gcredit.cpy[,"V21"], function(val) val-1)
response.tmp <- factor(response.tmp, levels = c("0", "1"), labels = c("0", "1"))
gcredit.cpy[,"V21"] <- response.tmp



# Rename dataset columns 
col.names <- 
  c("CHK_ACCT",
    "DURATION",
    "HISTORY",
    "PURPOSE",
    "AMOUNT",
    "SAVINGS_ACCT",
    "EMLOYMENT",
    "RATE_TO_DISP_INCOME",
    "STATUS_AND_SEX",
    "OTHER_DEBTORS",
    "RESIDENCE",
    "PROPERTY",
    "AGE",
    "OTHER_INSTALLMENT_PLANS",
    "HOUSING",
    "NUM_OF_THIS_BANK_CREDITS",
    "JOB",
    "NUM_OF_MAINTAINED_PEOPLE",
    "TELEPHONE",
    "IS_FOREIGN_WORKER",
    "RES")

names(gcredit.cpy) <- col.names

# Reorder columns in data frame to have firsty numeric, then factor variables
num.cols <- c(2,5,8,11,13,16,18)
gcredit.cpy <- gcredit.cpy[c(num.cols, setdiff(1:20, num.cols), 21)]

# Save tmp version of the data 
write.table(x = gcredit.cpy, file = "./data/german_data_processed1.txt", 
            sep=",",  col.names=TRUE, row.names = FALSE)

# Remove tmp versions of the data 
rm(gcredit.cpy)
rm(gcredit)