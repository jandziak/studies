
#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' The script with extracted functions to perform German Credit dataset 
#' preprocessing.




#' -----------------------------------------------------------------------------
#' Substitute value signatures with their human-readable values. 
#' 
#' @param col.sgn - signature of dataset column (from raw dataset)
#' @param val - value to be substituted 
#' 
gcredit.recode.values <- function(col.sgn, val){
  
  # Status of existing checking account 
  if(col.sgn == "V1"){
    switch(val,
           "A11"="<0 DM",
           "A12"="0-200 DM",
           "A13"=">=200 DM",
           "A14"="no checking account",
           val)
    
    # Credit history 
  } else if(col.sgn == "V3"){
    switch(val,
           "A30"="no credits taken/ all credits paid back duly",
           "A31"="all credits at this bank paid back duly",
           "A32"="existing credits paid back duly till now",
           "A33"="delay in paying off in the past", 
           "A34"="critical account/ other credits existing (not at this bank)",
           val)
    
    # Purpose
  } else if(col.sgn == "V4"){
    switch(val,
           "A40"="car (new)", 
           "A41"="car (used)",
           "A42"="furniture/equipment", 
           "A43"="radio/television", 
           "A44"="domestic appliances", 
           "A45"="repairs", 
           "A46"="education", 
           "A47"="(vacation - does not exist?)", 
           "A48"="retraining", 
           "A49"="business", 
           "A410"="others",
           val)
    
    # Savings account/bonds 
  } else if(col.sgn == "V6"){
    switch(val,
           "A61"="<100 DM", 
           "A62"="100-500 DM", 
           "A63"="500-1000 DM", 
           "A64"=">=1000 DM", 
           "A65"="unknown/ no savings account", 
           val)
    
    # Present employment since
  } else if(col.sgn == "V7"){
    switch(val,
           "A71"="unemployed", 
           "A72"="<1 year", 
           "A73"="1-4 years", 
           "A74"="4-7 years", 
           "A75"=">= 7 years", 
           val)
    
    # Personal status and sex 
  } else if(col.sgn == "V9"){
    switch(val,
           "A91"="male : divorced/separated",
           "A92"="female : divorced/separated/married",
           "A93"="male : single", 
           "A94"="male : married/widowed", 
           "A95"="female : single", 
           val)
    
    # Other debtors / guarantors 
  } else if(col.sgn == "V10"){
    switch(val,
           "A101"="none", 
           "A102"="co-applicant", 
           "A103"="guarantor", 
           val)
    
    # Property
  } else if(col.sgn == "V12"){
    switch(val,
           "A121"="real estate", 
           "A122"="building society savings agreement/ life insurance", 
           "A123"="car or other, not in Savings",
           "A124"="unknown / no property", 
           val)
    
    # Other installment plans
  } else if(col.sgn == "V14"){
    switch(val,
           "A141"="bank", 
           "A142"="stores", 
           "A143"="none", 
           val)
    
    # Housing
  } else if(col.sgn == "V15"){
    switch(val,
           "A151"="rent", 
           "A152"="own", 
           "A153"="for free",           
           val)
    
    # Job
  } else if(col.sgn == "V17"){
    switch(val,
           "A171"="unemployed/ unskilled - non-resident", 
           "A172"="unskilled - resident", 
           "A173"="skilled employee / official", 
           "A174"="management/ self-employed/ highly qualified employee/ officer",           
           val)
    
    # Telephone
  } else if(col.sgn == "V19"){
    switch(val,
           "A191"="none", 
           "A192"="yes",          
           val)
    
    # foreign worke
  } else if(col.sgn == "V20"){
    switch(val,
           "A201"="yes", 
           "A202"="no",         
           val)
    
  } else{    
    #message("Undefined column name.")
    return(val)
  }
}




#' -----------------------------------------------------------------------------
#' Convert vector to factor, with specified factor levels.   
#' 
#' @param col.sgn - signature of dataset column 
#'                  (from raw dataset or from dataset with given names)
#' @param col.vals - vector of column values
#' 
gcredit.convert.to.factor <- function(col.sgn, col.vals){
  
  # Status of existing checking account 
  if(col.sgn == "V1" || col.sgn == "CHK_ACCT"){
    lvls <- c(
      "<0 DM",
      "0-200 DM",
      ">=200 DM",
      "no checking account"
    )
    
    # Credit history 
  } else if(col.sgn == "V3" || col.sgn == "HISTORY"){
    lvls <- c(
      "no credits taken/ all credits paid back duly",
      "all credits at this bank paid back duly",
      "existing credits paid back duly till now",
      "delay in paying off in the past", 
      "critical account/ other credits existing (not at this bank)"
    )
    
    # Purpose
  } else if(col.sgn == "V4" || col.sgn == "PURPOSE"){
    lvls <- c(
      "car (new)", 
      "car (used)",
      "furniture/equipment", 
      "radio/television", 
      "domestic appliances", 
      "repairs", 
      "education", 
      "(vacation - does not exist?)", 
      "retraining", 
      "business", 
      "others"
    )
    
    # Savings account/bonds 
  } else if(col.sgn == "V6" || col.sgn == "SAVINGS_ACCT"){
    lvls <- c(
      "<100 DM", 
      "100-500 DM", 
      "500-1000 DM", 
      ">=1000 DM", 
      "unknown/ no savings account"
    )
    
    # Present employment since
  } else if(col.sgn == "V7" || col.sgn == "EMLOYMENT"){
    lvls <- c(
      "unemployed", 
      "<1 year", 
      "1-4 years", 
      "4-7 years", 
      ">= 7 years" 
    )
    
    # Personal status and sex 
  } else if(col.sgn == "V9" || col.sgn == "STATUS_AND_SEX"){
    lvls <- c(
      "male : divorced/separated",
      "female : divorced/separated/married",
      "male : single", 
      "male : married/widowed", 
      "female : single"
    )
    
    # Other debtors / guarantors 
  } else if(col.sgn == "V10" || col.sgn == "OTHER_DEBTORS"){
    lvls <- c(
      "none", 
      "co-applicant", 
      "guarantor" 
    )
    
    # Property
  } else if(col.sgn == "V12" || col.sgn == "PROPERTY"){
    lvls <- c(
      "real estate", 
      "building society savings agreement/ life insurance", 
      "car or other, not in Savings",
      "unknown / no property"
    )
    
    # Other installment plans
  } else if(col.sgn == "V14" || col.sgn == "OTHER_INSTALLMENT_PLANS"){
    lvls <- c(
      "bank", 
      "stores", 
      "none" 
    )
    
    # Housing
  } else if(col.sgn == "V15" || col.sgn == "HOUSING"){
    lvls <- c(
      "rent", 
      "own", 
      "for free"           
    )
    
    # Job
  } else if(col.sgn == "V17" || col.sgn == "JOB"){
    lvls <- c(
      "unemployed/ unskilled - non-resident", 
      "unskilled - resident", 
      "skilled employee / official", 
      "management/ self-employed/ highly qualified employee/ officer"           
    )
    
    # Telephone
  } else if(col.sgn == "V19" || col.sgn == "TELEPHONE"){
    lvls <- c(
      "none", 
      "yes"          
    )
    
    # foreign worke
  } else if(col.sgn == "V20" || col.sgn == "IS_FOREIGN_WORKER"){
    lvls <- c(
      "yes", 
      "no"         
    )
    
#   # Relevel factor only if its values have been recoded (they are not raw data)
#   } else if(col.sgn == "RES"){
#     lvls <- c(
#       0, 
#       1         
#     )

  } else{
    # Return original values wector 
    #message("Undefined column name.")
    return(col.vals)
  }
  
  # Build factor with defined order of levels 
  col.vals.res <- factor(col.vals, levels = lvls, labels = lvls)
  return(col.vals.res)
}



#' -----------------------------------------------------------------------------
#' Performs equal frequency discretization of continuous variable. 
#' 
#' x - vector of continuous values to be discretized
#' n - number of bins 
#' 
cutEqual <- function(x, n, include.lowest = TRUE, ...){
  
  require(lattice)
  cut(x, co.intervals(x, n, 0)[c(1, (n+1):(n*2))], 
      include.lowest = include.lowest, ...)
}




#' -----------------------------------------------------------------------------
#' Performs discretization of continuous variable with the use of 
#' rpart tree with default settings. 
#' 
#' x - vector of continuous values to be discretized
#' y - vector of response value aganist which the model is to be built
#' 
cutRpart <- function(x, y){
  
  require(rpart)
  fit <- rpart(y ~ x, method="class")
  fit.cutoffs <- sort(unique(fit$splits[, "index"]))
  fit.cutoffs.cor <- c(min(x, na.rm = TRUE), fit.cutoffs, max(x, na.rm = TRUE))
  cut(x, breaks = fit.cutoffs.cor, include.lowest = TRUE)
}



