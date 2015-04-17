
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
      "male : married/widowed"
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





#' -----------------------------------------------------------------------------
#' Function to get varable Information Value
#' 
#' @param y - vector with binary outcome (integer or factor)
#' @param x - variable vector 
#' 
#' @return information value of x 
#' 
infval <- function(y, x){
  
  require(woe)
  iv.mult(data.frame(x=x, y=y), "y", TRUE)[, "InformationValue"]
} 





#' -----------------------------------------------------------------------------
#' Function to join (combine together) selected levels of a factor variable 
#' into one level.
#' 
#' @param x - factor variable
#' @param levels.to.join - vector of levels to be combined
#' @param new.level.name - name of a new level
#' 
#' @return factor variable with selected levels combined
#' 
combine.factor.lvls <- function(x, levels.to.join, new.level.name=NULL){
  
  # Check if levels of the variable wchich are pointed out to be
  # joined are contained in the variable unique values 
  if(!(all(levels.to.join %in% unique(x)))){
    stop("Incorrect names of variable levels")
  }
  
  # If not given, define new level name
  if(is.null(x = new.level.name)){    
    # max length od new wariable name 
    max.new.level.name.l <- 30
    new.level.name <- 
      paste0(substr(levels.to.join, 0, floor(max.new.level.name.l/length(levels.to.join))),                            
             collapse = "_")
  }
  
  # Define levels of the new variable
  levels.tmp <- levels(x)
  levels.to.join.idx <- which(levels.tmp %in% levels.to.join)
  levels.new <- c(levels.tmp[-levels.to.join.idx], new.level.name)
  
  # Create the new variable which is simply an "old" factor, 
  # but with selectet levels joined
  x.new <- as.character(x)
  x.new[x.new %in% levels.to.join] <- new.level.name
  factor(x.new, levels = levels.new, labels = levels.new) 
}





#' -----------------------------------------------------------------------------
#' For every pair of levels of a factor, function combines 
#' this levels into one new level and compute information value
#' of this new variable. 
#' 
#' @param y - vector with binary outcome (integer or factor)
#' @param x - variable vector 
#' @param mosaic.table - should mosaic table x by y be printed? 
#' @param mosaic.plot - should mosaic plot x by y be plotted? 
#' 
#' @return a data frame with three colums, containig: 
#'         - name of the first of the two levels which are combined
#'         - name of the second of the two levels which are combined
#'         - information value of a variable vector for which those two
#'           levels have been combined
#'            
infval.comb.levels.effect <- function(y, x, mosaic.table = TRUE, mosaic.plot = FALSE){
  
  # Define result data frame
  res.df <- data.frame(lvl1 = numeric(0), lvl2 = numeric(0), iv = numeric(0))
  
  # Generate all pairs of factor variable levels 
  lvl.pairs <- t(combn(unique(x), 2))
  
  for(row.i in 1:nrow(lvl.pairs)){
    
    # Define vector of levels to be combined
    lvl1.tmp <- as.character(lvl.pairs[row.i,1])
    lvl2.tmp <- as.character(lvl.pairs[row.i,2])
    
    # Build new variable with selectet levels joined
    x.tmp <- combine.factor.lvls(x, c(lvl1.tmp, lvl2.tmp))
    # Get information value of a new variable 
    infval.tmp <- infval(y, x.tmp)
    
    # Add row for temporary pair of levels to result data frame
    df.tmp <- data.frame(lvl1 = c(lvl1.tmp), lvl2 = c(lvl2.tmp), iv = c(infval.tmp))
    res.df <- rbind(res.df, df.tmp)  
  }
  
  # Add row with information value without combining levels
  infval.tmp <- infval(y, as.character(x))
  df.tmp <- data.frame(lvl1 = c("NONE"), lvl2 = c("NONE"), iv = c(infval.tmp))
  res.df <- rbind(res.df, df.tmp)  
  
  
  t <- table(x, y)
  # print mosaic table if requested
  if(mosaic.table){
    message("mosaic table")
    print(t)
  }
  
  # plot mosaic plot if requested
  if(mosaic.plot){
    par(las=2) # for labels always perpendicular to the axis
    mosaicplot(t)
  }
    
  # Return result data frame orsered by iv (descending) 
  res.df <- res.df[order(-res.df[,3]), ] 
  res.df[,1] <- as.character(res.df[,1])
  res.df[,2] <- as.character(res.df[,2])
  res.df
}





#' -----------------------------------------------------------------------------
#' Customized smbinning function from `smbinning` package. 
#' The only change to original version depends on adding an mincriterion 
#' parameter. 
#' 
smbinning.custom <- 
  function (df, y, x, p = 0.05, mincriterion = 0.95) 
  {
    if (!is.data.frame(df)) {
      return("Data is not a data.frame")
    }
    else if (is.numeric(y) | is.numeric(x)) {
      return("Characteristic name not string")
    }
    else i = which(names(df) == y)
    j = which(names(df) == x)
    if (!is.numeric(df[, i])) {
      return("Target (y) not found or it is not numeric")
    }
    else if (max(df[, i], na.rm = T) != 1) {
      return("Maximum not 1")
    }
    else if (fn$sqldf("select count(*) from df where cast($x as text)='Inf' or cast($x as text)='-Inf'") > 
               0) {
      return("Characteristic (x) with an 'Inf' value (Divided by Zero). Replace by NA")
    }
    else if (min(df[, i], na.rm = T) != 0) {
      return("Minimum not 0")
    }
    else if (p <= 0 | p > 0.5) {
      return("p must be greater than 0 and lower than 0.5 (50%)")
    }
    else if (!is.numeric(df[, j])) {
      return("Characteristic (x) not found or it is not a number")
    }
    else if (length(unique(df[, j])) < 10) {
      return("Characteristic (x) has less than 10 uniques values")
    }
    else {
      ctree = ctree(formula(paste(y, "~", x)), data = df, na.action = na.exclude, 
                    control = ctree_control(minbucket = ceiling(round(p * nrow(df))),
                                            mincriterion=mincriterion))
      bins = width(ctree)
      if (bins < 2) {
        return("No Bins")
      }
      cutvct = data.frame(matrix(ncol = 0, nrow = 0))
      n = length(ctree)
      for (i in 1:n) {
        cutvct = rbind(cutvct, ctree[i]$node$split$breaks)
      }
      cutvct = cutvct[order(cutvct[, 1]), ]
      ivt = data.frame(matrix(ncol = 0, nrow = 0))
      n = length(cutvct)
      for (i in 1:n) {
        cutpoint = cutvct[i]
        ivt = rbind(ivt, fn$sqldf("select '<= $cutpoint' as Cutpoint,\n                  NULL as CntRec,\n                  NULL as CntGood,\n                  NULL as CntBad,\n                  sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,\n                  sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,\n                  sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,\n                  NULL as PctRec,\n                  NULL as BadRate,\n                  NULL as Odds,\n                  NULL as LnOdds,\n                  NULL as WoE,\n                  NULL as IV\n                  from df where $x is not NULL and $y is not NULL"))
      }
      cutpoint = max(df[, j], na.rm = T)
      mincutpoint = min(df[, j], na.rm = T)
      ivt = rbind(ivt, fn$sqldf("select '<= $cutpoint' as Cutpoint,\n                NULL as CntRec,\n                NULL as CntGood,\n                NULL as CntBad,\n                sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,\n                sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,\n                sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,\n                NULL as PctRec,\n                NULL as BadRate,\n                NULL as Odds,\n                NULL as LnOdds,\n                NULL as WoE,\n                NULL as IV\n                from df where $x is not NULL and $y is not NULL"))
      x.na = fn$sqldf("select count(*) from df where $x is null")
      y.na = fn$sqldf("select count(*) from df where $y is null")
      if (x.na > 0) {
        ivt = rbind(ivt, fn$sqldf("select 'Missing' as Cutpoint,\n                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,\n                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,\n                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,\n                  NULL as CntCumRec,\n                  NULL as CntCumGood,\n                  NULL as CntCumBad,\n                  NULL as PctRec,\n                  NULL as BadRate,\n                  NULL as Odds,\n                  NULL as LnOdds,\n                  NULL as WoE,\n                  NULL as IV\n                  from df where $y is not NULL"))
      }
      else {
        ivt = rbind(ivt, c("Missing", 0, 0, 0, NA, NA, NA, 
                           NA, NA, NA, NA, NA, NA))
      }
      ivt = rbind(ivt, fn$sqldf("select 'Total' as Cutpoint,\n                count(*) as CntRec,\n                sum(case when $y=1 then 1 else 0 end) as CntGood,\n                sum(case when $y=0 then 1 else 0 end) as CntBad,\n                NULL as CntCumRec,\n                NULL as CntCumGood,\n                NULL as CntCumBad,\n                NULL as PctRec,\n                NULL as BadRate,\n                NULL as Odds,\n                NULL as LnOdds,\n                NULL as WoE,\n                NULL as IV\n                from df where $y is not NULL"))
      ncol = ncol(ivt)
      for (i in 2:ncol) {
        ivt[, i] = as.numeric(ivt[, i])
      }
      ivt[1, 2] = ivt[1, 5]
      ivt[1, 3] = ivt[1, 6]
      ivt[1, 4] = ivt[1, 7]
      n = nrow(ivt) - 2
      for (i in 2:n) {
        ivt[i, 2] = ivt[i, 5] - ivt[i - 1, 5]
        ivt[i, 3] = ivt[i, 6] - ivt[i - 1, 6]
        ivt[i, 4] = ivt[i, 7] - ivt[i - 1, 7]
      }
      ivt[2, 2] = ivt[2, 5] - ivt[1, 5]
      ivt[2, 3] = ivt[2, 6] - ivt[1, 6]
      ivt[2, 4] = ivt[2, 7] - ivt[1, 7]
      ivt[i + 1, 5] = ivt[i, 5] + ivt[i + 1, 2]
      ivt[i + 1, 6] = ivt[i, 6] + ivt[i + 1, 3]
      ivt[i + 1, 7] = ivt[i, 7] + ivt[i + 1, 4]
      options(scipen = 999)
      ivt[, 8] = round(ivt[, 2]/ivt[i + 2, 2], 4)
      ivt[, 9] = round(ivt[, 4]/ivt[, 2], 4)
      ivt[, 10] = round(ivt[, 3]/ivt[, 4], 4)
      ivt[, 11] = round(log(ivt[, 3]/ivt[, 4]), 4)
      G = ivt[i + 2, 3]
      B = ivt[i + 2, 4]
      LnGB = log(G/B)
      ivt[, 12] = round(log(ivt[, 3]/ivt[, 4]) - LnGB, 4)
      ivt[, 13] = round(ivt[, 12] * (ivt[, 3]/G - ivt[, 4]/B), 
                        4)
      ivt[i + 2, 13] = round(sum(ivt[, 12] * (ivt[, 3]/G - 
                                                ivt[, 4]/B), na.rm = T), 4)
      iv = ivt[i + 2, 13]
    }
    bands = append(mincutpoint, cutvct)
    bands = append(bands, cutpoint)
    list(ivtable = ivt, iv = iv, ctree = ctree, bands = bands, 
         x = x, col_id = j)
  }