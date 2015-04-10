
#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' The script performs preprocessing STEP 2. of German Credit dataset.
#' 
#' Includes:
#'  - data formattng
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
             colClasses=c(rep("numeric", 8), rep("character", 13)))

# Relevel factors to keep original "order"
for(col.name in names(gcredit)) 
  gcredit[, col.name] <- gcredit.convert.to.factor(col.name, gcredit[, col.name])




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
#' Observation: 
#' there's no "NA" values or values we did not notice while converting to factor


# We look for any outliers, invalid data etc. among numeric variables 
summary(gcredit$DURATION)
plot(density(gcredit$DURATION))

summary(gcredit$AMOUNT)
plot(density(gcredit$AMOUNT)) # Interesting

summary(gcredit$RATE_TO_DISP_INCOME)
plot(density(gcredit$RATE_TO_DISP_INCOME))

summary(gcredit$RESIDENCE)
plot(density(gcredit$RESIDENCE))

summary(gcredit$AGE)
plot(density(gcredit$AGE)) # Interesting 

summary(gcredit$NUM_OF_MAINTAINED_PEOPLE)
plot(density(gcredit$NUM_OF_MAINTAINED_PEOPLE))
#' Observation: 
#' there is no obviously "bad" observations or strange distributions




#' --------------------------
#' Creating derived variables


# Check the correlations
#install.packages("corrgram")
library(corrgram)
corrgram(gcredit[1:7], order=TRUE, lower.panel=panel.pie,
         upper.panel=NULL, text.panel=panel.txt,
         cex.labels = 0.6, cor.method='pearson')

# Idea: AMOUNT/DURATION variable? (~ "monthly burden")
gcredit$AMOUNT_TO_DURATION <- gcredit$AMOUNT/gcredit$DURATION
summary(gcredit$AMOUNT_TO_DURATION)
plot(density(gcredit$AMOUNT_TO_DURATION)) # Interesting: very long tail! 
boxplot(gcredit$AMOUNT_TO_DURATION)




#' --------------------------
#' Binning countinuous variables: 
#'   - compare Information Value from equal frequency discretization and 
#'     optimal discretization
#'     
#'   - why to categorize variables? 
#'     * http://blog.revolutionanalytics.com/2015/03/r-package-smbinning-optimal-binning-for-scoring-modeling.html
#'     * http://www.rcreditscoring.com/binning-continuous-variables-in-r-the-basics/

# Define subset of numeric variables 
var.to.cat.names <- c("DURATION", "AMOUNT", "AGE")
gcredit.quan <- gcredit[c(var.to.cat.names, "RES")]

# # Save subset of numeric variables 
# write.table(x = gcredit.quan, file = "./data/german_quan.txt", 
#             sep=",",  col.names=TRUE, row.names = FALSE)
# rm(gcredit.quan)
# gcredit.quan <- read.table("./data/german_quan.txt", sep=",", header =TRUE,  
#                            colClasses=rep("numeric", 4))
# var.to.cat.names <- setdiff(names(gcredit.quan), "RES")



# Categorize variables in 3 ways
for(var.name in var.to.cat.names){
  
  print(var.name)
  
  x <- gcredit.quan[, var.name]
  y <- gcredit.quan[, "RES"]
  
  # Categorize variable with optimal binning with conditional tree 
  # (smbinning package)
  binning.res <- smbinning(df=gcredit.quan, y="RES", x=var.name, p=0.05)
  binned.var.name <- paste0(var.name, "_bin")
  gcredit.quan <- smbinning.gen(gcredit.quan, binning.res, binned.var.name)
  # Correct new variable to be a factor
  binned.var.levels <- sort(unique(gcredit.quan[, binned.var.name]))
  gcredit.quan[, binned.var.name] <- factor(gcredit.quan[, binned.var.name], 
                                            levels = binned.var.levels, 
                                            labels = binned.var.levels)
  n.bins <- length(binning.res$bands)-1
  
  # Categorize variable with simple tree model (rpart package)
  rparted.var <- cutRpart(x, y)
  gcredit.quan[, paste0(var.name, "_rpart")] <- rparted.var
  n.rparts <- length(levels(rparted.var))
  
  # Categorize variable with equal frequency discretization
  # (for defined num of bins we got in 1. optimal binning and 2. rpart binning)
  gcredit.quan[, paste0(var.name, "_equal1")] <- cutEqual(x, n.bins)
  gcredit.quan[, paste0(var.name, "_equal2")] <- cutEqual(x, n.rparts)
}



# Compare information value of those 
# download woe-master.zip from https://github.com/tomasgreif/woe and unzip it;
# the install from source: 
#install.packages("/home/martakarass/Downloads/woe-master", repos = NULL, type="source")
library(woe)
iv.df <- iv.mult(gcredit.quan, "RES", TRUE)
iv.df

# Build data frame to compare information values
iv.comparision.df <- data.frame(var.name = numeric(0), 
                                cat.sgn = numeric(0),
                                iv = numeric(0))
for(name in var.to.cat.names){
  
  # optimal binning
  idx.tmp <- which(iv.df[, "Variable"] == paste0(name, "_bin"))
  iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
    c(name, "smbinning", iv.df[idx.tmp, "InformationValue"])
  
  # rpart binning
  idx.tmp <- which(iv.df[, "Variable"] == paste0(name, "_rpart"))
  iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
    c(name, "rpart", iv.df[idx.tmp, "InformationValue"])
  
  # best cat 
  idx.tmp <- which(grepl(paste0(name, "_equal"), iv.df[, "Variable"]))
  iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
    c(name, "equal",  max(iv.df[idx.tmp, "InformationValue"]))  
}


# Plot comparision
library(ggplot2)
ggplot(iv.comparision.df, aes(var.name, iv, group = cat.sgn, 
                              colour = cat.sgn)) + geom_line()

#' Observation: 
#' smbinning does it pretty well and we may include those variables 
#' as new ones :D 

gcredit$DURATION_cat <- gcredit.quan$DURATION_bin
gcredit$AGE_cat <- gcredit.quan$AGE_bin
gcredit$AMOUNT_cat <- gcredit.quan$AMOUNT_bin




#' -----------------------------------------
#' Joining levels of categorical variables if reasonable. 
#' 
#' The point is that maybe some levels of categorical variables
#' are very similar in terms of rate of bad clients AND they are 
#' reasonalby similar in terms of their meaning (e.g. for PURPOSE variable
#' its levels: "car (new)" and "car (old)"). Maybe 
#' it is worth to "join" levels of these variables
#' in order to reduce "mess" in data. 

# good graphical tool 
par(las=2) # for labels always perpendicular to the axis,
mosaicplot(table(gcredit$PURPOSE, gcredit$RES))
#' e.g. here we may consider joining "domestic appliances"
#' and "repairs" etc. 
