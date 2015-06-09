
#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' The script performs preprocessing STEP 2. of German Credit dataset.
#' 
#' 
#' Includes:
#'  - data formattng
#'  - searching for and dealing with missing, corrupt and invalid data
#'  - creating derived variables
#'  - binning countinuous variables
#'  - correcting bins (levels) of categorical variables 
#'    (combining levels if needed)
#'  - recoding data to WoE
#'  
#'  
#'  Results in: 
#'  - "./data/german_data_cat.txt" - categorized (binned) data set 
#'  - "./data/german_data_woe.txt" - recoded to WoE data set 





# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# READ DATA 

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



# We look for any outliers, invalid data etc. among numeric variables 

# DURATION
x.vec <- gcredit$DURATION
x <- seq(min(x.vec)-1, max(x.vec)+1, length.out = 1000)

par(mfrow=c(1,3))
plot(density(x.vec), main = "DURATION", lwd=2)
fit <- fitdistr(x.vec, "Gamma") ## fitting gamma pdf parameters
lines(x, dgamma(x, rate=fit$estimate["rate"], shape=fit$estimate["shape"]), col = 2, lwd = 2)
legend("topright", legend = c("estimator", "Gamma"), col = c(1,2), cex = 1, lwd = 3)

plot(density(x.vec), main = "DURATION", lwd=2)
fit <- fitdistr(x.vec, "log-normal") ## fitting gamma pdf parameters
lines(x, dlnorm(x, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]), col = 3, lwd = 2)
legend("topright", legend = c("estimator", "log normal"), col = c(1,3), cex = 1, lwd = 3)

plot(density(x.vec), main = "DURATION", lwd=2)
fit <- fitdistr(x.vec, "weibull") ## fitting gamma pdf parameters
lines(x, dweibull(x, shape =fit$estimate["shape"], scale = fit$estimate["scale"]), col = 4, lwd = 2)
legend("topright", legend = c("estimator", "Weibull"), col = c(1,4), cex = 1, lwd = 3)

# Print best results
fitdistr(x.vec, "log-normal")


# AMOUNT
x.vec <- gcredit$AMOUNT
x <- seq(min(x.vec)-1, max(x.vec)+1, length.out = 1000)

par(mfrow=c(1,3))
plot(density(x.vec), main = "AMOUNT", lwd=2)
fit <- fitdistr(x.vec, "Gamma") ## fitting gamma pdf parameters
lines(x, dgamma(x, rate=fit$estimate["rate"], shape=fit$estimate["shape"]), col = 2, lwd = 2)
legend("topright", legend = c("estimator", "Gamma"), col = c(1,2), cex = 1, lwd = 3)

#par(mfrow=c(1,2))
plot(density(x.vec), main = "AMOUNT", lwd=2)
fit <- fitdistr(x.vec, "log-normal") ## fitting gamma pdf parameters
lines(x, dlnorm(x, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]), col = 3, lwd = 2)
legend("topright", legend = c("estimator", "log normal"), col = c(1,3), cex = 1, lwd = 3)

plot(density(x.vec), main = "AMOUNT", lwd=2)
fit <- fitdistr(x.vec, "weibull") ## fitting gamma pdf parameters
lines(x, dweibull(x, shape =fit$estimate["shape"], scale = fit$estimate["scale"]), col = 4, lwd = 2)
legend("topright", legend = c("estimator", "Weibull"), col = c(1,4), cex = 1, lwd = 3)

# Print best results
fitdistr(x.vec, "log-normal")



# AGE
x.vec <- gcredit$AGE
x <- seq(min(x.vec)-1, max(x.vec)+1, length.out = 1000)

par(mfrow=c(1,3))
plot(density(x.vec), main = "AGE", lwd=2)
fit <- fitdistr(x.vec, "Gamma") ## fitting gamma pdf parameters
lines(x, dgamma(x, rate=fit$estimate["rate"], shape=fit$estimate["shape"]), col = 2, lwd = 2)
legend("topright", legend = c("estimator", "Gamma"), col = c(1,2), cex = 1, lwd = 3)

plot(density(x.vec), main = "AGE", lwd=2)
fit <- fitdistr(x.vec, "log-normal") ## fitting gamma pdf parameters
lines(x, dlnorm(x, meanlog = fit$estimate["meanlog"], sdlog = fit$estimate["sdlog"]), col = 3, lwd = 2)
legend("topright", legend = c("estimator", "log normal"), col = c(1,3), cex = 1, lwd = 3)

plot(density(x.vec), main = "AGE", lwd=2)
fit <- fitdistr(x.vec, "weibull") ## fitting gamma pdf parameters
lines(x, dweibull(x, shape =fit$estimate["shape"], scale = fit$estimate["scale"]), col = 4, lwd = 2)
legend("topright", legend = c("estimator", "Weibull"), col = c(1,4), cex = 1, lwd = 3)

# Print best results
fitdistr(x.vec, "log-normal")

summary(gcredit)



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
corrgram(gcredit[1:7], order=TRUE, lower.panel=panel.pie,
         upper.panel=NULL, text.panel=panel.txt,
         cex.labels = 0.6, cor.method='pearson')

# Idea: AMOUNT/DURATION variable? (~ "monthly burden")
gcredit$AMOUNT_TO_DURATION <- gcredit$AMOUNT/gcredit$DURATION
gcredit$DURATION_TO_AGE <- gcredit$DURATION/gcredit$AGE
gcredit$AMOUNT_TO_AGE <- gcredit$AMOUNT/gcredit$AGE

ggplot(gcredit, aes(factor(RES), AMOUNT)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "AMOUNT boxplot", x="RES", y="AMOUNT")
ggplot(gcredit, aes(factor(RES), DURATION)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "DURATION boxplot", x="RES", y="DURATION")
ggplot(gcredit, aes(factor(RES), AGE)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "AGE boxplot", x="AGE", y="DURATION")

ggplot(gcredit, aes(factor(RES), AMOUNT_TO_DURATION)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "AMOUNT_TO_DURATION boxplot", x="RES", y="AMOUNT_TO_DURATION")
ggplot(gcredit, aes(factor(RES), DURATION_TO_AGE)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "DURATION_TO_AGE boxplot", x="RES", y="DURATION_TO_AGE")
ggplot(gcredit, aes(factor(RES), AMOUNT_TO_AGE)) + geom_boxplot(aes(fill = factor(RES))) + 
  scale_fill_discrete(name = "RES") + labs(title = "AMOUNT_TO_AGE boxplot", x="RES", y="AMOUNT_TO_AGE")



#' -----------------------------------------------------------------------------
#' Binning countinuous variables: 
#'   - compare Information Value from equal frequency discretization and 
#'     optimal discretization
#'     
#'   - why to categorize variables? 
#'     * http://blog.revolutionanalytics.com/2015/03/r-package-smbinning-optimal-binning-for-scoring-modeling.html
#'     * http://www.rcreditscoring.com/binning-continuous-variables-in-r-the-basics/

# Define subset of numeric variables 
var.to.cat.names <- c("AGE", "AMOUNT_TO_AGE", "AMOUNT_TO_DURATION","AMOUNT", "DURATION", "DURATION_TO_AGE")
gcredit.quan <- gcredit[,c(var.to.cat.names, "RES")]

# Categorize variables in 3 ways. Keep categorized variable in a data frame
for(var.name in var.to.cat.names){
  
  print(var.name)
  
  x <- gcredit.quan[, var.name]
  y <- gcredit.quan[, "RES"]
  
  # Categorize variable with optimal binning with conditional tree 
  # (smbinning package)
  mincriterion.tmp = 0.95
  binning.res <- smbinning.custom(df=gcredit.quan, y="RES", x=var.name, 
                                  mincriterion = mincriterion.tmp)
  # Check if we got requested num of bins 
  while(binning.res == "No Bins"){#} || (length(binning.res$bands)-1) < min.smbin.bins.num){
    mincriterion.tmp = mincriterion.tmp - 0.05
    print(mincriterion.tmp)
    binning.res <- smbinning.custom(df=gcredit.quan, y="RES", x=var.name, 
                                    mincriterion = mincriterion.tmp)
  }
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
  gcredit.quan[, paste0(var.name, "_equal_nbins")] <- cutEqual(x, n.bins)
  gcredit.quan[, paste0(var.name, "_equal_nrparts")] <- cutEqual(x, n.rparts)
}


# # Compare information value of those 
# # download woe-master.zip from https://github.com/tomasgreif/woe and unzip it;
# # the install from source: 
# #install.packages("/home/martakarass/Downloads/woe-master", repos = NULL, type="source")
# library(woe)
iv.df <- iv.mult(gcredit.quan, "RES", TRUE)
iv.df

# Build data frame to compare information values
iv.comp.df <- data.frame(var.name = numeric(0), 
                         cat.sgn = numeric(0),
                         iv = numeric(0))
for(i in 1:(nrow(iv.df))){
  n.tmp <- nrow(iv.comp.df)
  var.name.tmp <- iv.df[i, "Variable"]
  
  if(grepl("_bin", var.name.tmp)){
    iv.comp.df[n.tmp+1, ] <- c(str_replace(var.name.tmp, "_bin", ""), "smbinning", iv.df[i, "InformationValue"])
  } else if (grepl("_equal_nbins", var.name.tmp)){
    iv.comp.df[n.tmp+1, ] <- c(str_replace(var.name.tmp, "_equal_nbins", ""), "equal_nbins", iv.df[i, "InformationValue"])
  } else if (grepl("_equal_nrparts", var.name.tmp)){
    iv.comp.df[n.tmp+1, ] <- c(str_replace(var.name.tmp, "_equal_nrparts", ""), "equal_nrparts", iv.df[i, "InformationValue"])
  } else if (grepl("_rpart", var.name.tmp)){
    iv.comp.df[n.tmp+1, ] <- c(str_replace(var.name.tmp, "_rpart", ""), "rpart", iv.df[i, "InformationValue"])
  }   
}

iv.comp.df$iv <- round(as.numeric(iv.comp.df$iv), 3)
iv.comp.df$var.name <- factor(iv.comp.df$var.name , levels = var.to.cat.names)
dput(iv.comp.df, "./data/iv-comparision-df")
rm(iv.comp.df)

# Plot
iv.comp.df <- dget("./data/iv-comparision-df")
ggplot(iv.comp.df, aes(var.name, iv, group = cat.sgn, colour = cat.sgn)) + 
  geom_line() + theme(axis.text.x = element_text(angle = 10, hjust = 1)) + 
  scale_fill_discrete(name = "approach sgn") + 
  labs(title = "AMOUNT_TO_AGE boxplot", x="RES", y="AMOUNT_TO_AGE")




gcredit.cat <- data.frame(RES = gcredit.quan$RES,
                          DURATION = gcredit.quan$DURATION_bin,
                          AGE = gcredit.quan$AGE_bin,
                          AMOUNT = gcredit.quan$AMOUNT_bin,
                          AMOUNT_TO_DURATION = gcredit.quan$AMOUNT_TO_DURATION_bin,
                          DURATION_TO_AGE = gcredit.quan$DURATION_TO_AGE_bin,
                          AMOUNT_TO_AGE = gcredit.quan$AMOUNT_TO_AGE_bin)


#' -----------------------------------------
#' Joining levels of categorical variables if reasonable. 
#' 
#' The point is that maybe some levels of categorical variables
#' are very similar in terms of rate of bad clients AND they are 
#' reasonalby similar in terms of their meaning (e.g. for PURPOSE variable
#' its levels: "car (new)" and "car (old)"). Maybe 
#' it is worth to "join" levels of these variables
#' in order to reduce "mess" in data. 


y <- gcredit$RES

# ---------------
# gcredit$PURPOSE ******************

x.tmp <- gcredit$PURPOSE
res.df.tmp <- infval.comb.levels.effect(y, x.tmp, mosaic.plot = FALSE)
head(res.df.tmp)
iv.plot.woe(iv.mult(gcredit,"RES",vars=c("PURPOSE"),summary=FALSE))
# Połączenie leveli i zapamiętanie zmiennej
x.tmp <- combine.factor.lvls(x.tmp, c(res.df.tmp[2,1], res.df.tmp[2,2]))
gcredit.cat$PURPOSE <- x.tmp



# ---------------
# gcredit$RATE_TO_DISP_INCOME

x.tmp <- gcredit$RATE_TO_DISP_INCOME
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# Hopeless variable :P 
gcredit.cat$RATE_TO_DISP_INCOME <- x.tmp



# ---------------
# gcredit$RESIDENCE

x.tmp <- gcredit$RESIDENCE
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# Hopeless variable :P
gcredit.cat$RESIDENCE <- x.tmp



# ---------------
# gcredit$NUM_OF_THIS_BANK_CREDITS

x.tmp <- gcredit$NUM_OF_THIS_BANK_CREDITS
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# Hopeless variable :P
gcredit.cat$NUM_OF_THIS_BANK_CREDITS <- x.tmp



# ---------------
# gcredit$NUM_OF_MAINTAINED_PEOPLE

x.tmp <- gcredit$NUM_OF_MAINTAINED_PEOPLE
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# Hopeless variable :P
gcredit.cat$NUM_OF_MAINTAINED_PEOPLE <- x.tmp



# ---------------
# gcredit$CHK_ACCT

x.tmp <- gcredit$CHK_ACCT
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# good variable; we do not change nothing!! :) 
gcredit.cat$CHK_ACCT <- x.tmp



# ---------------
# gcredit$HISTORY

x.tmp <- gcredit$HISTORY
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# no logical reason to join anything :P 
gcredit.cat$HISTORY <- x.tmp



# ---------------
# gcredit$SAVINGS_ACCT

x.tmp <- gcredit$SAVINGS_ACCT
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# we do not join "<100 DM" AND "100-500 DM" because 
# "<100 DM" is relatively numerous in comparision to others 
gcredit.cat$SAVINGS_ACCT <- x.tmp



# ---------------
# gcredit$EMLOYMENT

x.tmp <- gcredit$EMLOYMENT
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
# we do not join e.g. "4-7 years" and ">= 7 years"
# because we would disorder frequencies in the levels
gcredit.cat$EMLOYMENT <- x.tmp


# ---------------
# gcredit$STATUS_AND_SEX

x.tmp <- gcredit$STATUS_AND_SEX
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))

# SEX
gcredit$SEX <- sapply(gcredit$STATUS_AND_SEX, function(val){
  ifelse(grepl("female",val), "female", "male")
})
gcredit$SEX <- as.factor(gcredit$SEX)
x.tmp <- gcredit$SEX
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))

gcredit$STATUS <- as.factor(gcredit$STATUS)
# nope i do not touch this strange not nice variable :P 
gcredit.cat$STATUS_AND_SEX <- gcredit$STATUS_AND_SEX



# ---------------
# gcredit$OTHER_DEBTORS

x.tmp <- gcredit$OTHER_DEBTORS
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$OTHER_DEBTORS <- x.tmp



# ---------------
# gcredit$PROPERTY

x.tmp <- gcredit$PROPERTY
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$PROPERTY <- x.tmp



# ---------------
# gcredit$OTHER_INSTALLMENT_PLANS

x.tmp <- gcredit$OTHER_INSTALLMENT_PLANS
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$OTHER_INSTALLMENT_PLANS <- x.tmp



# ---------------
# gcredit$HOUSING

x.tmp <- gcredit$HOUSING
res.df.tmp <- infval.comb.levels.effect(y, x.tmp)
head(res.df.tmp)
iv.plot.woe(iv.mult(gcredit,"RES",vars=c("HOUSING"),summary=FALSE))

# Połączenie leveli i zapamiętanie zmiennej
x.tmp <- combine.factor.lvls(x.tmp, c(res.df.tmp[2,1], res.df.tmp[2,2]))
gcredit.cat$HOUSING <- x.tmp




# ---------------
# gcredit$JOB

x.tmp <- gcredit$JOB
res.df.tmp <- infval.comb.levels.effect(y, x.tmp)
head(res.df.tmp)
iv.plot.woe(iv.mult(gcredit,"RES",vars=c("JOB"),summary=FALSE))

x.tmp <- combine.factor.lvls(x.tmp, c(res.df.tmp[4,1], res.df.tmp[4,2]))
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$JOB <- x.tmp





# ---------------
# gcredit$TELEPHONE

x.tmp <- gcredit$TELEPHONE
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$TELEPHONE <- x.tmp



# ---------------
# gcredit$IS_FOREIGN_WORKER

x.tmp <- gcredit$IS_FOREIGN_WORKER
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))
gcredit.cat$IS_FOREIGN_WORKER <- x.tmp





# ------------------------------------------------------------------------------
# Recode to WoE

# iv.num  - calculate WoE/IV for numeric variables
# iv.str - calculate WoE/IV for character/factor variables
# iv.mult - calculate WoE/IV, summary IV for one or more variables
# iv.plot.summary - plot IV summary
# iv.plot.woe - plot WoE patterns for one or more variables
# iv.replace.woe - recode original variables to WoE (adds new columns)


(iv.mult.res <- iv.mult(gcredit.cat, "RES", TRUE))


# Variable InformationValue Bins ZeroBins    Strength
# 1                  CHK_ACCT      0.666011503    4        0 Very strong
# 2                   HISTORY      0.293233547    5        0      Strong
# 3                  DURATION      0.229296148    3        0      Strong
# 4                    AMOUNT      0.225097717    4        0      Strong
# 5              SAVINGS_ACCT      0.196009557    5        0     Average
# 6                   PURPOSE      0.169155274    9        0     Average
# 7        AMOUNT_TO_DURATION      0.114469395    6        0     Average
# 8                  PROPERTY      0.112638262    4        0     Average
# 9                 EMLOYMENT      0.086433631    5        0        Weak
# 10                  HOUSING      0.082950783    2        0        Weak
# 11                      AGE      0.073166424    2        0        Weak
# 12  OTHER_INSTALLMENT_PLANS      0.057614542    3        0        Weak
# 13           STATUS_AND_SEX      0.044670678    4        0        Weak
# 14        IS_FOREIGN_WORKER      0.043877412    2        0        Weak
# 15            OTHER_DEBTORS      0.032019322    3        0        Weak
# 16      RATE_TO_DISP_INCOME      0.023858552    2        0        Weak
# 17 NUM_OF_THIS_BANK_CREDITS      0.010083557    2        0   Wery weak
# 18                      JOB      0.008095050    3        0   Wery weak
# 19                TELEPHONE      0.006377605    2        0   Wery weak
# 20 NUM_OF_MAINTAINED_PEOPLE      0.000000000    1        0   Wery weak
# 21                RESIDENCE      0.000000000    1        0   Wery weak



#' At this point we REMOVE:
#' - NUM_OF_MAINTAINED_PEOPLE
#' - RESIDENCE
#' variables
#' 
#' 

#dput(gcredit.cat, file = "./data/german_data_cat-before-removal.txt" )
gcredit.cat.before.removal <- dget("./data/german_data_cat-before-removal.txt")
iv.mult.res <- iv.mult(gcredit.cat.before.removal,"RES", TRUE)
print(gcredit.cat.before.removal)


col.to.remove.idx <- which(names(gcredit.cat) %in% c("NUM_OF_MAINTAINED_PEOPLE", "RESIDENCE"))
gcredit.cat <- gcredit.cat[, -col.to.remove.idx]
(iv.mult.res <- iv.mult(gcredit.cat,"RES", TRUE))




# ------------------------------------------------------------------------------
# Save categorized data to a file

# Reorder columns in data frame to have firsty numeric, then factor variables
num.cols <- which(sapply(gcredit.cat, class) == "numeric")
gcredit.cat <- gcredit.cat[c(num.cols, setdiff(1:ncol(gcredit.cat), num.cols))]

# Save tmp version of the data 
write.table(x = gcredit.cat, file = "./data/german_data_cat.txt", 
            sep=",",  col.names=TRUE, row.names = FALSE)

# # Read data  
# gcredit.cat <- 
#   read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
#              colClasses=c(rep("numeric", 3), rep("character", 17)))



iv.plot.summary(iv.mult.res) # very nice plot! :) 


# WoE patterns for "Very strong" / "Strong" variables
iv.plot.woe(iv.mult(gcredit.cat,"RES",vars=c("CHK_ACCT"),summary=FALSE))
iv.plot.woe(iv.mult(gcredit.cat,"RES",vars=c("HISTORY"),summary=FALSE))
iv.plot.woe(iv.mult(gcredit.cat,"RES",vars=c("DURATION"),summary=FALSE))
iv.plot.woe(iv.mult(gcredit.cat,"RES",vars=c("AMOUNT"),summary=FALSE))
iv.plot.woe(iv.mult(gcredit.cat,"RES",vars=c("AMOUNT"),summary=FALSE))

# Replace WoE for all variables
gcredit.woe <- iv.replace.woe(gcredit.cat, iv.mult(gcredit.cat,"RES"))
gcredit.woe <- gcredit.woe[, c(which(names(gcredit.woe) == "RES"), 
                               grep("woe", names(gcredit.woe)))]



# ------------------------------------------------------------------------------
# Save recoded to WoE data to a file 
write.table(x = gcredit.woe, file = "./data/german_data_woe.txt", 
            sep=",",  col.names=TRUE, row.names = FALSE)