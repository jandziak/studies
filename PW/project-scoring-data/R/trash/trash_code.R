# # Save subset of numeric variables 
# write.table(x = gcredit.quan, file = "./data/german_quan.txt", 
#             sep=",",  col.names=TRUE, row.names = FALSE)
# rm(gcredit.quan)
# gcredit.quan <- read.table("./data/german_quan.txt", sep=",", header =TRUE,  
#                            colClasses=rep("numeric", 4))
# var.to.cat.names <- setdiff(names(gcredit.quan), "RES")

# Define min number of bins from smbinning package 
#min.smbin.bins.num <- 4






str_replace(var.name.tmp, "_bin", "")


grepl("_bin", var.name.tmp) 

# for(name in var.to.cat.names){
#   
#   # optimal binning
#   idx.tmp <- which(iv.df[, "Variable"] == paste0(name, "_bin"))
#   iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
#     c(name, "smbinning", iv.df[idx.tmp, "InformationValue"])
#   
#   # rpart binning
#   idx.tmp <- which(iv.df[, "Variable"] == paste0(name, "_rpart"))
#   iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
#     c(name, "rpart", iv.df[idx.tmp, "InformationValue"])
#   
#   # best cat 
#   idx.tmp <- which(grepl(paste0(name, "_equal"), iv.df[, "Variable"]))
#   iv.comparision.df[nrow(iv.comparision.df)+1, ] <- 
#     c(name, "equal",  max(iv.df[idx.tmp, "InformationValue"]))  
# }


# Plot comparision
iv.comparision.df$var.name <- factor(iv.comparision.df$var.name, levels = var.to.cat.names)
dput(iv.comparision.df, "./data/iv-comparision-df")
rm(iv.comparision.df)

iv.comparision.df <- dget("./data/iv-comparision-df")
ggplot(iv.comparision.df, aes(var.name, iv, group = cat.sgn, colour = cat.sgn)) + 
  geom_line() + theme(axis.text.x = element_text(angle = 10, hjust = 1))

# Investigate AGE case

iv.mult(gcredit.quan,"RES",TRUE)


iv.num(gcredit.quan, "RES", "AGE_equal1", verbose = FALSE, rcontrol = NULL)
iv.num(gcredit.quan, "RES", "AGE_equal2", verbose = FALSE, rcontrol = NULL)
iv.num(gcredit.quan, "RES", "AGE_equal2")




iv.plot.woe(iv.mult(gcredit.quan, "RES", 
                    vars=c("AGE_equal1", "AGE_bin"),
                    summary=FALSE))

iv.plot.woe(iv.mult(gcredit.quan, "RES", 
                    vars=c("AMOUNT_equal1", "AMOUNT_bin", "AMOUNT_equal2"),
                    summary=FALSE))

iv.plot.woe(iv.mult(gcredit.quan, "RES", 
                    vars=c("AMOUNT_equal1", "AMOUNT_bin"),
                    summary=FALSE))

y <- gcredit$RES
x.tmp <- gcredit$RATE_TO_DISP_INCOME
(res.df.tmp <- infval.comb.levels.effect(y, x.tmp))






gcredit.quan




#' Observation: 
#' smbinning does it pretty well and we may include those variables 
#' as new ones :D 