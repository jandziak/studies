
# Clustering analysis 

gcredit.cat <- 
  read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 1), rep("character", 21)))


gcredit.woe <- 
  read.table("./data/german_data_woe.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 22)))
