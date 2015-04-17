#' @author 
#' Marta Karaś, Jan Idziak 
#' 
#' @description
#' In this part of analysis we operate over cleaned and pre-preocessed data sets: 
#'  - "./data/german_data_cat.txt" - categorized (binned) data set 
#'  - "./data/german_data_woe.txt" - recoded to WoE data set   
#' to build classification models for German Credit data. 
#' 
#' 
#' Types of models: 
#'  - logistic regression (simple model, cost-sensitive model)
#'  - ... 




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# READ DATA 

german.cat <- 
  read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 3), rep("character", 17)))

german.woe <- 
  read.table("./data/german_data_woe.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 20)))