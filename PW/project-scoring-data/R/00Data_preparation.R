gcredit.cat <- 
  read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 1), rep("character", 21)))

gcredit.cat$RES <- as.factor(gcredit.cat$RES)

gcredit.woe <- 
  read.table("./data/german_data_woe.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 22)))
gcredit.cat$RES <- as.factor(gcredit.cat$RES)
# Define full formula
gcredit.cat.f.full <- as.formula("RES ~ .")

data <- gcredit.cat
data$RES <- ifelse(data$RES == 1, "GOOD", "BAD")
data$RES <- as.factor(data$RES)
data_woe <- gcredit.woe
data_woe$RES <- ifelse(data_woe$RES == 1, "GOOD", "BAD")
data_woe$RES <- as.factor(data_woe$RES)

inTrain <- createDataPartition(data$RES, p =0.7, list = F)
training <- data[inTrain, ]
testing <- data[-inTrain, ]
training_woe <- data_woe[inTrain, ]
testing_woe <- data_woe[-inTrain, ]
