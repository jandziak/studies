library(stringr)
data <- gcredit.cat
gcredit.cat$RES <- ifelse(gcredit.cat$RES == 1, "GOOD", "BAD")
gcredit.cat$RES <- as.factor(gcredit.cat$RES)
inTrain <- createDataPartition(data$RES, p =0.7, list = F)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

data12 <- data[,c(2,3)]
data <- data[,-c(2,3)]
data1 <- as.data.frame(apply(data,2,function(x)gsub(':', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub(' ', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('-', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('=', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('>', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('<', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('/', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('\\(', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub(')', '_',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('00AMOUNT_TO_DURATION86.2', '00AMOUNT_TO_DURATION86_2',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('02AMOUNT_TO_DURATION291.5', '02AMOUNT_TO_DURATION291_5',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('04AMOUNT_TO_DURATION2482.6', '04AMOUNT_TO_DURATION2482_6',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('01AMOUNT_TO_DURATION250.944444444444', '01AMOUNT_TO_DURATION250_944444444444',x)))
data1 <- as.data.frame(apply(data1,2,function(x)gsub('03AMOUNT_TO_DURATION358.090909090909', '03AMOUNT_TO_DURATION358_090909090909',x)))

data1 <- as.data.frame(apply(data1,2,function(x)gsub(',', '_',x)))
data1 <- cbind(data1,data12)
data <- cbind(data, data12)