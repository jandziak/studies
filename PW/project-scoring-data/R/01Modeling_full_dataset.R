#' Tuning parameters for boosting algorithm
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 10)


nrow(gbmGrid)

#' Control of training for models 
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)


set.seed(12423)
gbmFit1 <- train(RES ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit1


set.seed(12423)
gbmFit2 <- train(RES ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaludate:
                 tuneGrid = gbmGrid)
gbmFit2


set.seed(12423)
gbmFit3 <- train(RES ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit3


set.seed(12423)
svmFit <- train(RES ~ ., data = training,
                method = "svmRadial",
                trControl = fitControl,
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")
svmFit

set.seed(12423)
rdaFit <- train(RES ~ ., data = training,
                method = "rda",
                trControl = fitControl,
                tuneLength = 4,
                metric = "ROC")
rdaFit




set.seed(12423)
rfFit <- train(RES ~ ., data = training, 
                method='rf',
                trControl = fitControl,
                ntree = 100,
                tuneGrid = data.frame(.mtry = c(4, 7, 10)))
rfFit
glmAIC_Fit

glmFit
knnFit
ldaFit
qdaFit
### Logistic Regresion

set.seed(12423)
glmAIC_Fit <- train(RES ~ ., data = training, 
                             method = "glmStepAIC", 
                             family    = binomial,
                             trControl = fitControl,
                             preProc = c("center", "scale"),
                             trace = 0)

glmAIC_Fit


set.seed(12423)
glmFit <- train(RES ~ ., data       = training,
                method    = "glm",
                family    = binomial,
                trControl = fitControl,
                metric = "ROC")

glmFit
### Modeling using WOE data

set.seed(12423)
knnFit <- train(RES ~ ., data = training_woe, 
                method = "knn", 
                trControl = fitControl, 
                tuneLength = 8)

knnFit

set.seed(12423)
ldaFit <- train(RES ~ ., method='lda', 
                data=training_woe, 
                trControl = fitControl)
ldaFit

set.seed(12423)
qdaFit<- train(RES ~ ., method='qda', 
               data=training_woe, 
               trControl = fitControl)
qdaFit