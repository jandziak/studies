cfs_Formula <- as.simple.formula(cfs_VARS, "RES")
consistency_Formula <- as.simple.formula(consistency.VARS,"RES")
cfs_Formula_woe <- as.simple.formula(cfs_VARS_woe, "RES")
consistency_Formula_woe <- as.simple.formula(consistency.VARS_woe, "RES")
best_search_formula <- as.simple.formula(information.gain.res_imoportance[1:8,2], "RES")
best_search_formula_woe <- 
  as.simple.formula(paste(information.gain.res_imoportance[1:8,2],"_woe", sep = "")
                     , "RES")


cfs_Formula
consistency_Formula
cfs_Formula_woe
consistency_Formula_woe


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
glmAIC_Fit_cfs <- train(cfs_Formula, data = training, 
                    method = "glmStepAIC", 
                    family    = binomial,
                    trControl = fitControl,
                    preProc = c("center", "scale"),
                    trace = 0)

glmAIC_Fit_cfs

set.seed(12423)
glmFit_cfs <- train(cfs_Formula, data       = training,
                method    = "glm",
                family    = binomial,
                trControl = fitControl,
                metric = "ROC")

glmFit_cfs

### Modeling using WOE data

set.seed(12423)
knnFit_cfs <- train(cfs_Formula_woe, data = training_woe, 
                method = "knn", 
                trControl = fitControl, 
                tuneLength = 8)

knnFit_cfs

set.seed(12423)
ldaFit_cfs <- train(cfs_Formula_woe, method='lda', 
                data=training_woe, 
                trControl = fitControl)
ldaFit_cfs

set.seed(12423)
qdaFit_cfs<- train(cfs_Formula_woe, method='qda', 
               data=training_woe, 
               trControl = fitControl)
qdaFit_cfs



set.seed(12423)
glmAIC_Fit_consistency <- train(consistency_Formula, data = training, 
                    method = "glmStepAIC", 
                    family    = binomial,
                    trControl = fitControl,
                    preProc = c("center", "scale"),
                    trace = 0)

glmAIC_Fit_consistency 

set.seed(12423)
glmFit_consistency <- train(consistency_Formula, data       = training,
                method    = "glm",
                family    = binomial,
                trControl = fitControl,
                metric = "ROC")

glmFit_consistency

### Modeling using WOE data

set.seed(12423)
knnFit_consistency <- train(consistency_Formula_woe, data = training_woe, 
                method = "knn", 
                trControl = fitControl, 
                tuneLength = 8)

knnFit_consistency

set.seed(12423)
ldaFit_consistency <- train(consistency_Formula_woe, method='lda', 
                data=training_woe, 
                trControl = fitControl)
ldaFit_consistency

set.seed(12423)
qdaFit_consistency<- train(consistency_Formula_woe, method='qda', 
               data=training_woe, 
               trControl = fitControl)
qdaFit_consistency


set.seed(12423)
glmFit_best <- train(best_search_formula, data  = training,
                            method    = "glm",
                            family    = binomial,
                            trControl = fitControl,
                            metric = "ROC")

set.seed(12423)
knnFit_best <- train(best_search_formula_woe, data = training_woe, 
                            method = "knn", 
                            trControl = fitControl, 
                            tuneLength = 8)

knnFit_best

set.seed(12423)
ldaFit_best <- train(best_search_formula_woe, method='lda', 
                            data=training_woe, 
                            trControl = fitControl)
ldaFit_best

set.seed(12423)
qdaFit_best<- train(best_search_formula_woe, method='qda', 
                           data=training_woe, 
                           trControl = fitControl)
qdaFit_best

c = 20/log(2)
d= 500
karta_punkty = coef(model_4)*c
karta_punkty["(Intercept)"] = karta_punkty["(Intercept)"] + d
format(karta_punkty,scientific=FALSE)  
tabelka <- glmFit_best$finalModel$coefficients
score_card <- data.frame(Variable = names(tabelka),
                          Value = round(as.numeric(tabelka[1:31])*20/log(2) 
                                        + c(500, rep(0,30)),2)) 


xtable(score_card, caption="Scorecard table")

