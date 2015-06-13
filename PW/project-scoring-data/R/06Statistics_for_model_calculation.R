library(xtable)
stats <- function(test, model){
  conf_mat <- table(test$RES, predict(model, test, type="raw"))
  TP <- conf_mat[1,1]
  FP <- conf_mat[1,2]
  TN <- conf_mat[2,2]
  FN <- conf_mat[2,1]
  error.rate = (FP+FN)/(TP+TN+FP+FN)
  ACC = 1 - error.rate
  TPR = TP/(TP+FN)
  SPC = TN/(TN+FP)
  PPV = TP/(TP+FP)
  F1  = 2*TPR*PPV/(TPR+PPV)
  FDR = 1 - PPV
  results_tree <- data.frame(ACC = ACC,
                             TPR = TPR,
                             SPC = SPC,
                             PPV = PPV,
                             F1  = F1,
                             FDR = FDR)
  return(results_tree)
}

stats_full_models <- rbind(round(stats(testing, gbmFit3),3), 
                           round(stats(testing,rfFit),3), 
                           round(stats(testing,glmAIC_Fit),3),
                           round(stats(testing,glmFit),3), 
                           round(stats(testing_woe,knnFit),3),
                           round(stats(testing_woe,ldaFit),3),
                           round(stats(testing_woe,qdaFit),3))


labels_fm <- c("Boosting"
               , "Random Forest"
               , "AIC Logistic regression"
               , "Logistic regression"
               , "K-NN"
               , "Linear Dyscryminant Analysis"
               , "Quadric Dyscriminant Analysis")

stats_full_models <- cbind(Label = labels_fm, stats_full_models)


stats_consistency_models<- rbind(round(stats(testing, glmAIC_Fit_consistency),3),
                         round(stats(testing, glmFit_consistency),3),
                         round(stats(testing_woe, knnFit_consistency),3),
                         round(stats(testing_woe, ldaFit_consistency),3),
                         round(stats(testing_woe, qdaFit_consistency),3))

labels_consistency<- c("AIC Logistic Regression"
               , "Logistic Regression"
               , "K-NN"
               , "Linear Dyscriminant Analysis"
               , "Quadric Dyscriminant Analysis")

stats_consistency_models <- cbind(Label = labels_consistency,stats_consistency_models)



stats_cfs_models<- rbind(round(stats(testing, glmAIC_Fit_cfs),3),
                         round(stats(testing, glmFit_cfs),3),
                         round(stats(testing_woe, knnFit_cfs),3),
                         round(stats(testing_woe, ldaFit_cfs),3),
                         round(stats(testing_woe, qdaFit_cfs),3))

labels_cfs<- c("AIC Logistic Regression"
               , "Logistic Regression"
               , "K-NN"
               , "Linear Dyscriminant Analysis"
               , "Quadric Dyscriminant Analysis")

stats_cfs_models <- cbind(Label = labels_cfs,stats_cfs_models)




stats_best_models<- rbind(round(stats(testing, glmFit_best),3),
                         round(stats(testing_woe, knnFit_best),3),
                         round(stats(testing_woe, ldaFit_best),3),
                         round(stats(testing_woe, qdaFit_best),3))

labels_best<- c("Logistic Regression"
               , "K-NN"
               , "Linear Dyscriminant Analysis"
               , "Quadric Dyscriminant Analysis")

stats_best_models <- cbind(Label = labels_best,stats_best_models)



xtable(stats_cfs_models, caption="Basic statistics for models with features from CFS method")
xtable(stats_consistency_models, caption="Basic statistics for models with features from consistency method")
xtable(stats_best_models, caption="Basic statistics for models with features from filtered best method")
xtable(stats_full_models, caption="Basic statistics for models with all features")
