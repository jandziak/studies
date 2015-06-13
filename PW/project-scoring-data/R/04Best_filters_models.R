method_ACC_plot_data <- function(importance, data){
  #' Control of training for models 
  results <- 0
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 10,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using 
                             ## the following function
                             summaryFunction = twoClassSummary)
  for (i in 1:21){
    set.seed(12423)
    formula <- as.simple.formula(importance[1:i,2], "RES")
    print(formula)
    glmFit <- train(formula, data       = data,
                    method    = "glm",
                    family    = binomial,
                    trControl = fitControl,
                    metric = "ROC")
    results <- cbind(results, glmFit$results[2])
  }
  
  return(results[-1])
}


random.forest.importance2.res_imoportance <- arrange(random.forest.importance2.res_name,desc(attr_importance))
chisq_ROC <- method_ACC_plot_data(chi.squared.res_imoportance, data)
inf_gain_ROC <- method_ACC_plot_data(information.gain.res_imoportance, data)
gain_ROC <- method_ACC_plot_data(gain.ratio.res_imoportance, data)
uncer_ROC <- method_ACC_plot_data(symmetrical.uncertainty.res_imoportance, data)
rf_ROC <- method_ACC_plot_data(random.forest.importance2.res_imoportance, data)
nei_ROC <- method_ACC_plot_data(relief.neighb5.res_imoportance, data)
lin_corr_ROC <- method_ACC_plot_data(linear.correlation.res_imoportance, data_woe)
rank_corr_ROC <- method_ACC_plot_data(rank.correlation.res_imoportance, data_woe)
oneR_ROC <- method_ACC_plot_data(oneR.res_imoportance, data_woe)


ROC_Value <- c(t(chisq_ROC) ,
               t(inf_gain_ROC),
               t(gain_ROC),
               t(uncer_ROC),
               t(rf_ROC),
               t(nei_ROC),
               t(lin_corr_ROC),
               t(rank_corr_ROC),
               t(oneR_ROC))

label <- c(
  rep("Chi_Squared",21),
  rep("Information Gain",21),
      rep("Gain Ratio",21),
      rep("Uncertainty",21),
      rep("RF importance",21),
      rep("Relief Neighb",21),
      rep("Linear",21),
      rep("Rank",21),
      rep("OneR",21))

ROC_data <- data.frame(ROC_Value = ROC_Value, Label = as.factor(label), Number = rep(1:21,9))
qplot(data = ROC_data, x = Number, y = ROC_Value, colour = Label
      , geom = "line", main = "ROC value for different number of best predictors for Logistic Regression")


as.simple.formula(information.gain.res_imoportance[1:8,2], "RES")
