
pr_rf <- predict(rfFit, newdata = testing, type = "prob")
pr_rf<-data.frame(score=pr_rf$GOOD
                  ,flag=ifelse(testing$RES == "GOOD","GOOD","BAD"))

frac <- calculate_fractions(probabilities = pr_rf, threshold = 0.01)
ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD
                          , fraction.BAD = frac$fraction.BAD)

plot_hist(pr_rf,20)
plot_ROC(fraction.GOOD = frac$fraction.GOOD
         ,fraction.BAD = frac$fraction.BAD, index = ind)
plot_distribution(fraction.GOOD = frac$fraction.GOOD
                  , fraction.BAD = frac$fraction.BAD, division = frac$division)



pr_gbm <- predict(gbmFit3, newdata = testing, type = "prob")
pr_gbm<-data.frame(score=pr_gbm$GOOD
                  ,flag=ifelse(testing$RES == "GOOD","GOOD","BAD"))

frac <- calculate_fractions(probabilities = pr_gbm, threshold = 0.01)
ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD
                          , fraction.BAD = frac$fraction.BAD)

plot_hist(pr_gbm,20)
plot_ROC(fraction.GOOD = frac$fraction.GOOD
         ,fraction.BAD = frac$fraction.BAD, index = ind)
plot_distribution(fraction.GOOD = frac$fraction.GOOD
                  , fraction.BAD = frac$fraction.BAD, division = frac$division)


pr_glmAIC_ <- predict(glmFit_best, newdata = testing, type = "prob")
pr_glmAIC_<-data.frame(score=pr_glmAIC_$GOOD
                  ,flag=ifelse(testing$RES == "GOOD","GOOD","BAD"))

frac <- calculate_fractions(probabilities = pr_glmAIC_, threshold = 0.01)
ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD
                          , fraction.BAD = frac$fraction.BAD)

plot_hist(pr_glmAIC_,20)
plot_ROC(fraction.GOOD = frac$fraction.GOOD
         ,fraction.BAD = frac$fraction.BAD, index = ind)
plot_distribution(fraction.GOOD = frac$fraction.GOOD
                  , fraction.BAD = frac$fraction.BAD, division = frac$division)



pr_lda <- predict(ldaFit_best, newdata = testing_woe, type = "prob")
pr_lda<-data.frame(score=pr_lda$GOOD
                  ,flag=ifelse(testing_woe$RES == "GOOD","GOOD","BAD"))

frac <- calculate_fractions(probabilities = pr_lda, threshold = 0.01)
ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD
                          , fraction.BAD = frac$fraction.BAD)

plot_hist(pr_lda,20)
plot_ROC(fraction.GOOD = frac$fraction.GOOD
         ,fraction.BAD = frac$fraction.BAD, index = ind)
plot_distribution(fraction.GOOD = frac$fraction.GOOD
                  , fraction.BAD = frac$fraction.BAD, division = frac$division)


pr_knn <- predict(knnFit, newdata = testing_woe, type = "prob")
pr_knn<-data.frame(score=pr_knn$GOOD
                  ,flag=ifelse(testing_woe$RES == "GOOD","GOOD","BAD"))

frac <- calculate_fractions(probabilities = pr_knn, threshold = 0.01)
ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD
                          , fraction.BAD = frac$fraction.BAD)

plot_hist(pr_knn,20)
plot_ROC(fraction.GOOD = frac$fraction.GOOD
         ,fraction.BAD = frac$fraction.BAD, index = ind)
plot_distribution(fraction.GOOD = frac$fraction.GOOD
                  , fraction.BAD = frac$fraction.BAD, division = frac$division)


