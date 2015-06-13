# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# READ DATA 

gcredit.cat <- 
  read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 1), rep("character", 21)))

gcredit.cat$RES <- as.factor(gcredit.cat$RES)

gcredit.woe <- 
  read.table("./data/german_data_woe.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 22)))
gcredit.cat$RES <- as.factor(gcredit.cat$RES)

gcredit.cat.f.full <- as.formula("RES ~ .")


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#' FEATURE SELECTION METHODS
#' 
#' FSelector package

cfs_Formula <- as.simple.formula(cfs_VARS, "RES")
consistency_Formula <- as.simple.formula(consistency.VARS,"RES")
cfs_Formula_woe <- as.simple.formula(cfs_VARS_woe, "RES")
consistency_Formula_woe <- as.simple.formula(consistency.VARS_woe, "RES")

#' CFS filter 
#' The algorithm finds attribute subset using correlation and entropy measures 
#' for continous and discrete data.
cfs_VARS <- cfs(gcredit.cat.f.full, gcredit.cat)
cfs_VARS_woe <- cfs(gcredit.cat.f.full, gcredit.woe)

#' Chi-squared filter
#' The algorithm finds weights of discrete attributes basing on a chi-squared test.
chi.squared.res <- chi.squared(gcredit.cat.f.full, gcredit.cat)
chi.squared.res$attr_name <- rownames(chi.squared.res)
arrange(chi.squared.res, desc(attr_importance))


#' Consistency-based filter
#' The algorithm finds attribute subset using consistency measure for continous 
#' and discrete data.
consistency.VARS <- consistency(gcredit.cat.f.full, gcredit.cat)
consistency.VARS_woe <- consistency(gcredit.cat.f.full, gcredit.woe)


#' ENTROPY-BASED-FILTERS
#' The algorithms find weights of discrete attributes basing on their correlation 
#' with continous class attribute.
information.gain.res <- information.gain(gcredit.cat.f.full, gcredit.cat)
information.gain.res$attr_name <- rownames(information.gain.res)
arrange(information.gain.res, desc(attr_importance))

gain.ratio.res <- gain.ratio(gcredit.cat.f.full, gcredit.cat)
gain.ratio.res$attr_name <- rownames(gain.ratio.res)
arrange(gain.ratio.res, desc(attr_importance))

symmetrical.uncertainty.res <- symmetrical.uncertainty(gcredit.cat.f.full, gcredit.cat)
symmetrical.uncertainty.res$attr_name <- rownames(symmetrical.uncertainty.res)
arrange(symmetrical.uncertainty.res, desc(attr_importance))



#' CORRELATION FILTER
#' 
#' The algorithm finds weights of continous attributes basing on their correlation 
#' with continous class attribute.
linear.correlation.res <- linear.correlation(RES ~ ., gcredit.woe)
linear.correlation.res$attr_name <- rownames(linear.correlation.res)
arrange(linear.correlation.res, desc(attr_importance))

rank.correlation.res <- rank.correlation(RES ~ ., gcredit.woe)
rank.correlation.res$attr_name <- rownames(rank.correlation.res)
arrange(rank.correlation.res, desc(attr_importance))



#' OneR algorithm
#' The algorithms find weights of discrete attributes basing on 
#' very simple association rules involving only one attribute in condition part.
oneR.res <- oneR(RES ~ ., gcredit.woe)
oneR.res$attr_name <- rownames(oneR.res)
arrange(oneR.res, desc(attr_importance))



#' RandomForest filter
#' 
#' The algorithm finds weights of attributes using RandomForest algorithm.
#' 
#' importance.type -  either 1 or 2, specifying the type of importance measure 
#'                    (1=mean decrease in accuracy, 2=mean decrease in node impurity)

# random.forest.importance1.res <- 
#   random.forest.importance(gcredit.cat.f.full, gcredit.cat, importance.type = 1)
# random.forest.importance1.res$attr_name <- rownames(random.forest.importance1.res)
# arrange(random.forest.importance1.res, desc(attr_importance))

random.forest.importance2.res <- 
  random.forest.importance(gcredit.cat.f.full, gcredit.cat, importance.type = 2)
random.forest.importance2.res$attr_name <- rownames(random.forest.importance1.res)
arrange(random.forest.importance2.res, desc(attr_importance))



#' RReliefF filter
#' 
#' The algorithm finds weights of continous and discrete attributes basing on a 
#' distance between instances.
relief.neighb5.res <- relief(gcredit.cat.f.full, gcredit.cat, neighbours.count = 5, sample.size = 10)
relief.neighb5.res$attr_name <- rownames(relief.neighb5.res)
arrange(relief.neighb5.res, desc(attr_importance))



gcredit.woe$RES <- as.factor(gcredit.woe$RES)


#' CFS filter 
#' The algorithm finds attribute subset using correlation and entropy measures 
#' for continous and discrete data.
cfs_VARS <- cfs(gcredit.cat.f.full, gcredit.cat)
cfs_VARS_woe <- cfs(gcredit.cat.f.full, gcredit.woe)

#' Chi-squared filter
#' The algorithm finds weights of discrete attributes basing on a chi-squared test.
chi.squared.res <- chi.squared(gcredit.cat.f.full, gcredit.cat)
chi.squared.res$attr_name <- rownames(chi.squared.res)
arrange(chi.squared.res, desc(attr_importance))


#' Consistency-based filter
#' The algorithm finds attribute subset using consistency measure for continous 
#' and discrete data.
consistency.VARS <- consistency(gcredit.cat.f.full, gcredit.cat)
consistency.VARS_woe <- consistency(gcredit.cat.f.full, gcredit.woe)




chi.squared.res_imoportance <- arrange(chi.squared.res, desc(attr_importance))
information.gain.res_imoportance <- arrange(information.gain.res, desc(attr_importance))
gain.ratio.res_imoportance <- arrange(gain.ratio.res, desc(attr_importance))
symmetrical.uncertainty.res_imoportance <- arrange(symmetrical.uncertainty.res, desc(attr_importance))
random.forest.importance2.res_imoportance <- arrange(random.forest.importance2.res, desc(attr_importance))
relief.neighb5.res_imoportance <- arrange(relief.neighb5.res, desc(attr_importance))
linear.correlation.res_imoportance <- arrange(linear.correlation.res, desc(attr_importance))
rank.correlation.res_imoportance <- arrange(rank.correlation.res, desc(attr_importance))
oneR.res_imoportance <- arrange(oneR.res, desc(attr_importance))

results_feature_importance <- data.frame(chi.squared.res_imoportance,
                                         information.gain.res_imoportance,
                                         gain.ratio.res_imoportance,
                                         symmetrical.uncertainty.res_imoportance,
                                         random.forest.importance2.res_imoportance, 
                                         relief.neighb5.res_imoportance,
                                         linear.correlation.res_imoportance,
                                         rank.correlation.res_imoportance,
                                         oneR.res_imoportance)

chi.squared.res_name <- arrange(chi.squared.res, desc(attr_name))
information.gain.res_name <- arrange(information.gain.res, desc(attr_name))
gain.ratio.res_name <- arrange(gain.ratio.res, desc(attr_name))
symmetrical.uncertainty.res_name <- arrange(symmetrical.uncertainty.res, desc(attr_name))
random.forest.importance2.res_name <- arrange(random.forest.importance2.res, desc(attr_name))
relief.neighb5.res_name <- arrange(relief.neighb5.res, desc(attr_name))
linear.correlation.res_name <- arrange(linear.correlation.res, desc(attr_name))
rank.correlation.res_name <- arrange(rank.correlation.res, desc(attr_name))
oneR.res_name <- arrange(oneR.res, desc(attr_name))


results_feature_name <- data.frame(chi.squared.res_name,
                                   information.gain.res_name,
                                   gain.ratio.res_name,
                                   symmetrical.uncertainty.res_name,
                                   random.forest.importance2.res_name, 
                                   relief.neighb5.res_name,
                                   linear.correlation.res_name,
                                   rank.correlation.res_name,
                                   oneR.res_name)




standarization <- function(importance)
{
  importance$attr_importance <- 
    (importance$attr_importance - (min(na.omit(importance$attr_importance))))/
    var(na.omit( (importance$attr_importance - (min(na.omit(importance$attr_importance))))))  
  return(importance)
}

chi.squared.res_name <- standarization(chi.squared.res_name)
information.gain.res_name <- standarization(information.gain.res_name)
gain.ratio.res_name <- standarization(gain.ratio.res_name)
symmetrical.uncertainty.res_name <- standarization(symmetrical.uncertainty.res_name)
random.forest.importance2.res_name <- standarization(random.forest.importance2.res_name) 
relief.neighb5.res_name <- standarization(relief.neighb5.res_name)
linear.correlation.res_name <- standarization(linear.correlation.res_name)
rank.correlation.res_name <- standarization(rank.correlation.res_name)
oneR.res_name <- standarization(oneR.res_name)




values <- rbind(chi.squared.res_name,
                information.gain.res_name,
                gain.ratio.res_name,
                symmetrical.uncertainty.res_name,
                random.forest.importance2.res_name, 
                relief.neighb5.res_name,
                linear.correlation.res_name,
                rank.correlation.res_name,
                oneR.res_name)

label <- c(
  rep("Chi_Squared",21),
  rep("Information Gain",21,
      rep("Gain Ratio",21),
      rep("Uncertainty",21),
      rep("RF importance",21),
      rep("Relief Neighb",21),
      rep("Linear",21),
      rep("Rank",21),
      rep("OneR",21))
  
  
  variables_importance_measures <- cbind(values, label)
  
  
  variables_importance_measures_cont <- variables_importance_measures[1:126, ]
  variables_importance_measures_fact <- variables_importance_measures[127:189, ]
  
  
  tmp_plot2 <- qplot(data = variables_importance_measures_cont, x = as.factor(attr_name)
                     , y = attr_importance
                     , fill = as.factor(label), geom = "bar"
                     , stat="identity"
                     , position="dodge"
                     , main = "Standarised Importance of different variables for different methods"
                     , xlab = "")
  
  tmp_plot22 <- tmp_plot2 + scale_y_continuous(name="Importance") +  
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_fill_manual(values=c("#8c510a"
                               , "#bf812d"
                               , "#dfc27d"
                               , "#80cdc1"
                               , "#35978f"
                               , "#01665e"),guide = guide_legend(title = "Method"))
  
  
  tmp_plot1 <- qplot(data = variables_importance_measures_fact, x = as.factor(attr_name)
                     , y = attr_importance
                     , fill = as.factor(label), geom = "bar"
                     , stat="identity"
                     , position="dodge"
                     , main = "Standarised Importance of different variables for different methods"
                     , xlab = "")
  
  tmp_plot11 <- tmp_plot1 + scale_y_continuous(name="Importance") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_fill_manual(values=c(
      "#bf812d"
      , "#80cdc1"
      , "#35978f"
      , "#01665e"),guide = guide_legend(title = "Method"))
  
