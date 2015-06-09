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

gcredit.cat <- 
  read.table("./data/german_data_cat.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 1), rep("character", 21)))


gcredit.woe <- 
  read.table("./data/german_data_woe.txt", sep=",", header =TRUE,  
             colClasses=c(rep("numeric", 22)))








# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#' FEATURE SELECTION METHODS
#' 
#' FSelector package



#' CFS filter 
#' The algorithm finds attribute subset using correlation and entropy measures 
#' for continous and discrete data.
cfs(gcredit.cat.f.full, gcredit.cat)


#' Chi-squared filter
#' The algorithm finds weights of discrete attributes basing on a chi-squared test.
chi.squared.res <- chi.squared(gcredit.cat.f.full, gcredit.cat)
chi.squared.res$attr_name <- rownames(chi.squared.res)
arrange(chi.squared.res, desc(attr_importance))


#' Consistency-based filter
#' The algorithm finds attribute subset using consistency measure for continous 
#' and discrete data.
consistency.res <- consistency(gcredit.cat.f.full, gcredit.cat)


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





library(rpart)
data(iris)

evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(iris))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- iris[test.idx, , drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "Species"), train)
    error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- exhaustive.search(names(iris)[-5], evaluator)
f <- as.simple.formula(subset, "Species")
print(f)







