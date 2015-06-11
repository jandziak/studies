#' After modeling part it is impoortant to show some of the diagnostics for the 
#' model
#' We predict probabilities for the classes and then we can compare them using 
#' distribution,
#'  histogram and ROC curve
#' In order to perform analysis form of the scores should be as follows
#' It suppose to be data frame with two columns - scores with values of 
#' probability and flag (GOOD/BAD) for each case
#' Function to plot a histogram for the data
#' @param probablities - data frame with scores and flag GOOD/BAD for 
#' observations 
#' @param bins - number of bins for the histogram
#' @return p - histogram 
plot_hist <- function(probabilities, bins = 30){
  binwidth <- (max(probabilities$score)-min(probabilities$score))/bins
  p <- ggplot(probabilities, aes(score, fill = flag)) 
  p <- p + geom_histogram(alpha = 0.7                       
                   , aes(y = ..density..)                              
                   , position = 'identity'
                   , binwidth = binwidth)
  return(p)
}
#' Function to calculate GINI and AUROC indexes 
#' @param fraction.GOOD fraction of observations with GOOD flag for 
#' certain interval 
#' @param fraction.BAD fraction of observations with BAD flag for 
#' certain interval 
#' @return list of calculated coefficients: GINI, AUROC
calculate_ROC_GINI <- function(fraction.GOOD, fraction.BAD){
  k <- length(fraction.GOOD)
  df <- data.frame(LOWER_T_NUM = fraction.GOOD[1:(k-1)]
                   , UPPER_F_NUM = fraction.BAD[2:k]
                   , UPPER_T_NUM = fraction.GOOD[2:k]
                   , LOWER_F_NUM = fraction.BAD[1:(k-1)])
  
  # Area under ROC curve and GINI coefficient
  T_DIFFERENCE <- df$UPPER_T_NUM - df$LOWER_T_NUM
  F_DIFFERENCE <- df$UPPER_F_NUM - df$LOWER_F_NUM
  Area <- (T_DIFFERENCE * F_DIFFERENCE)/2 + df$UPPER_F_NUM * T_DIFFERENCE
  AUROC <- sum(Area) 
  GINI <- 2 * AUROC - 1
  return(list(GINI = GINI, AUROC = AUROC))
}

#' Funtion to calculate GINI and AUROC index 
#' @param probablities - data frame with scores and flag GOOD/BAD for observations 
#' @param thershold - value of length of the interval
#' @return list of fractions of GOOD and BAD observations calculated within intervals
calculate_fractions <- function(probabilities, threshold){
  
  # Calculation number of observations for flags 
  n <- sum(probabilities$flag == "GOOD")
  m <- sum(probabilities$flag == "BAD")
  # Construction of intervals
  division <- seq(min(probabilities$score), max(probabilities$score), by = threshold)
  k <- length(division)
  fraction.GOOD <- fraction.BAD <- rep(0, k)
  
  # Calculation of appropriate values
  fraction.GOOD <- sapply(division
                          , function(x) sum(ifelse(probabilities$score <= x & probabilities$flag=="GOOD", 1, 0)))/n
  fraction.BAD <- sapply(division
                         , function(x) sum(ifelse(probabilities$score <= x & probabilities$flag=="BAD", 1, 0)))/m
  
  return(list(fraction.GOOD = c(0, fraction.GOOD),fraction.BAD = c(0, fraction.BAD), division = c(0, division)))
}

#' Funtion to plot ROC curve with the GINI and AUROC index 
#' @param fraction.GOOD - vector of cumulative fraction of good observations for intervals
#' @param fraction.BAD - vector of cumulative fraction of bad observations for intervals
#' @param index - calculated values of GINI and AUROC indexes
#' @return p - plot of ROC curve
plot_ROC <- function(fraction.GOOD, fraction.BAD, index){
  df <- data.frame(fraction.GOOD, fraction.BAD)
  p <- ggplot(data = df, aes(x = fraction.GOOD, y = fraction.BAD, color = "Red")) +  
    geom_line(alpha = 1, size = 1.2) +
    labs(title = "ROC curve") +
    ylab("True Positive Rate (Sensitivity)") +
    xlab("False Positive Rate (1-Specificity)") + 
    theme(plot.title = element_text(lineheight =.8, face ="bold")) + 
    geom_text(data = NULL, x = 0.75, y = 0.05, label = paste("AUROC =",round(index$AUROC,4)), color = "Black") +
    geom_text(data = NULL, x = 0.75, y = 0.15, label = paste("GINI =",round(index$GINI,4)), color = "Black") + ylim(0,1) + xlim(0,1)
  return(p)
}

#' Funtion to plot ROC curve with the GINI and AUROC index 
#' @param fraction.GOOD - vector of cumulative fraction of good observations for intervals
#' @param fraction.BAD - vector of cumulative fraction of bad observations for intervals
#' @param division - values for interval bands
#' @return p - plot of ROC curve
plot_distribution <- function(fraction.GOOD, fraction.BAD, division){
  df <- data.frame(scores = c(fraction.GOOD, fraction.BAD)
                   , division = c(division, division)
                   , etykieta = as.factor(c(rep("F(s|GOOD)", length(division)), rep("F(s|BAD)", length(division)))))
  KS <- max(abs(fraction.GOOD - fraction.BAD))
  p <- ggplot(data = df, aes(x = division, y = scores, color = etykieta)) +
    geom_line(alpha = 1,size = 1.2) +
    labs(title = "Wykres dystrybuant") +
    ylab("Prawdopodobienstwo") +
    xlab("x") + 
    theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
    geom_text(data = NULL, x = 0.4, y = 0.05, label = paste("KS =", round(KS,4)), color = "Black")
  return(p)
}

#' Tests for appropriate pr value
#' frac <- calculate_fractions(probabilities = pr, threshold = 0.01)
#' ind <- calculate_ROC_GINI(fraction.GOOD = frac$fraction.GOOD, fraction.BAD = frac$fraction.BAD)
#' plot_hist(pr,20)
#' plot_ROC(fraction.GOOD = frac$fraction.GOOD, fraction.BAD = frac$fraction.BAD, index = ind)
#' plot_distribution(fraction.GOOD = frac$fraction.GOOD, fraction.BAD = frac$fraction.BAD, division = frac$division)