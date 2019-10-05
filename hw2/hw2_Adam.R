##################################################
# Functions to compute classification measures.
##################################################
# I know you have these already, but I thought
# it may be good practice to do my own.
#

accuracy <- function(df, pred, act){
  sum(pred == act)/nrow(df)
}

errors <- function(df, pred,act){
  sum(pred != act)/nrow(df)
}

precision <- function(df, pred, act){
  sum(pred == 1 & act == 1) /
    (sum(pred == 1 & act == 1) + sum(pred == 1 & act == 0))
}

sensitivity <- function(df, pred, act){
  sum(pred == 1 & act == 1) /
    (sum(act == 1))
}

specificity <- function(df, pred, act){
  sum(pred == 0 & act == 0) /
    (sum(pred == 0 & act == 0) + sum(pred == 1 & act == 0))
}

false_positive <- function(df, pred, act){
  sum(pred == 1 & act == 0) /
    (sum(act == 0))
}

f1 <- function(df, pred, act){
  p <- precision(df, pred, act)
  s <- sensitivity(df, pred, act)
  2 * (p * s)/
    (p + s)
}

########################
# ROC Curve generation
########################
# I think this will also work with your functions, Mike.
#
ROC <- function(df, prob, act){
  require(dplyr)
  outcome <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(outcome) <- c("prob","TPR","FPR")
  
  for (thresh in seq(0,1,0.01)){
    
    outcome <- rbind(outcome,
                     data.frame(
                       prob=thresh,
                       TPR = sensitivity(df,ifelse(prob >= thresh,1,0),act),
                       FPR = false_positive(df,ifelse(prob >= thresh,1,0),act),
                       area = 0
                       )
    )
  }
  
  # Get AUC
  outcome$area <- (dplyr::lag(outcome$FPR) - outcome$FPR) * outcome$TPR

    plot.new()
    plot(x=outcome$FPR, y=outcome$TPR, type="l",main="ROC Curve",
         xlab="False Positive Rate (FPR)",
         ylab="True Positive Rate (TPR)")
    abline(a=0,b=1,lty=2)
    text(0.6,0.1,paste("AUC = ",round(sum(outcome$area, na.rm = T),3)))
}

ROC(df,df$scored.probability,df$class)