confusion_matrix <- function(df, actual, predicted){
  table(df[[actual]], df[[predicted]])
}

confusion_matrix_counts <- function(cm){
  
}

# Write a function that takes the data set as a dataframe, with actual and 
# predicted classifications identified, and returns the accuracy of the 
# predictions.

accuracy <- function(df, actual, predicted){
  
}

# Write a function that takes the data set as a dataframe, with actual and 
# predicted classifications identified, and returns the classification error 
# rate of the predictions.
# Verify that you get an accuracy and an error rate that sums to one.

classification_error_rate <- function(df, actual, predicted){
  
}

# Write a function that takes the data set as a dataframe, with actual and 
# predicted classifications identified, and returns the precision of the 
# predictions.

precision <- function(df, actual, predicted){
  
}

# Write a function that takes the data set as a dataframe, with actual and 
# predicted classifications identified, and returns the sensitivity of the 
# predictions. Sensitivity is also known as recall.

sensitivity <- function(df, actual, predicted){
  
}

recall <- function(df, actual, predicted){
  sensitivity(df, actual, predicted)
}

# Write a function that takes the data set as a dataframe, with actual and
# predicted classifications identified, and returns the specificity of the
# predictions.

specificity <- function(df, actual, predicted){
  
}

# Write a function that takes the data set as a dataframe, with actual and
# predicted classifications identified, and returns the F1 score of the
# predictions.

f1_score <- function(df, actual, predicted){
  
}

# Write a function that generates an ROC curve from a data set with a true
# classification column (class in our example) and a probability column
# (scored.probability in our example). Your function should return a list
# that includes the plot of the ROC curve and a vector that contains the
# calculated area under the curve (AUC). Note that I recommend using a
# sequence of thresholds ranging from 0 to 1 at 0.01 intervals.

roc_curve <- function(df, true_classification, probabilty){
  
}