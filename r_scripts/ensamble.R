#-------------------------------------------------------------------------------
library(forcats)
# DATA PRE-PROCESSING
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
bank <- bank[, -c(1, 22, 23)]
#bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)
# Convert Attrition_Flag to a binary factor
bank$Attrition_Flag <- factor(bank$Attrition_Flag == "Attrited Customer", levels = c(FALSE, TRUE))

# If "Attrited Customer" is TRUE, it will be coded as 1, and other values will be coded as 0


# Convert all categorical variables to factors and reorder the levels
bank$Gender <- as.factor(bank$Gender)
bank$Income_Category <- fct_collapse(bank$Income_Category,
                                     "Less than 120K" = c("Unknown",
                                                          "Less than $40K",
                                                          "$40K - $60K",
                                                          "$60K - $80K",
                                                          "$80K - $120K"),
                                     "More than 120K" = c("$120K +"))

# Chnanging the levels of Marital_Status in either married or not married
bank$Marital_Status <- fct_collapse(bank$Marital_Status,
                                    "Married" = c("Married"),
                                    "Not Married" = c("Divorced",
                                                      "Single",
                                                      "Unknown"))
# Converting Months_Inactive_12_mon to a factor
bank$Months_Inactive_12_mon <- as.factor(bank$Months_Inactive_12_mon)
levels(bank$Months_Inactive_12_mon) <- c("0", "1", "2", "3", "4", "5", "6+")

# Joining toghether the levels after 4 months
bank$Months_Inactive_12_mon <- fct_collapse(bank$Months_Inactive_12_mon,
                                            "4+" = c("4", "5", "6+"))

bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)
# Override the Total_Trans_Amt variable with its log !!!
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)
# Standardization (optional) all columns except response and categorical
#bank[, -c(1, 2, 3, 4)] <- scale(bank[, -c(1, 2, 3, 4)]) 

#-------------------------------------------------------------------------------
# Basic assessment functions
# LIBRARIES --------------------------------------------------------------------
library(car)
library(caret)
library(pROC)

# FUNCTIONS --------------------------------------------------------------------

# Learning function: f'_learn
learn.boost <- function(data) {
  # Logistic regression
  model <- boosting(Attrition_Flag ~ .,
               data = data,
               boos=TRUE)
  return(model)
}

# Prediction function: f'_predict
predict_prime.boost <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response")$prob[,2] > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second.boost <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")$prob[,2]
  return(predicted_probs)
}

# Function to assess the model later
assess.boost <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response")$prob[,2] > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == FALSE) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second.boost(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
  # Extract the variable selection information for each boosting iteration
  var_selection <- model$importance
  
  # Sort the variable importance in descending order
  sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       row.names = NULL)
  
  # AIC
  #aic <- AIC(model)
  
  # BIC
  #bic <- BIC(model)
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
    variable_importance = variable_importance_df
    #aic = aic,
    #bic = bic
  )
  
  # Print results
  cat("----------------------------------------\n")
  print(results$confusion_matrix)
  cat("----------------------------------------\n")
  cat("Accuracy:", round(results$accuracy * 100, 2), "%\n")
  cat("Dummy classifier accuracy:",
      round(results$dummy_classifier_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AUC:", round(results$auc * 100, 2), "%\n")
  cat("Dummy classifier AUC:",
      round(results$dummy_classifier_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("FPR:", round(results$fpr * 100, 2), "%\n")
  cat("FNR:", round(results$fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Variable importance:\n")
  print(results$variable_importance)
  cat("----------------------------------------\n")
  #cat("AIC:", results$aic, "\n")
  #cat("BIC:", results$bic, "\n")
  #cat("----------------------------------------\n")
  
  return(results)
}

# k-fold cross validation function: f'_cv
cv.boost <- function(data, k = 10) {
  # Create k equally sized folds
  folds <- createFolds(data$Attrition_Flag, k = k)
  
  # Initialize lists to store evaluation metrics and variable importance
  accuracy <- vector("numeric", k)
  auc <- vector("numeric", k)
  fpr <- vector("numeric", k)
  fnr <- vector("numeric", k)
  mean_decrease_list <- vector("list", k)
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    model <- learn.boost(train)
    
    # Predict on the testing set
    predicted <- predict_prime.boost(model, test)
    predicted_probs <- predict_second.boost(model, test)
    
    # Compute evaluation metrics
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, predicted_probs)
    auc[i] <- auc(roc)
    fp <- sum(predicted == TRUE & test$Attrition_Flag == FALSE)
    fn <- sum(predicted == FALSE & test$Attrition_Flag == TRUE)
    fpr[i] <- fp / sum(test$Attrition_Flag == FALSE)
    fnr[i] <- fn / sum(test$Attrition_Flag == TRUE)
    
    # Extract the variable selection information for each boosting iteration
    var_selection <- model$importance
    
    # Sort the variable importance in descending order
    sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
    
    # Store the sorted variable importance in the list
    mean_decrease_list[[i]] <- sorted_variable_importance
  }
  
  # Compute average evaluation metrics
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  # Compute average variable importance
  # Convert the list to a matrix to compute the average mean decrease
  mean_decrease_matrix <- do.call(rbind, mean_decrease_list)
  
  # Compute the average mean decrease across all iterations
  average_mean_decrease <- colMeans(mean_decrease_matrix)
  sd_mean_decrease <- apply(mean_decrease_matrix, 2, sd)
  
  
  sorted_variable_importance <- sort(average_mean_decrease, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       Std_Dev = sd_mean_decrease,
                                       row.names = NULL)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  
  # Print average evaluation metrics and variable importance
  cat("----------------------------------------\n")
  cat("Average accuracy:", round(average_accuracy * 100, 2), "+/-",
      round(sd_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average AUC:", round(average_auc * 100, 2), "+/-",
      round(sd_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average FPR:", round(average_fpr * 100, 2), "+/-",
      round(sd_fpr * 100, 2), "%\n")
  cat("Average FNR:", round(average_fnr * 100, 2), "+/-",
      round(sd_fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average variable importance ranking:\n")
  print(variable_importance_df)
  cat("----------------------------------------\n")
}


# Learning function: f'_learn
learn <- function(data) {
  # Logistic regression
  model <- randomForest(Attrition_Flag ~ ., data = train.bank, ntree = 500,
                        seed=123, importance = TRUE)
  return(model)
}

# Prediction function: f'_predict
predict_prime <- function(model, data, tau = 0.5) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "prob")[, 2] > tau
  return(predicted)
}

# Prediction function: f''_predict
predict_second <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "prob")[, 2]
  return(predicted_probs)
}

assess <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response")
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == FALSE) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, predict_second(model, data))
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
  # Extract variable importance
  variable_importance <- importance(bank.rf, type = 2)
  
  # Create a data frame for variable names and importance values
  variable_importance_df <- data.frame(
    Variable = row.names(variable_importance),
    Mean_Gini_Decrease = as.numeric(variable_importance),
    row.names = NULL
  )
  
  # Sort the data frame by Importance in descending order
  variable_importance_df <- variable_importance_df[order(-variable_importance_df$Mean_Gini_Decrease), ]
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
    variable_importance = variable_importance_df
  )
  
  # Print results
  cat("----------------------------------------\n")
  print(results$confusion_matrix)
  cat("----------------------------------------\n")
  cat("Accuracy:", round(results$accuracy * 100, 2), "%\n")
  cat("Dummy classifier accuracy:",
      round(results$dummy_classifier_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AUC:", round(results$auc * 100, 2), "%\n")
  cat("Dummy classifier AUC:",
      round(results$dummy_classifier_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("FPR:", round(results$fpr * 100, 2), "%\n")
  cat("FNR:", round(results$fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Variable importance:\n")
  print(results$variable_importance)
  cat("----------------------------------------\n")

  
  return(results)
}

# k-fold cross validation function: f'_cv
cv <- function(data, k = 10) {
  # Create k equally size folds
  folds <- createFolds(data$Attrition_Flag, k = k)
  
  # Initialize vectors
  accuracy <- rep(0, k)
  auc <- rep(0, k)
  fpr <- rep(0, k)
  fnr <- rep(0, k)
  mean_decrease_list <- vector("list", k)
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    model <- learn(train)
    
    # Predict on the testing set
    predicted <- predict_prime(model, test)
    predicted_probs <- predict_second(model, test)
    
    # Positive, negatives and false cases
    p <- sum(predicted == TRUE)
    n <- sum(predicted == FALSE)
    fp <- sum(predicted == TRUE & test$Attrition_Flag == FALSE)
    fn <- sum(predicted == FALSE & test$Attrition_Flag == TRUE)
    
    # Compute the accuracy, Auc, FPR, FNR, AIC and BIC
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, predicted_probs)
    auc[i] <- auc(roc)
    fpr[i] <- fp / n
    fnr[i] <- fn / p
    mean_decrease_list[[i]] <- importance(model, type = 2)
  }
  
  # Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  # Compute average variable importance
  # Convert the list to a matrix to compute the average mean decrease
  mean_decrease_matrix <- do.call(cbind, mean_decrease_list)
  
  # Compute the average mean decrease across all iterations
  average_mean_decrease <- rowMeans(mean_decrease_matrix)
  sd_mean_decrease <- apply(mean_decrease_matrix, 1, sd)
  
  sorted_variable_importance <- sort(average_mean_decrease, decreasing = TRUE)
  
  # Convert variable importance to a data frame
  variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                       Mean_Gini_Decrease = sorted_variable_importance,
                                       Std_Dev = sd_mean_decrease,
                                       row.names = NULL)
  
  # Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
  sd_accuracy <- sd(accuracy)
  sd_auc <- sd(auc)
  sd_fpr <- sd(fpr)
  sd_fnr <- sd(fnr)
  
  # Print the average accuracy, AIC and BIC with their standard deviations
  cat("----------------------------------------\n")
  cat("Average accuracy:", round(average_accuracy * 100, 2), "+/-",
      round(sd_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average AUC:", round(average_auc * 100, 2), "+/-",
      round(sd_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average FPR:", round(average_fpr * 100, 2), "+/-",
      round(sd_fpr * 100, 2), "%\n")
  cat("Average FNR:", round(average_fnr * 100, 2), "+/-",
      round(sd_fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average variable importance ranking:\n")
  print(variable_importance_df)
  cat("----------------------------------------\n")
}


#-------------------------------------------------------------------------------
# Results with full dataset
#-------------------------------------------------------------------------------
# IMPORT LIBRARIES
library(randomForest)
library(caret)
library(adabag)   # Boosting

# FIRST TRY WITH BOOSTING
# Static splitting the dataset into the Training set and Test set
set.seed(1234)
index <- createDataPartition(bank$Attrition_Flag , p =0.8, list = FALSE)

train.bank <- bank[index,]
test.bank <- bank[-index,]
#cat("Attrited customers (rare class): ",table(train.bank$Attrition_Flag)[2])
#cat("Existing customers (maj class): ",table(train.bank$Attrition_Flag)[1])

# Fitting AdaBoost to the Training set and keep track of variable importance
bank.boost <- boosting(Attrition_Flag ~ ., data = train.bank, boos = TRUE)
# print(bank.boost)

# Predicting the Test set results
# pred <- predict(bank.boost, newdata = test.bank, type="response")
# pred$confusion
# pred$prob[,2] >0.5

# Extract information about variable selection
# Extract the variable selection information for each boosting iteration
# var_selection <- bank.boost$importance
# var_selection
# 
# # Sort the variable importance in descending order
# sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
# 
# # Convert variable importance to a data frame
# variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
#                                      Importance = sorted_variable_importance,
#                                      row.names = NULL)
# 
# # Print the variable importance as a column
# print(variable_importance_df)
# 
# # CV splitting the dataset into the Training set and Test set
# set.seed(1234)
# bank.boostcv <- boosting.cv(Attrition_Flag ~ ., data = bank, boos = TRUE,
#                              v=10)
# 
# # Confusion matrix for the best iteration
# bank.boostcv$confusion

#-------------------------------------------------------------------------------
# SECOND TRY WITH RANDOM FOREST
bank.rf <- randomForest(Attrition_Flag ~ ., data = train.bank, ntree = 500,
                        seed=123, importance = TRUE)

# # Predicting the Test set results
# pred <- predict(bank.rf, newdata = test.bank, type="response")
# pred
# pred<- pred >0.5
# confusion_matrix<- table(pred, test.bank$Attrition_Flag)
# confusion_matrix
# 
# print(bank.rf)

# Variable importance plot
varImpPlot(bank.rf, sort = TRUE, main = "Variable Importance")

# Variable importance table
# Extract variable importance
# variable_importance <- importance(bank.rf, type = 2)
# 
# # Create a data frame for variable names and importance values
# variable_importance_df <- data.frame(
#   Variable = row.names(variable_importance),
#   Importance = as.numeric(variable_importance),
#   row.names = NULL
# )
# 
# # Sort the data frame by Importance in descending order
# variable_importance_df <- variable_importance_df[order(-variable_importance_df$Importance), ]
# 
# # Print the variable importance as a data frame
# print(variable_importance_df)


# Plotting the tree
# plot(bank.rf, main = "Random Forest")
# legend("topright", colnames(bank.rf$err.rate), col = 1:3, fill = 1:3)

#-------------------------------------------------------------------------------
# ASSESSMENT
cat("Results on whole dataset:\n")
# AdaBoost
boost.results <- assess.boost(bank.boost, test.bank)
boost.resultscv <- cv.boost(bank, k = 10)

# Random Forest
rf.results <- assess(bank.rf, test.bank)
rf.resultscv <- cv(bank, k = 10)

#-------------------------------------------------------------------------------
# Results with reduced dataset
bank <- bank[, -c(2, 4, 5, 8, 9, 13, 15, 16, 20)]
cat("Results on reduced dataset:\n")

# FIRST TRY WITH BOOSTING
# Static splitting the dataset into the Training set and Test set
set.seed(1234)
index <- createDataPartition(bank$Attrition_Flag , p =0.8, list = FALSE)

train.bank <- bank[index,]
test.bank <- bank[-index,]
# cat("Attrited customers (rare class): ",table(train.bank$Attrition_Flag)[2])
# cat("Existing customers (maj class): ",table(train.bank$Attrition_Flag)[1])

# Fitting AdaBoost to the Training set and keep track of variable importance
bank.boost <- boosting(Attrition_Flag ~ ., data = train.bank, boos = TRUE)
# print(bank.boost)
# 
# # Predicting the Test set results
# pred <- predict(bank.boost, newdata = test.bank, type="response")
# pred$confusion
# pred$prob[,2] >0.5
# 
# # Extract information about variable selection
# # Extract the variable selection information for each boosting iteration
# var_selection <- bank.boost$importance
# 
# # Sort the variable importance in descending order
# sorted_variable_importance <- sort(var_selection, decreasing = TRUE)
# 
# # Convert variable importance to a data frame
# variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
#                                      Importance = sorted_variable_importance,
#                                      row.names = NULL)
# 
# # Print the variable importance as a column
# print(variable_importance_df)
# 
# # CV splitting the dataset into the Training set and Test set
# set.seed(1234)
# bank.boostcv <- boosting.cv(Attrition_Flag ~ ., data = bank, boos = TRUE,
#                             v=10)
# 
# # Confusion matrix for the best iteration
# bank.boostcv$confusion

#-------------------------------------------------------------------------------
# SECOND TRY WITH RANDOM FOREST
bank.rf <- randomForest(Attrition_Flag ~ ., data = train.bank, ntree = 500,
                        seed=123, importance = TRUE)

# Predicting the Test set results
# pred <- predict(bank.rf, newdata = test.bank, type="response")
# pred
# pred<- pred >0.5
# confusion_matrix<- table(pred, test.bank$Attrition_Flag)
# confusion_matrix
# 
# print(bank.rf)

# Variable importance plot
varImpPlot(bank.rf, sort = TRUE, main = "Variable Importance")

# Variable importance table
# Extract variable importance
# variable_importance <- importance(bank.rf, type = 2)
# 
# # Create a data frame for variable names and importance values
# variable_importance_df <- data.frame(
#   Variable = row.names(variable_importance),
#   Importance = as.numeric(variable_importance),
#   row.names = NULL
# )
# 
# # Sort the data frame by Importance in descending order
# variable_importance_df <- variable_importance_df[order(-variable_importance_df$Importance), ]
# 
# # Print the variable importance as a data frame
# print(variable_importance_df)


# Plotting the tree
# plot(bank.rf, main = "Random Forest")
# legend("topright", colnames(bank.rf$err.rate), col = 1:3, fill = 1:3)

#-------------------------------------------------------------------------------
# ASSESSMENT
# AdaBoost
boost.results <- assess.boost(bank.boost, test.bank)
boost.resultscv <- cv.boost(bank, k = 10)

# Random Forest
rf.results <- assess(bank.rf, test.bank)
rf.resultscv <- cv(bank, k = 10)




#-------------------------------------------------------------------------------
# DEBUGGING CHUNK
k<-10
data<-bank
# Create k equally size folds
folds <- createFolds(data$Attrition_Flag, k = k)

# Initialize vectors
accuracy <- rep(0, k)
auc <- rep(0, k)
fpr <- rep(0, k)
fnr <- rep(0, k)
mean_decrease_list <- vector("list", k)

# For each fold
for (i in 1:k) {
  # Split the data into training and testing sets
  train <- data[-folds[[i]], ]
  test <- data[folds[[i]], ]
  
  # Train the model on the training set
  model <- learn(train)
  
  # Predict on the testing set
  predicted <- predict_prime(model, test)
  predicted_probs <- predict_second(model, test)
  
  # Positive, negatives and false cases
  p <- sum(predicted == TRUE)
  n <- sum(predicted == FALSE)
  fp <- sum(predicted == TRUE & test$Attrition_Flag == FALSE)
  fn <- sum(predicted == FALSE & test$Attrition_Flag == TRUE)
  
  # Compute the accuracy, Auc, FPR, FNR, AIC and BIC
  accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
  roc <- roc(test$Attrition_Flag, predicted_probs)
  auc[i] <- auc(roc)
  fpr[i] <- fp / n
  fnr[i] <- fn / p
  mean_decrease_list[[i]] <- importance(model, type = 2)
}

# Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
average_accuracy <- mean(accuracy)
average_auc <- mean(auc)
average_fpr <- mean(fpr)
average_fnr <- mean(fnr)

# Compute average variable importance
# Convert the list to a matrix to compute the average mean decrease
mean_decrease_list
mean_decrease_matrix <- do.call(cbind, mean_decrease_list)
mean_decrease_matrix

# Compute the average mean decrease across all iterations
average_mean_decrease <- rowMeans(mean_decrease_matrix)
average_mean_decrease
sd_mean_decrease <- apply(mean_decrease_matrix, 1, sd)
sd_mean_decrease

sorted_variable_importance <- sort(average_mean_decrease, decreasing = TRUE)

# Convert variable importance to a data frame
variable_importance_df <- data.frame(Variable = names(sorted_variable_importance),
                                     Importance = sorted_variable_importance,
                                     Std_Dev = sd_mean_decrease,
                                     row.names = NULL)
variable_importance_df

# Compute the standard deviation of the accuracy, AUC, FPR, FNR, AIC and BIC
sd_accuracy <- sd(accuracy)
sd_auc <- sd(auc)
sd_fpr <- sd(fpr)
sd_fnr <- sd(fnr)

# Print the average accuracy, AIC and BIC with their standard deviations
cat("----------------------------------------\n")
cat("Average accuracy:", round(average_accuracy * 100, 2), "+/-",
    round(sd_accuracy * 100, 2), "%\n")
cat("----------------------------------------\n")
cat("Average AUC:", round(average_auc * 100, 2), "+/-",
    round(sd_auc * 100, 2), "%\n")
cat("----------------------------------------\n")
cat("Average FPR:", round(average_fpr * 100, 2), "+/-",
    round(sd_fpr * 100, 2), "%\n")
cat("Average FNR:", round(average_fnr * 100, 2), "+/-",
    round(sd_fnr * 100, 2), "%\n")
cat("----------------------------------------\n")