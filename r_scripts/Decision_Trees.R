
install.packages("caret")
# LOADING AND PREPROCESSING ----------------------------------------------------

# Set working directory as this directory
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

setwd("/Users/vishal/Documents/STATISTICAL METHODS/Project")

# Load the dataset from the datasets/ folder
#bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

bank <- read.csv("/Users/vishal/Documents/STATISTICAL METHODS/Project/Dataset/BankChurners.csv", sep = ",")

# ?????? Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]

# ?????? Remove the first column as it is just an index
bank <- bank[, -1]

# ?????? Convert the Attrition_Flag column to a binary variable:
# - 0: Existing Customer
# - 1: Attrited Customer
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)

# Convert all categorical variables to factors
bank$Gender <- as.factor(bank$Gender)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)


#Classification tree

#Libraries ---------------------------------------------------------------

library(MASS)
library(rpart)
library(rpart.plot)
library(caret)


# Functions ---------------------------------------------------------------

# Function for creating train and test data

create_train_test <- function(data, train_size = 0.8, train = TRUE) {
  num_rows = nrow(data)
  total_rows = size * num_rows
  train_sample <- 1: total_rows
  if (train == TRUE) {
    train_data <- data[train_sample, ]
  } else {
    test_data <- data[-train_sample, ]
  }
}

# classification function
classify <- function(data) {
  # Classification tree
  fit <- rpart(Attrition_Flag ~ .,
               method = "class",
               data = data)
  return(fit)
}


# classification tree plot function 

tree <- function(fit){
  
  rpart.plot(fit, extra=1, box.palette="auto")
}
  

# Function for plotting error rate for different cp values. cp = complexity parameter

classify <- function(data) {
  # Classification tree
  fit <- rpart(Attrition_Flag ~ .,
               method = "class",
               data = data)
  
  plotcp(fit) # plots error rate by cp=complexity parameter for pruning
}


# Prediction function
prediction_test_data <- function(fitted_model, data) {
  # Predicted classes
  predicted_class <- predict(fit, newdata = data, type = "class")
  return(predicted_class)
  data$Predicted_class <- predicted_class
}

# Function for confusion matrix and results
 output_matrix <- function(data) {
  # Confusion matrix
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = Predicted_class)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")
  
  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == 0) / length(actual)
  
  # False positive rate
  fpr <- confusion_matrix[1, 2] / sum(confusion_matrix[1, ])
  
  # False negative rate
  fnr <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  
  # ROC curve and AUC
  roc <- roc(actual, Predicted_class, data)
  auc <- auc(roc)
  
  # Dummy classifier ROC curve and AUC
  dummy_classifier_roc <- roc(actual, rep(0, length(actual)))
  dummy_classifier_auc <- auc(dummy_classifier_roc)
  
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
    fnr = fnr
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
 
  
  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    # Train the model on the training set
    fit <- classify(train)
    
    # Predict on the testing set
    Predicted_class <- prediction_test_data(model, test)
    
    
    # Positive, negatives and false cases
    p <- sum(predicted == 1)
    n <- sum(predicted == 0)
    fp <- sum(predicted == 1 & test$Attrition_Flag == 0)
    fn <- sum(predicted == 0 & test$Attrition_Flag == 1)
    
    # Compute the accuracy, Auc, FPR, FNR
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    roc <- roc(test$Attrition_Flag, Predicted_class)
    auc[i] <- auc(roc)
    fpr[i] <- fp / n
    fnr[i] <- fn / p
  }
  
  # Compute the average accuracy, AUC, FPR, FNR, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_auc <- mean(auc)
  average_fpr <- mean(fpr)
  average_fnr <- mean(fnr)
  
  
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
  
}



# Tuning Classification tree

#Accuracy function (to compute accuracy for tuned fit)

tuned_accuracy <- function(fit) {
  prediction_test_data(fit, test_data)
  confusion_matrix <- table(test_data$Atrrition_Flag, test_data$Predicted_class)
  accuracy_test_data <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracy_test_data
}

#Get control 
control <- rpart.control(minsplit = 4,
                           minbucket = round(5 / 3),
                           maxdepth = 3,
                           cp = 0)
tuned_fit <- rpart(Attrition_Flag~., data = train_data, method = 'class', control = control)
tuned_accuracy(tuned_fit)
