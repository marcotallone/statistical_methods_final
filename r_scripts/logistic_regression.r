# LIBRARIES --------------------------------------------------------------------
library(car)
library(caret)

# FUNCTIONS --------------------------------------------------------------------

# Learning function: f'_learn
learn <- function(data) {
  # Logistic regression
  model <- glm(Attrition_Flag ~ .,
               data = data,
               family = binomial(link = "logit"))
  return(model)
}

# Prediction function: f'_predict
predict_prime <- function(model, data) {
  # Predictions
  predicted <- predict(model, newdata = data, type = "response") > 0.5
  return(predicted)
}

# Function to assess the model later
assess <- function(model, data) {
  # Confusion matrix
  predicted <- predict(model, newdata = data, type = "response") > 0.5
  actual <- data$Attrition_Flag
  confusion_matrix <- table(Actual = actual, Predicted = predicted)
  colnames(confusion_matrix) <- c("Existing", "Attrited")
  rownames(confusion_matrix) <- c("Existing", "Attrited")

  # Accuracy from confusion matrix
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

  # Dummy clasifier accuracy
  dummy_classifier_accuracy <- sum(actual == 0) / length(actual)

  # AIC
  aic <- AIC(model)

  # BIC
  bic <- BIC(model)

  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    aic = aic,
    bic = bic
  )

  # Print results
  cat("----------------------------------------\n")
  print(results$confusion_matrix)
  cat("----------------------------------------\n")
  cat("Accuracy:", round(results$accuracy * 100, 2), "%\n")
  cat("Dummy classifier accuracy:",
      round(results$dummy_classifier_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AIC:", results$aic, "\n")
  cat("BIC:", results$bic, "\n")
  cat("----------------------------------------\n")

  return(results)
}

# k-fold cross validation function: f'_cv
cv <- function(data, k = 10) {
  # Create k equally size folds
  folds <- createFolds(data$Attrition_Flag, k = k)

  # Initialize a vector to hold the accuracy for each fold
  accuracy <- rep(0, k)

  # Initialize a vector to hold the AIC for each fold
  aic <- rep(0, k)

  # Initialize a vector to hold the BIC for each fold
  bic <- rep(0, k)

  # For each fold
  for (i in 1:k) {
    # Split the data into training and testing sets
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]

    # Train the model on the training set
    model <- learn(train)

    # Predict on the testing set
    predicted <- predict_prime(model, test)

    # Compute the accuracy, AIC and BIC
    accuracy[i] <- sum(predicted == test$Attrition_Flag) / nrow(test)
    aic[i] <- AIC(model)
    bic[i] <- BIC(model)
  }

  # Compute the average accuracy, AIC and BIC
  average_accuracy <- mean(accuracy)
  average_aic <- mean(aic)
  average_bic <- mean(bic)

  # Print the average accuracy
  cat("----------------------------------------\n")
  cat("Average accuracy:", round(average_accuracy * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("Average AIC:", average_aic, "\n")
  cat("Average BIC:", average_bic, "\n")
  cat("----------------------------------------\n")
}

# PRE-PROCESSING AND FEATURE EN\GINEERING --------------------------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")
bank <- bank[, -c(1, 3, 5, 6, 9, 10, 14, 16, 17, 21, 22, 23)]
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)

# Convert all categorical variables to factors
bank$Gender <- as.factor(bank$Gender)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)

# Override the Total_Trans_Amt variable with its log
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)

# Standardization (optional) all columns except response and categorical
bank[, -c(1, 2, 3, 4)] <- scale(bank[, -c(1, 2, 3, 4)])

# LOGISTIC REGRESSION "LEARNING" PHASE -----------------------------------------

bank_logistic <- learn(bank)
summary(bank_logistic)

# ASSESSING THE MODEL ---------------------------------------------------------

cat("Results on whole dataset:\n")
results <- assess(bank_logistic, bank)

cat("Results on 10-fold cross validation:\n")
cv(bank)

# ANOVA AND VIF TESTS ---------------------------------------------------------

anova(bank_logistic, test = "Chisq")
vif(bank_logistic)
