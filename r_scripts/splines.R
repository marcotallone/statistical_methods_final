library(MASS)
library(splines)
library(pROC)
library(mgcv)

##PREPROCESSING (according to previous codes)---------------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset and pre-process it
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

# ⚠️ Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]

# ⚠️ Remove the first column as it is just an index
bank <- bank[, -1]

# ⚠️ Convert the Attrition_Flag column to a binary variable:
# - 0: Existing Customer
# - 1: Attrited Customer
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Attrited Customer", 1, 0)

# Convert all categorical variables to factors
bank$Gender <- as.factor(bank$Gender)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)
bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)

#FUNCTIONS--------------------------------------------------
predict_second <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, newdata = data, type = "response")
  return(predicted_probs)
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
  
  # AIC
  aic <- AIC(model)
  
  # BIC
  bic <- BIC(model)
  
  # Results
  results <- list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    dummy_classifier_accuracy = dummy_classifier_accuracy,
    auc = auc,
    dummy_classifier_auc = dummy_classifier_auc,
    fpr = fpr,
    fnr = fnr,
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
  cat("AUC:", round(results$auc * 100, 2), "%\n")
  cat("Dummy classifier AUC:",
      round(results$dummy_classifier_auc * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("FPR:", round(results$fpr * 100, 2), "%\n")
  cat("FNR:", round(results$fnr * 100, 2), "%\n")
  cat("----------------------------------------\n")
  cat("AIC:", results$aic, "\n")
  cat("BIC:", results$bic, "\n")
  cat("----------------------------------------\n")
  
  return(results)
}

##MODELS---------------------------------------------------------------

gamfit_first_try<-gam(Attrition_Flag ~ s(Customer_Age)+Gender+Dependent_count
            +Education_Level+Marital_Status
            +Income_Category+Card_Category+s(Months_on_book )
            +Total_Relationship_Count+Months_Inactive_12_mon
            +Contacts_Count_12_mon+s(Credit_Limit)+s(Total_Revolving_Bal)
            +s(Avg_Open_To_Buy)+s(Total_Amt_Chng_Q4_Q1)
            +s(Total_Trans_Amt)+s(Total_Trans_Ct)+s(Total_Ct_Chng_Q4_Q1)
            +s(Avg_Utilization_Ratio), data=bank)
summary(gamfit_first_try)

#considering edf in the summary I change as additive the variables that has edf<2
gamfit<-gam(Attrition_Flag ~ s(Customer_Age)+Gender+Dependent_count
            +Education_Level+Marital_Status
            +Income_Category+Card_Category+Months_on_book 
            +Total_Relationship_Count+Months_Inactive_12_mon
            +Contacts_Count_12_mon+Credit_Limit+s(Total_Revolving_Bal)
            +Avg_Open_To_Buy+s(Total_Amt_Chng_Q4_Q1)
            +s(Total_Trans_Amt)+s(Total_Trans_Ct)+s(Total_Ct_Chng_Q4_Q1)
            +s(Avg_Utilization_Ratio), data=bank)
summary(gamfit)

##ASSESSING--------------------------------------------------------





cat("Results on whole dataset:\n")
results <- assess(gamfit, bank)


anova(bank_logistic, test = "Chisq")
vif(bank_logistic)