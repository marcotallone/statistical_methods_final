# LOADING AND PREPROCESSING ----------------------------------------------------

library(car)

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
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

# FIlter numerical variables
bank_num <- bank[, sapply(bank, is.numeric)]

# Print column names and their index
for (i in 1:length(colnames(bank_num))) {
  print(paste(i, colnames(bank_num)[i]))
}

# LOGISTIC REGRESSION ---------------------------------------------------------

bank_filtered <- bank_num[, -c(2, 3, 4, 8, 10, 11, 15)]

# Add a log(bank$Total_Trans_Amt) column to the bank dataset
bank$log_Total_Trans_Amt <- log(bank$Total_Trans_Amt)

bank_logistic <- glm(Attrition_Flag ~
                       + bank$Gender
                       + bank$Marital_Status
                       + bank$Income_Category
                       + bank$Total_Relationship_Count
                       + bank$Months_Inactive_12_mon
                       + bank$Contacts_Count_12_mon
                       + bank$Total_Revolving_Bal
                       + bank$log_Total_Trans_Amt
                       + bank$Total_Trans_Ct
                       + bank$Total_Ct_Chng_Q4_Q1,
                     data = bank_filtered,
                     family = binomial(link = "logit"))

# ASSESSING THE MODEL ---------------------------------------------------------

model <- bank_logistic
data <- bank_filtered

# Confusion matrix
predicted <- predict(model, type = "response") > 0.5
actual <- data$Attrition_Flag
confusion_matrix <- table(Actual = actual, Predicted = predicted)
colnames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")
rownames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")

# Accuracy from confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Dummy clasifier accuracy
dummy_classifier_accuracy <- sum(actual == 0) / length(actual)

# AIC
aic <- AIC(model)

# BIC
bic <- BIC(model)

# RESULTS ---------------------------------------------------------------------

# Model summary
summary(model)

# Confusion matrix
confusion_matrix

# Accuracy
accuracy
dummy_classifier_accuracy

# AIC
aic

# BIC
bic

# Anova test
anova(model, test = "Chisq")

# VIF test
vif(model)

# # STANDARDIZATION OF THE USED VARIABLES ----------------------------------------

# # Standardize the numerical variables
# bank_filtered_standardized <- bank_filtered

# # Standardize the numerical variables except for the Attrition_Flag
# bank_filtered_standardized[, -1] <- scale(bank_filtered_standardized[, -1])

# # Logistic regression
# bank_logistic_standardized <- glm(Attrition_Flag ~ .
#                                   + bank$Gender
#                                   + bank$Marital_Status
#                                   + bank$Income_Category,
#                                   data = bank_filtered_standardized,
#                                   family = binomial(link = "logit"))

# # Confusion matrix
# predicted <- predict(bank_logistic_standardized, type = "response") > 0.5
# actual <- data$Attrition_Flag
# confusion_matrix <- table(Actual = actual, Predicted = predicted)
# colnames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")
# rownames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")

# # Accuracy from confusion matrix
# accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# # Dummy clasifier accuracy
# dummy_classifier_accuracy <- sum(actual == 0) / length(actual)

# # RESULTS ---------------------------------------------------------------------

# # Model summary
# summary(bank_logistic_standardized)

# # Confusion matrix
# confusion_matrix

# # Accuracy
# accuracy
# dummy_classifier_accuracy

# # Anova test
# anova(bank_logistic_standardized, test = "Chisq")

# # VIF test
# vif(bank_logistic_standardized)
