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

# 1. Full model with all numerical values --------------------------------------
# Logistic regression to predict the Attrition_Flag variable
# only with the numerical variables
bank_logistic <- glm(Attrition_Flag ~ .,
                     data = bank_num,
                     family = binomial(link = "logit"))

# 2. Reduced model using p-values ---------------------------------------------
# Removing the variables with a p-value > 0.05
bank_filtered_p_values <- bank_num[, -c(2, 4, 8, 10, 11, 15)]

# Logistic regression to predict the Attrition_Flag variable
# only with the numerical variables and the variables with a p-value < 0.05
bank_logistic_p_values <- glm(Attrition_Flag ~ .,
                              data = bank_filtered_p_values,
                              family = binomial(link = "logit"))

# 3. Reduced model using p-values results and correlation matrix --------------
# Removing variables not correlated with response and "highly" correlated
# with other covariates
bank_filtered_corr <- bank_num[, -c(2, 3, 4, 8, 10, 11, 12, 15)]

bank_logistic_corr <- glm(Attrition_Flag ~ .,
                          data = bank_filtered_corr,
                          family = binomial(link = "logit"))

print("Full model")
summary(bank_logistic)
print("Reduced model using p-values")
summary(bank_logistic_p_values)
print("Reduced model using p-values and correlation matrix")
summary(bank_logistic_corr)

# ASSESSING THE MODEL ---------------------------------------------------------

model <- bank_logistic
data <- bank_filtered

# Confusion matrix
predicted <- predict(model, type = "response") > 0.5
actual <- data$Attrition_Flag

# Create a confusion matrix with improved labels
# Define the "actual" and "predicted" objects
actual <- data$Attrition_Flag
predicted <- predict(model, type = "response") > 0.5

confusion_matrix <- table(Actual = actual, Predicted = predicted)
colnames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")
rownames(confusion_matrix) <- c("Existing Customer", "Attrited Customer")

confusion_matrix

# Accuracy from confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

# Anova test
anova(model, test = "Chisq")

# VIF test
vif(model)
