# LOADING AND PREPROCESSING ----------------------------------------------------

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

# LOGISTIC REGRESSION ---------------------------------------------------------
# FIlter numerical variables
bank_num <- bank[, sapply(bank, is.numeric)]

# Logistic regression to predict the Attrition_Flag variable
# only with the numerical variables
bank_logistic <- glm(Attrition_Flag ~ .,
                     data = bank_num,
                     family = binomial(link = "logit"))

# Summary of the model
summary(bank_logistic)

# Removing the variables with a p-value > 0.05
bank_filtered <- bank_num[, -c(2, 4, 8, 10, 11, 15)]

# Logistic regression to predict the Attrition_Flag variable
# only with the numerical variables and the variables with a p-value < 0.05
bank_logistic_filtered <- glm(Attrition_Flag ~ .,
                              data = bank_filtered,
                              family = binomial(link = "logit"))
summary(bank_logistic_filtered)
