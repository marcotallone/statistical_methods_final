# Load libraries
library(car)

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

# ⚠️ Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]

# Converting Attrition_Flag as a binary variable 0 = Existing Customer, 1 = Attrited Customer
bank$Attrition_Flag <- ifelse(bank$Attrition_Flag == "Existing Customer", 0, 1)

# Encode categorical variables as factors
bank$Dependent_count <- as.factor(bank$Dependent_count)
bank$Education_Level <- as.factor(bank$Education_Level)
bank$Marital_Status <- as.factor(bank$Marital_Status)
bank$Income_Category <- as.factor(bank$Income_Category)
bank$Card_Category <- as.factor(bank$Card_Category)

# Change the names of the columns for easier use
# colnames(bank)[1] <- "ID"
# colnames(bank)[2] <- "attrition"
# colnames(bank)[3] <- "age"
# colnames(bank)[4] <- "gender"
# colnames(bank)[5] <- "dependent"
# colnames(bank)[6] <- "education"
# colnames(bank)[7] <- "marital"
# colnames(bank)[8] <- "income"
# colnames(bank)[9] <- "card"
# colnames(bank)[10] <- "months"
# colnames(bank)[11] <- "contacts"
# colnames(bank)[12] <- "products"
# colnames(bank)[13] <- "cc"
# colnames(bank)[14] <- "online"
# colnames(bank)[15] <- "credit"
# colnames(bank)[16] <- "debit"
# colnames(bank)[17] <- "purchases"
# colnames(bank)[18] <- "balance"
# colnames(bank)[19] <- "open"
# colnames(bank)[20] <- "revolving"
# colnames(bank)[21] <- "transfers"

# Select only numeric columns
numeric_columns <- sapply(bank, is.numeric)

# Correlation matrix of the numeric columns
# chart.Correlation(bank[, numeric_columns]) # <- takes too long to run
round(cor(bank[, numeric_columns]), 2)

# Plot the correlation matrix with a heatmap
heatmap(cor(bank[, numeric_columns]),
        Rowv = NA,
        Colv = NA,
        col = c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"),
        scale = "none",
        margins = c(5, 10))

# Logistic regression to predict the Attrition_Flag variable
bank_logistic <- glm(Attrition_Flag ~ .,
                     data = bank,
                     family = binomial(link = "logit"))

# Summary of the model
summary(bank_logistic)