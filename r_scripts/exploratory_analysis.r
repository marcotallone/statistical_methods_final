# LIBRARIES --------------------------------------------------------------------
library(corrplot)
library(hexbin)
library(ggplot2)
library(ggExtra)

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

# EXPLORATORY ANALYSIS --------------------------------------------------------

# Structure
str(bank)

# 1. Among the numerical variables, which ones are the most correlated with the
#    Attrition_Flag variable?
# -----------------------------------------------------------------------------
# FIlter numerical variables
bank_num <- bank[, sapply(bank, is.numeric)]

# Compute the correlation matrix
round(cor(bank_num), 2)

# Plot the correlation matrix as a heatmap with a color scale from -1 to 1
corrplot(cor(bank_num), method = "color", type = "upper", tl.cex = 0.7,
         col = colorRampPalette(c("#000075", "white", "#a50000"))(100),
         addCoef.col = "black", number.cex = 0.7)

# 2. Boxplots of numerical variables w.r.t. Attrition_Flag
# -----------------------------------------------------------------------------
par(mfrow = c(2, 2))

boxplot(Customer_Age ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Age", ylab = "Attrition")

boxplot(Dependent_count ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Number of dependents", ylab = "Attrition")

boxplot(Months_on_book ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Months on book", ylab = "Attrition")

boxplot(Contacts_Count_12_mon ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Number of contacts in the last 12 months",
        ylab = "Attrition")

# -----------------------------------------------------------------------------
par(mfrow = c(2, 2))

boxplot(Total_Relationship_Count ~ Attrition_Flag, data = bank,
        horizontal = TRUE,
        xlab = "Total number of products held", ylab = "Attrition")

boxplot(Months_Inactive_12_mon ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Months inactive in the last 12 months", ylab = "Attrition")

boxplot(Contacts_Count_12_mon ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Number of contacts in the last 12 months",
        ylab = "Attrition")

boxplot(Avg_Utilization_Ratio ~ Attrition_Flag, data = bank,
        horizontal = TRUE,
        xlab = "Average card utilization ratio",
        ylab = "Attrition")

# -----------------------------------------------------------------------------
par(mfrow = c(2, 2))

boxplot(Credit_Limit ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Credit limit", ylab = "Attrition")

boxplot(Total_Revolving_Bal ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Total revolving balance", ylab = "Attrition")

boxplot(Avg_Open_To_Buy ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Average open to buy", ylab = "Attrition")

boxplot(Total_Amt_Chng_Q4_Q1 ~ Attrition_Flag, data = bank, horizontal = TRUE,
        xlab = "Change in transaction amount (Q4 over Q1)",
        ylab = "Attrition")

# 3. Barplots of numerical variables w.r.t. Attrition_Flag
# -----------------------------------------------------------------------------
par(mfrow = c(1, 1))
barplot(prop.table(table(bank$Attrition_Flag, bank$Customer_Age),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Age (years)", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag, bank$Dependent_count),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Number of dependents", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag, bank$Months_on_book),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Months on book", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag, bank$Contacts_Count_12_mon),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Number of contacts in the last 12 months",
        ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag, bank$Total_Relationship_Count),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Total number of products held", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag, bank$Months_Inactive_12_mon),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Months inactive in the last 12 months",
        ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Avg_Utilization_Ratio,
                             breaks = seq(0, 1, length.out = 11))),
                   margin = 1),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Average card utilization ratio",
        ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Credit_Limit, breaks = 10))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Credit limit", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Total_Revolving_Bal, breaks = 10))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Total revolving balance", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Avg_Open_To_Buy, breaks = 10))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Average open to buy", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Total_Amt_Chng_Q4_Q1, breaks = 20))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Change in transaction amount (Q4 over Q1)",
        ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Total_Trans_Amt, breaks = 10))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Total Transaction Amount", ylab = "Proportion")

barplot(prop.table(table(bank$Attrition_Flag,
                         cut(bank$Total_Trans_Ct, breaks = 10))),
        beside = TRUE,
        legend.text = c("Not Churned", "Churned"),
        xlab = "Total Transaction Count", ylab = "Proportion")


# 4. Other usefulf plots -------------------------------------------------------

# Scatterplot of Total_Trans_Ct vs Total_Trans_Amt where the points are colored
# using the Attrition_Flag variable
plot(bank$Total_Trans_Amt, bank$Total_Trans_Ct,
     col = ifelse(bank$Attrition_Flag == 1, "red", "blue"),
     pch = 19, # Use filled circles for points
     xlab = "Total Transaction Amount", ylab = "Total Transaction Count")
legend("topright", legend = c("Existing Customer", "Attrited Customer"),
       col = c("blue", "red"), pch = 19)

# Density plot of Total_Trans_Ct vs Total_Trans_Amt
# Create a hexbin plot
hexbinplot(Total_Trans_Ct ~ Total_Trans_Amt, data = bank,
           xlab = "Total Transaction Amount", ylab = "Total Transaction Count",
           main = "Hexbin Plot")

# 5. Plots for categorical variables -------------------------------------------

# Barplot of Attrition_Flag
barplot(prop.table(table(bank$Attrition_Flag)),
        xlab = "Attrition Flag", ylab = "Proportion")

#6. Plots of distributions -----------------------------------------------------

# Histogram of Customer_Age
hist(bank$Customer_Age, xlab = "Age (years)", ylab = "Frequency")

# Histogram of Dependent_count
hist(bank$Dependent_count, xlab = "Number of dependents", ylab = "Frequency")

# Histogram of Months_on_book
hist(bank$Months_on_book, xlab = "Months on book", ylab = "Frequency")

# Histogram of Contacts_Count_12_mon
hist(bank$Contacts_Count_12_mon,
     xlab = "Number of contacts in the last 12 months", ylab = "Frequency")

# Histogram of Total_Relationship_Count
hist(bank$Total_Relationship_Count,
     xlab = "Total number of products held", ylab = "Frequency")

# Histogram of Months_Inactive_12_mon
hist(bank$Months_Inactive_12_mon,
     xlab = "Months inactive in the last 12 months", ylab = "Frequency")

# Histogram of Avg_Utilization_Ratio
hist(bank$Avg_Utilization_Ratio,
     xlab = "Average card utilization ratio", ylab = "Frequency")

# Histogram of Credit_Limit
hist(bank$Credit_Limit, xlab = "Credit limit", ylab = "Frequency")

# Histogram of Total_Revolving_Bal
hist(bank$Total_Revolving_Bal, xlab = "Total revolving balance",
     ylab = "Frequency")

# Histogram of Avg_Open_To_Buy
hist(bank$Avg_Open_To_Buy, xlab = "Average open to buy", ylab = "Frequency")

# Histogram of Total_Amt_Chng_Q4_Q1
hist(bank$Total_Amt_Chng_Q4_Q1,
     xlab = "Change in transaction amount (Q4 over Q1)", ylab = "Frequency")

# Histogram of Total_Trans_Amt
hist(bank$Total_Trans_Amt, xlab = "Total Transaction Amount",
     ylab = "Frequency")

hist(log(bank$Total_Trans_Amt), xlab = "Log of Total Transaction Amount",
     ylab = "Frequency")

# Histogram of Total_Trans_Ct
hist(bank$Total_Trans_Ct, xlab = "Total Transaction Count",
     ylab = "Frequency")

hist(log(bank$Total_Trans_Ct), xlab = "log of Total Transaction Count",
     ylab = "Frequency")

# Histogram of Total_Ct_Chng_Q4_Q1
hist(bank$Total_Ct_Chng_Q4_Q1,
     xlab = "Change in transaction count (Q4 over Q1)", ylab = "Frequency")

hist(log(bank$Total_Ct_Chng_Q4_Q1),
     xlab = "Change in transaction count (Q4 over Q1)", ylab = "Frequency")
