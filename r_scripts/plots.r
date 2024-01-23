# LIBRARIES --------------------------------------------------------------------

library(ggplot2)
library(ggExtra)
library(ggforce)
library(patchwork)
library(RColorBrewer)

# LOADING AND PREPROCESSING ----------------------------------------------------

# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

# ⚠️ Remove the first and last two columns as suggested in the README
bank <- bank[, -c(1, 22, 23)]

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

# FIlter numerical variables and categorical variables
bank_num <- bank[, sapply(bank, is.numeric)]
bank_cat <- bank[, sapply(bank, is.factor)]

summary(bank)

# PLOTS ------------------------------------------------------------------------

plot_continuous <- function(dataset, variable, title, xlab, bins_width = 1) {
  p1 <- ggplot(dataset, aes(x = {{ variable }},
                            fill = as.factor(Attrition_Flag))) +
    geom_histogram(binwidth = bins_width, color = "#FFFFFF") +
    labs(title = paste("Histogram of", title), x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = c(.85, .85),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  p2 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
    geom_sina(aes(group = Attrition_Flag), alpha = .25) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p3 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_boxplot(fill = "gray80", alpha = .5,
                 outlier.size = 4, outlier.alpha = .75) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p4 <- ggplot(dataset, aes(x = {{ variable }})) +
    stat_ecdf(geom = "step", color = "royalblue") +
    labs(title = paste("Cumulative Distribution of", title),
         x = xlab, y = "Cumulative Distribution") +
    theme(legend.position = "none", aspect.ratio = 1)

  (p1 / p4 / p2 / p3) + plot_layout(ncol = 2)
}

plot_discrete <- function(dataset, variable, title, xlab) {
  p1 <- ggplot(dataset, aes(x = as.factor({{ variable }}),
                            fill = as.factor(Attrition_Flag))) +
    geom_bar(color = "#FFFFFF") +
    labs(title = paste("Barplot of", title), x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = c(.85, .85),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  p2 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
    geom_sina(aes(group = Attrition_Flag), alpha = .25) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p3 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = xlab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_boxplot(fill = "gray80", alpha = .5,
                 outlier.size = 4, outlier.alpha = .75) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p4 <- ggplot(dataset, aes(x = {{ variable }})) +
    stat_ecdf(geom = "step", color = "royalblue") +
    labs(title = paste("Cumulative Distribution of", title),
         x = xlab, y = "Cumulative Distribution") +
    theme(legend.position = "none", aspect.ratio = 1)

  (p1 / p4 / p2 / p3) + plot_layout(ncol = 2)
}

# Plot continuous and discrete variables
plot_continuous(bank, Customer_Age, "Customer Age", "Age")
plot_discrete(bank, Dependent_count, "Number of Dependents", "Dependents")
plot_discrete(bank, Months_on_book, "Months on Book", "Months")
plot_discrete(bank, Total_Relationship_Count, "Total Relationship Count", "Count")
plot_discrete(bank, Months_Inactive_12_mon, "Months Inactive (12 months)", "Months")
plot_discrete(bank, Contacts_Count_12_mon, "Contacts Count (12 months)", "Count")
plot_continuous(bank, Credit_Limit, "Credit Limit", "Credit Limit", 1000)
plot_continuous(bank, Total_Revolving_Bal, "Total Revolving Balance", "Balance", 100)
plot_continuous(bank, Avg_Open_To_Buy, "Average Open to Buy", "Balance", 1000)
plot_continuous(bank, Total_Amt_Chng_Q4_Q1, "Total Amount Change (Q4-Q1)", "Amount Change", 0.25)
plot_continuous(bank, Total_Trans_Amt, "Total Transaction Amount", "Amount", 1000)
plot_discrete(bank, Total_Trans_Ct, "Total Transaction Count", "Count")
plot_continuous(bank, Total_Ct_Chng_Q4_Q1, "Total Count Change (Q4-Q1)", "Count Change", 0.25)
plot_continuous(bank, Avg_Utilization_Ratio, "Average Utilization Ratio", "Ratio", 0.1)
