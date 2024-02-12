# LIBRARIES --------------------------------------------------------------------

library(ggplot2)
library(ggExtra)
library(ggforce)
library(patchwork)
library(RColorBrewer)
library(dplyr)
library(corrr)
library(forcats)

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

  # Save the plot in the plots/ folder
  ggsave(paste("../plots/", title, ".png", sep = ""), width = 10, height = 10)
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

  # Save the plot in the plots/ folder
  ggsave(paste("../plots/", title, ".png", sep = ""), width = 10, height = 10)
}

plot_categorical <- function(dataset, variable, xlab) {

  # Compute the position of labels
  class_prop <- dataset %>%
    count({{variable}}) %>%
    mutate(prop = n / sum(n) * 100,
           ypos = cumsum(prop) - 0.5 * prop)

  # Create the pie chart
  pie <- ggplot(class_prop, aes(x = "", y = prop, fill = {{variable}})) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "top",
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal", aspect.ratio = 1) +
    # geom_text(aes(y = ypos, label = paste(round(prop, 1), "%")),
    #           color = "white", size = 6) +
    scale_fill_brewer(palette = "Set2",
                      name = xlab)

  box <- ggplot(dataset, aes(x = as.factor({{variable}}),
                             fill = as.factor(Attrition_Flag))) +
    geom_bar(color = "#FFFFFF") +
    labs(x = xlab, y = "Count") +
    scale_fill_manual(values = c("royalblue", "#FF5733"),
                      name = "Attrition Flag:",
                      labels = c("Existing", "Attrited")) +
    theme(legend.position = "top",
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal",
          legend.title = element_text(size = 10),
          aspect.ratio = 1)

  (pie / box) + plot_layout(ncol = 2)

  # Save the plot in the plots/ folder
  ggsave(paste("../plots/", xlab, ".png", sep = ""), width = 10, height = 10)
}

# Plot variables
plot_continuous(bank, Customer_Age, "Customer Age", "Age")
plot_categorical(bank, Gender, "Gender")
plot_discrete(bank, Dependent_count, "Number of Dependents", "Dependents")
plot_categorical(bank, Education_Level, "Education Level")
plot_categorical(bank, Marital_Status, "Marital Status")
plot_categorical(bank, Income_Category, "Income Category")
plot_categorical(bank, Card_Category, "Card Category")
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

# Correlation matrix

# Compute the correlation matrix
corm <- bank_num |>
  corrr::correlate() |>
  corrr::shave(upper = FALSE)


# Pivot the matrix and fix the labels
corm <- corm |>
  tidyr::pivot_longer(
    cols = -term,
    names_to = "colname",
    values_to = "corr"
  ) |>
  dplyr::mutate(
    rowname = forcats::fct_inorder(term),
    colname = forcats::fct_inorder(colname),
    label = dplyr::if_else(is.na(corr), "", sprintf("%1.2f", corr))
  )

# Plot the correlation matrix
ggplot(corm, aes(rowname, fct_rev(colname),
                 fill = corr)) +
  geom_tile() +
  geom_text(aes(
    label = label,
    color = abs(corr) < .75
  )) +
  coord_fixed(expand = FALSE) +
  scale_color_manual(
    values = c("white", "black"),
    guide = "none"
  ) +
  scale_fill_distiller(
    palette = "RdYlBu", na.value = "white",
    direction = -1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:"
  ) +
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_rect(color = NA, fill = NA),
        legend.position = c(.85, .8),
        axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

# Save the plot in the plots/ folder
ggsave("../plots/correlation_matrix.png", width = 10, height = 10)

# Plots from correlation analysis

# Override the Total_Trans_Amt with its log
# bank$Total_Trans_Amt <- log(bank$Total_Trans_Amt)

# Plot of Total_Trans_Ct vs Total_Trans_Amt
ggplot(bank, aes(x = log(Total_Trans_Amt), y = Total_Trans_Ct, color = as.factor(Attrition_Flag))) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733"),
                     name = "Attrition Flag:",
                     labels = c("Existing", "Attrited")) +
  labs(x = "Total Transaction Amount", y = "Total Transaction Count") +
  theme(legend.position = c(.85, .15),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 10),
        aspect.ratio = 1)

# Save the plot in the plots/ folder
ggsave("../plots/total_trans_ct_vs_total_trans_amt.png", width = 10, height = 10)

# Density hexagonal map of counts of the previous plot
ggplot(bank, aes(x = Total_Trans_Amt, y = Total_Trans_Ct)) +
  geom_hex() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  labs(x = "Total Transaction Amount", y = "Total Transaction Count")

# Save the plot in the plots/ folder
ggsave("../plots/total_trans_ct_vs_total_trans_amt_hex.png", width = 10, height = 10)

# Plot of Avg_Open_To_Buy vs Credit_Limit
ggplot(bank, aes(x = Credit_Limit, y = Avg_Open_To_Buy, color = as.factor(Attrition_Flag))) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733"),
                     name = "Attrition Flag:",
                     labels = c("Existing", "Attrited")) +
  labs(x = "Credit Limit", y = "Average Open to Buy") +
  theme(legend.position = c(.85, .15),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 10),
        aspect.ratio = 1)

# Save the plot in the plots/ folder
ggsave("../plots/credit_limit_vs_open_to_buy.png", width = 10, height = 10)

# Boxplot of the response variable Attrition_Flag

ggplot(bank, aes(x = as.factor(Attrition_Flag))) +
  geom_bar(aes(fill = as.factor(Attrition_Flag)), color = "#FFFFFF") +
  scale_fill_manual(values = c("royalblue", "#FF5733"),
                    name = "Attrition Flag:",
                    labels = c("Existing", "Attrited")) +
  geom_text(aes(label = after_stat(count),
                y = after_stat(count)),
            stat = "count",
            vjust = -0.5,
            size = 10) +
  labs(x = "Attrition Flag", y = "Count") +
  ggtitle("Response variable: Attrition_Flag") +
  theme(legend.position = c(.85, .85),
        legend.background = element_rect(fill = "transparent"),
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        aspect.ratio = 1) +
  ylim(0, 10000)

# -----------------------------------------------------------------------------
df <- data.frame(
  Models = c("Dummy Classifier", "Logistic Regression", "GAM", "Decision Trees", "Boost", "Random Forest"),
  Accuracy = c(83.93, 91.36 , 96.19 , NA, 95.8 , 95.88 ),
  AUC = c(0.5, 93.44 , 98.79 , NA, 98.58 , 98.53 ),
  FPR = c(16.07, 3.15 , 1.84 , NA, 2.14 , 1.65 ),
  FNR = c(0, 46.1 , 14.62 , NA, 14.94 , 18.63 )
)

# Barplot of accuracy values for different models
df %>% filter(!Models %in% c("Decision Trees","Dummy Classifier")) %>%
  ggplot(aes(x = Models, y = Accuracy, fill = Models)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste(Accuracy, "%")),
            vjust = -0.5, size = 4) +
  labs(x = "Model", y = "Accuracy (%)") +
  ggtitle("Accuracy of different models") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 46, hjust = 1))

df %>% filter(!Models %in% c("Decision Trees","Dummy Classifier")) %>%
  ggplot(aes(x = Models, y = AUC, fill = Models)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste(AUC, "%")),
            vjust = -0.5, size = 4) +
  labs(x = "Model", y = "AUC (%)") +
  ggtitle("AUC of different models") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 46, hjust = 1))

df %>% filter(!Models %in% c("Decision Trees","Dummy Classifier")) %>%
  ggplot(aes(x = Models, y = FPR, fill = Models)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste(FPR, "%")),
            vjust = -0.5, size = 4) +
  labs(x = "Model", y = "FPR (%)") +
  ggtitle("FPR of different models") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 46, hjust = 1))

df %>% filter(!Models %in% c("Decision Trees","Dummy Classifier")) %>%
  ggplot(aes(x = Models, y = FNR, fill = Models)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste(FNR, "%")),
            vjust = -0.5, size = 4) +
  labs(x = "Model", y = "FNR (%)") +
  ggtitle("FNR of different models") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 46, hjust = 1))


# -----------------------------------------------------------------------------

df <- data.frame(
  Variable = c("Total_Trans_Amt", "Total_Trans_Ct", "Total_Revolving_Bal", "Total_Ct_Chng_Q4_Q1", 
               "Total_Relationship_Count", "Contacts_Count_12_mon", "Months_Inactive_12_mon", 
               "Gender", "Marital_Status", "Income_Category"),
  Mean_Gini_Decrease = c(594.260721, 547.113190, 417.708629, 381.125419, 229.326038, 
                          100.106859, 95.318035, 41.048829, 28.676287, 8.953285)
)

# Horizontal barplot of the mean Gini decrease
df %>%
  ggplot(aes(x = Mean_Gini_Decrease, y = fct_reorder(Variable, Mean_Gini_Decrease))) +
  geom_col(fill = "royalblue") +
  geom_text(aes(label = round(Mean_Gini_Decrease, 2)),
            hjust = -0.5, size = 4) +
  xlim(0,680) +
  labs(x = "Mean Gini Decrease", y = "Variable") +
  ggtitle("Mean Gini Decrease of different variables") +
  ggtitle("Variable Importance (Mean Gini Decrease)") +
  theme(legend.position = "none")
