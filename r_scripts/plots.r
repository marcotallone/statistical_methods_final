# LIBRARIES --------------------------------------------------------------------

library(ggplot2)
library(ggExtra)
library(ggforce)
library(gridExtra)
library(patchwork)
library(RColorBrewer)

display.brewer.all()

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

# PLOTS ------------------------------------------------------------------------

# Re-do the plot above but coloring each bar by the Attrition_Flag variable
p1 <- ggplot(bank, aes(x = Customer_Age, fill = as.factor(Attrition_Flag))) +
  geom_histogram(binwidth = 1, color = "#FFFFFF") +
  labs(title = "Histogram of Customer Age", x = "Age", y = "Count") +
  scale_fill_manual(values = c("royalblue", "#FF5733"),
                    name = "Attrition Flag:",
                    labels = c("Existing", "Attrited")) +
  theme(legend.position = c(.85, .85),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 10),
        aspect.ratio = 1)

# Violin/jitter plot
p2 <- ggplot(bank, aes(x = as.factor(Attrition_Flag), y = Customer_Age,
                       color = as.factor(Attrition_Flag))) +
  labs(x = "Attrition Flag", y = "Age") +
  scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
  geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
  geom_sina(aes(group = Attrition_Flag), alpha = .25) +
  coord_flip() +
  theme(legend.position = "none", aspect.ratio = 1)

# Boxplot
p3 <- ggplot(bank, aes(x = as.factor(Attrition_Flag), y = Customer_Age,
                       color = as.factor(Attrition_Flag))) +
  labs(x = "Attrition Flag", y = "Age") +
  scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
  geom_boxplot(fill = "gray80", alpha = .5,
               outlier.size = 4, outlier.alpha = .75) +
  coord_flip() +
  theme(legend.position = "none", aspect.ratio = 1)

# Cumulative distribution plot without separating by Attrition_Flag
p4 <- ggplot(bank, aes(x = Customer_Age)) +
  stat_ecdf(geom = "step", color = "royalblue") +
  labs(title = "Cumulative Distribution of Customer Age",
       x = "Customer Age", y = "Cumulative Distribution") +
  theme(legend.position = "none", aspect.ratio = 1)

# Arrange the plots in a 2x2 grid
(p1 / p4 / p2 / p3) + plot_layout(ncol = 2)

# Function to convenitntly reproduce the previous plot with other variables

plot <- function(dataset, variable, title, xlab, ylab) {
  p1 <- ggplot(dataset, aes(x = {{ variable }},
                            fill = as.factor(Attrition_Flag))) +
    geom_histogram(binwidth = 1, color = "#FFFFFF") +
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
    labs(x = "Attrition Flag", y = ylab) +
    scale_color_manual(values = c("0" = "royalblue", "1" = "#FF5733")) +
    geom_violin(fill = "gray80", linewidth = 1, alpha = .5) +
    geom_sina(aes(group = Attrition_Flag), alpha = .25) +
    coord_flip() +
    theme(legend.position = "none", aspect.ratio = 1)

  p3 <- ggplot(dataset, aes(x = as.factor(Attrition_Flag), y = {{ variable }},
                            color = as.factor(Attrition_Flag))) +
    labs(x = "Attrition Flag", y = ylab) +
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

# Test the function with age again
plot(bank, Customer_Age, "Customer Age", "Age", "Age")
