# Set working directory as this directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the dataset from the datasets/ folder
bank <- read.csv("../datasets/BankChurners.csv", sep = ",")

# ⚠️ Remove the last two columns as suggested in the README
bank <- bank[, -c(22, 23)]

# View the dataset
View(bank)

# Summary
summary(bank)

# Structure
str(bank)

# Number of rows
nrow(bank)

# Total number of NA
sum(is.na(bank))
