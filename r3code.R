# Load necessary libraries
library(caret)
library(dplyr)
library(glmnet)
library(randomForest)
library(DMwR2)  # For SMOTE
library(ggplot2)


# Load dataset
loan_data <- read.csv("/Users/aryanpatelkolagani/Downloads/loan_dataset.csv", sep = ";")

# Data Preparation
loan_data <- loan_data %>%
  mutate(
    Loan_Status = ifelse(Loan_Status == "Y", 1, 0), # Convert Loan_Status to binary
    Loan_Status = as.factor(Loan_Status),           # Convert to factor for classification
    Education = as.factor(Education),
    ApplicantIncome = as.numeric(ApplicantIncome)
  )

# Handle Missing Values
loan_data <- loan_data %>%
  filter(!is.na(ApplicantIncome) & !is.na(LoanAmount))

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(loan_data$Loan_Status, p = 0.7, list = FALSE)
train_data <- loan_data[train_index, ]
test_data <- loan_data[-train_index, ]

# Handle Class Imbalance using SMOTE
train_data <- SMOTE(Loan_Status ~ ., data = train_data, perc.over = 200, perc.under = 150)

# Prepare Features and Target Variables
x_train <- model.matrix(Loan_Status ~ ApplicantIncome + Education, data = train_data)[, -1]
x_test <- model.matrix(Loan_Status ~ ApplicantIncome + Education, data = test_data)[, -1]
y_train <- as.numeric(as.character(train_data$Loan_Status))
y_test <- as.numeric(as.character(test_data$Loan_Status))

# Verify dimensions
cat("Rows in x_train:", nrow(x_train), "\n")
cat("Rows in x_test:", nrow(x_test), "\n")

# Lasso Regression
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")
lasso_prob <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")
lasso_predictions <- ifelse(lasso_prob > 0.5, 1, 0)
lasso_accuracy <- mean(lasso_predictions == y_test)
cat("Lasso Accuracy:", lasso_accuracy, "\n")

# Visualization for Applicant Income, Education vs Loan Status
ggplot(loan_data, aes(x = Education, y = ApplicantIncome, fill = Loan_Status)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(
    title = "Loan Status vs Applicant Income and Education",
    x = "Education",
    y = "Average Applicant Income",
    fill = "Loan Status"
  ) +
  theme_minimal()
