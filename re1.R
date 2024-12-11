/Users/aryanpatelkolagani/Downloads/loan_dataset.csv

library(ggplot2)
library(dplyr)
library(tidyverse)

# Load the dataset (replace with actual file path)
loan_data <- read.csv("/Users/aryanpatelkolagani/Downloads/loan_dataset.csv", sep = ";", stringsAsFactors = FALSE)

# Parse columns correctly if needed
if (ncol(loan_data) == 1) {
  loan_data <- read.csv("path_to_dataset.csv", sep = ",", stringsAsFactors = FALSE)
}

# Handle missing values
loan_data <- loan_data %>% 
  mutate(
    Loan_Status = as.factor(Loan_Status),
    Credit_History = as.factor(Credit_History),
    LoanAmount = as.numeric(LoanAmount),
    Loan_Amount_Term = as.numeric(Loan_Amount_Term),
    ApplicantIncome = as.numeric(ApplicantIncome),
    CoapplicantIncome = as.numeric(CoapplicantIncome)
  ) %>%
  filter(!is.na(Loan_Status))

# Logistic Regression Model
model <- glm(Loan_Status ~ ApplicantIncome + CoapplicantIncome + LoanAmount +
               Loan_Amount_Term + Credit_History + Property_Area + Education,
             data = loan_data, family = "binomial")

# Display the summary of the model
summary(model)

# Visualize relationships
# 1. Credit History vs Loan Status
ggplot(loan_data, aes(x = Credit_History, fill = Loan_Status)) +
  geom_bar(position = "fill") +
  labs(title = "Loan Status by Credit History", x = "Credit History", y = "Proportion") +
  theme_minimal()

 # 2. Property Area vs Loan Status
ggplot(loan_data, aes(x = Property_Area, fill = Loan_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Loan Status by Property Area", x = "Property Area", y = "Count") +
  theme_minimal()
####################################
library(ggplot2)

# Scatter plot: Applicant Income vs Loan Amount
ggplot(loan_data, aes(x = ApplicantIncome, y = LoanAmount)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Applicant Income vs Loan Amount", 
       x = "Applicant Income", 
       y = "Loan Amount") +
  theme_minimal()

# Scatter plot: Coapplicant Income vs Loan Amount
ggplot(loan_data, aes(x = CoapplicantIncome, y = LoanAmount)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "Coapplicant Income vs Loan Amount", 
       x = "Coapplicant Income", 
       y = "Loan Amount") +
  theme_minimal()

# Combine ApplicantIncome and CoapplicantIncome for regression
loan_data <- loan_data %>%
  mutate(TotalIncome = ApplicantIncome + CoapplicantIncome)

# Scatter plot: Total Income vs Loan Amount
ggplot(loan_data, aes(x = TotalIncome, y = LoanAmount)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(title = "Total Income vs Loan Amount", 
       x = "Total Income", 
       y = "Loan Amount") +
  theme_minimal()

# Correlation analysis: ApplicantIncome and LoanAmount
correlation_applicant <- cor(loan_data$ApplicantIncome, loan_data$LoanAmount, use = "complete.obs")
print(paste("Correlation between Applicant Income and Loan Amount:", correlation_applicant))

# Correlation analysis: CoapplicantIncome and LoanAmount
correlation_coapplicant <- cor(loan_data$CoapplicantIncome, loan_data$LoanAmount, use = "complete.obs")
print(paste("Correlation between Coapplicant Income and Loan Amount:", correlation_coapplicant))

# Correlation analysis: TotalIncome and LoanAmount
correlation_total <- cor(loan_data$TotalIncome, loan_data$LoanAmount, use = "complete.obs")
print(paste("Correlation between Total Income and Loan Amount:", correlation_total))

# Linear regression: Loan Amount predicted by Total Income
model_income <- lm(LoanAmount ~ ApplicantIncome + CoapplicantIncome, data = loan_data)
summary(model_income)


