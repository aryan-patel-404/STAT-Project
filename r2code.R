if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)

# Load the dataset (replace with the correct file path)
loan_data <- read.csv("/Users/aryanpatelkolagani/Downloads/loan_dataset.csv", sep = ";", stringsAsFactors = FALSE)

# Parse and clean dataset
if (ncol(loan_data) == 1) {
  loan_data <- read.csv("/Users/aryanpatelkolagani/Downloads/loan_dataset.csv", sep = ",", stringsAsFactors = FALSE)
}

# Handle missing values
loan_data$LoanAmount[is.na(loan_data$LoanAmount)] <- median(loan_data$LoanAmount, na.rm = TRUE)
loan_data$ApplicantIncome <- as.numeric(loan_data$ApplicantIncome)
loan_data$CoapplicantIncome <- as.numeric(loan_data$CoapplicantIncome)

# Create TotalIncome column
loan_data <- loan_data %>%
  mutate(TotalIncome = ApplicantIncome + CoapplicantIncome)

# Visualize correlations
# Select numeric columns
numeric_data <- loan_data %>%
  select(ApplicantIncome, CoapplicantIncome, LoanAmount, TotalIncome)

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Correlation heatmap
corrplot(cor_matrix, method = "color", type = "full",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))

# Pair plot for relationships
ggpairs(numeric_data, title = "Pair Plot of Numeric Variables")

# Correlation analysis
cor_applicant <- cor(loan_data$ApplicantIncome, loan_data$LoanAmount, use = "complete.obs")
cor_coapplicant <- cor(loan_data$CoapplicantIncome, loan_data$LoanAmount, use = "complete.obs")
cor_total <- cor(loan_data$TotalIncome, loan_data$LoanAmount, use = "complete.obs")
print(paste("Correlation between Applicant Income and Loan Amount:", cor_applicant))
print(paste("Correlation between Coapplicant Income and Loan Amount:", cor_coapplicant))
print(paste("Correlation between Total Income and Loan Amount:", cor_total))

# Linear regression: Loan Amount predicted by Total Income
model_income <- lm(LoanAmount ~ TotalIncome, data = loan_data)
summary(model_income)

# Extract residuals and add them to a filtered dataset
loan_data_filtered <- loan_data[complete.cases(loan_data$TotalIncome, loan_data$LoanAmount), ]
loan_data_filtered$residuals <- residuals(model_income)

# Residual plot
ggplot(data = loan_data_filtered, aes(x = TotalIncome, y = residuals)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Total Income vs Loan Amount",
       x = "Total Income", y = "Residuals") +
  theme_minimal()

# Diagnostic plots for the linear regression model
par(mfrow = c(2, 2))
plot(model_income)
