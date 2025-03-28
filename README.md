# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(tseries)

# Load the dataset
covid_data <- read.csv('/Users/moshoodlanre/Desktop/Data Analytic Project/New Covid data result/Covid Data.csv')

# View the first few rows of the dataset
head(covid_data)

# Check for missing values
sum(is.na(covid_data))

# Remove rows with missing values
covid_clean <- covid_data %>% drop_na()

# Check the structure of the cleaned data
str(covid_clean)

# Summary statistics of the cleaned data
summary(covid_clean)

# Exploratory Data Analysis (EDA)

# 1. Distribution of Age
ggplot(covid_clean, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Count")

# 2. Distribution of Sex
ggplot(covid_clean, aes(x = factor(SEX), fill = factor(SEX))) +
  geom_bar() +
  labs(title = "Distribution of Sex", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink"))

# 3. Distribution of Patient Type (Hospitalized vs. Not Hospitalized)
ggplot(covid_clean, aes(x = factor(PATIENT_TYPE), fill = factor(PATIENT_TYPE))) +
  geom_bar() +
  labs(title = "Distribution of Patient Type", x = "Patient Type", y = "Count") +
  scale_fill_manual(values = c("1" = "red", "2" = "green"))

# 4. Distribution of Pneumonia
ggplot(covid_clean, aes(x = factor(PNEUMONIA), fill = factor(PNEUMONIA))) +
  geom_bar() +
  labs(title = "Distribution of Pneumonia", x = "Pneumonia", y = "Count") +
  scale_fill_manual(values = c("1" = "orange", "2" = "purple"))

# 5. Distribution of Diabetes
ggplot(covid_clean, aes(x = factor(DIABETES), fill = factor(DIABETES))) +
  geom_bar() +
  labs(title = "Distribution of Diabetes", x = "Diabetes", y = "Count") +
  scale_fill_manual(values = c("1" = "yellow", "2" = "brown"))

# 6. Distribution of ICU Admission
ggplot(covid_clean, aes(x = factor(ICU), fill = factor(ICU))) +
  geom_bar() +
  labs(title = "Distribution of ICU Admission", x = "ICU", y = "Count") +
  scale_fill_manual(values = c("1" = "cyan", "2" = "magenta"))

# Time Series Forecasting with ARIMA

# Convert DATE_DIED to Date format
covid_clean$DATE_DIED <- as.Date(covid_clean$DATE_DIED, format = "%d/%m/%Y")

# Aggregate data by date and region (MEDICAL_UNIT) to get daily infection counts
daily_infections <- covid_clean %>%
  group_by(DATE_DIED, MEDICAL_UNIT) %>%
  summarise(infection_count = n(), .groups = 'drop')

# Plot time series for a specific region (e.g., MEDICAL_UNIT = 1)
region_data <- daily_infections %>% filter(MEDICAL_UNIT == 1)

# Convert to time series object
ts_data <- ts(region_data$infection_count, frequency = 7)  # Assuming weekly seasonality

# Plot the time series
autoplot(ts_data) +
  labs(title = "Daily Infection Count for Region 1", x = "Date", y = "Infection Count")

# Check for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(ts_data)
print(adf_test)

# If not stationary, difference the data
if (adf_test$p.value > 0.05) {
  ts_data <- diff(ts_data)
  print("Data was differenced to achieve stationarity.")
}

# Fit ARIMA model
arima_model <- auto.arima(ts_data, seasonal = TRUE)
summary(arima_model)

# Forecast future infection rates
forecast_result <- forecast(arima_model, h = 30)  # Forecast for the next 30 days

# Plot the forecast
autoplot(forecast_result) +
  labs(title = "30-Day Infection Rate Forecast for Region 1", x = "Date", y = "Infection Count")

# Check model residuals
checkresiduals(arima_model)
