library(readxl)
library(dplyr)
library(ggplot2)
library(minpack.lm)

install.packages("minpack.lm")
library(minpack.lm)


setwd("C:/Users/hp/Documents/Gompertz Makeham model")
getwd()

df1 <- read_excel("Mortality Female - individual.xlsx")
df2 <- read_excel("Mortality Male - individual.xlsx")
df3 <- read_excel("Mortality Female - Group lives.xlsx")
df4 <- read_excel("Mortality Male - Group lives.xlsx")
View(df1)
library(readxl)

# Set working directory
setwd("C:/Users/hp/Documents/Gompertz Makeham model")
getwd()

# Import datasets
female_individual <- read_excel("Mortality Female - individual.xlsx")
male_individual <- read_excel("Mortality Male - individual.xlsx")
female_group <- read_excel("Mortality Female - Group lives.xlsx")
male_group <- read_excel("Mortality Male - Group lives.xlsx")

# View the imported data
View(female_individual)
View(male_individual)
View(female_group)
View(male_group)

head(female_individual)
str(female_individual)

head(male_individual)
str(male_individual)

head(female_group)
str(female_group)

head(male_group)
str(male_group)

# Remove spaces from column names
colnames(female_individual) <- c("Age", "lx", "qx")
colnames(male_individual) <- c("Age", "lx", "qx")
colnames(female_group) <- c("Age", "lx", "qx")
colnames(male_group) <- c("Age", "lx", "qx")

# Check column names
names(female_individual)
names(male_individual)
names(female_group)
names(male_group)

install.packages("survival")
library(survival)


names(female_individual)
# Create the survival object
surv_obj <- with(female_individual, Surv(rep(`Age(x)`, `l x`), event = rep(1, sum(`l x`))))

# Fit Gompertz-Makeham model
fit_female_individual <- survreg(surv_obj ~ `Age(x)`, data = female_individual, dist = "gaussian")

# View summary of the fitted model
summary(fit_female_individual)

# Define a function to fit the Gompertz-Makeham model using nlsLM
# This one
fit_gompertz_makeham <- function(data) {
  # Define starting values for the parameters
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  
  # Fit the model using nlsLM
  model <- nlsLM(qx ~ A + B * exp(C * Age), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  
  # Return the fitted model
  return(model)
}

# Function to plot the fit
plot_fit <- function(data, model, title) {
  # Add the fitted values to the data
  data$fitted_values <- predict(model)
  
  # Create the plot
  ggplot(data, aes(x = Age)) +
    geom_point(aes(y = qx), color = "blue") +  # Plot observed values
    geom_line(aes(y = fitted_values), color = "red") +  # Plot fitted values
    labs(title = title, x = "Age", y = "Mortality Rate (qx)") +
    theme_minimal()
}

# Plot fits for each dataset
plot_fit(female_individual, model_female_individual, "Female Individual Mortality")
plot_fit(male_individual, model_male_individual, "Male Individual Mortality")
plot_fit(female_group, model_female_group, "Female Group Mortality")
plot_fit(male_group, model_male_group, "Male Group Mortality")

# Check model summaries
summary(model_female_individual)
summary(model_male_individual)
summary(model_female_group)
summary(model_male_group)
#################################################################
# Extract model coefficients
coef_female_individual <- coef(model_female_individual)
coef_male_individual <- coef(model_male_individual)
coef_female_group <- coef(model_female_group)
coef_male_group <- coef(model_male_group)

# Define the Gompertz-Makeham function
gompertz_makeham <- function(A, B, C, age) {
  A + B * exp(C * age)
}

# Forecast mortality rates
forecast_mortality <- function(model_coefs, ages) {
  A <- model_coefs["A"]
  B <- model_coefs["B"]
  C <- model_coefs["C"]
  sapply(ages, function(age) gompertz_makeham(A, B, C, age))
}

# Define the range of ages for forecasting
forecast_ages <- seq(60, 100, by = 1)

# Generate forecasts for each dataset
forecast_female_individual <- forecast_mortality(coef_female_individual, forecast_ages)
forecast_male_individual <- forecast_mortality(coef_male_individual, forecast_ages)
forecast_female_group <- forecast_mortality(coef_female_group, forecast_ages)
forecast_male_group <- forecast_mortality(coef_male_group, forecast_ages)

# Ensure lengths match before plotting
length(forecast_female_individual) == length(forecast_ages)  # Should be TRUE
length(forecast_male_individual) == length(forecast_ages)    # Should be TRUE
length(forecast_female_group) == length(forecast_ages)       # Should be TRUE
length(forecast_male_group) == length(forecast_ages)         # Should be TRUE

# Function to plot the fit and forecasts
plot_forecast <- function(data, model, forecast, ages, title) {
  data$fitted_values <- predict(model)
  forecast_data <- data.frame(Age = ages, Forecast = forecast)
  
  ggplot(data, aes(x = Age)) +
    geom_point(aes(y = qx), color = "blue") +  # Plot observed values
    geom_line(aes(y = fitted_values), color = "red") +  # Plot fitted values
    geom_line(data = forecast_data, aes(x = Age, y = Forecast), color = "green", linetype = "dashed") +  # Plot forecasted values
    labs(title = title, x = "Age", y = "Mortality Rate (qx)") +
    theme_minimal()
}

# Plot forecasts for each dataset
plot_forecast(female_individual, model_female_individual, forecast_female_individual, forecast_ages, "Female Individual Mortality Forecast")
plot_forecast(male_individual, model_male_individual, forecast_male_individual, forecast_ages, "Male Individual Mortality Forecast")
plot_forecast(female_group, model_female_group, forecast_female_group, forecast_ages, "Female Group Mortality Forecast")
plot_forecast(male_group, model_male_group, forecast_male_group, forecast_ages, "Male Group Mortality Forecast")

###############################################################
fit_gompertz_makeham <- function(data) {
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  model <- nlsLM(`q x` ~ A + B * exp(C * `Age(x)`), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  return(model)
}

forecast_mortality <- function(coefs, ages) {
  A <- coefs["A"]
  B <- coefs["B"]
  C <- coefs["C"]
  forecast <- A + B * exp(C * ages)
  return(forecast)
}

plot_forecast <- function(data, model, forecast, ages, title) {
  data$fitted_values <- predict(model)
  forecast_data <- data.frame(Age = ages, forecast_values = forecast)
  
  ggplot(data, aes(x = `Age(x)`)) +
    geom_point(aes(y = `q x`), color = "blue") +
    geom_line(aes(y = fitted_values), color = "red") +
    geom_line(data = forecast_data, aes(x = Age, y = forecast_values), color = "green", linetype = "dashed") +
    labs(title = title, x = "Age", y = "Mortality Rate (qx)") +
    theme_minimal()
}












#################################################################
# Fit models to each dataset
model_female_individual <- fit_gompertz_makeham(female_individual)
model_male_individual <- fit_gompertz_makeham(male_individual)
model_female_group <- fit_gompertz_makeham(female_group)
model_male_group <- fit_gompertz_makeham(male_group)


fit_gompertz_makeham <- function(data) {
  # Define starting values for the parameters
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  
  # Fit the model using nlsLM
  model <- nlsLM(`q x` ~ A + B * exp(C * `Age(x)`), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  
  # Return the fitted model
  return(model)
}
model_female_individual <- fit_gompertz_makeham(female_individual)
model_male_individual <- fit_gompertz_makeham(male_individual)
model_female_group <- fit_gompertz_makeham(female_group)
model_male_group <- fit_gompertz_makeham(male_group)

install.packages(c("readxl", "dplyr", "ggplot2", "minpack.lm"))
library(readxl)
library(dplyr)
library(ggplot2)
library(minpack.lm)
fit_gompertz_makeham <- function(data) {
  # Define starting values for the parameters
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  
  # Fit the model using nlsLM
  model <- nlsLM(`q x` ~ A + B * exp(C * `Age(x)`), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  
  # Return the fitted model
  return(model)
}
model_female_individual <- fit_gompertz_makeham(female_individual)
model_male_individual <- fit_gompertz_makeham(male_individual)
model_female_group <- fit_gompertz_makeham(female_group)
model_male_group <- fit_gompertz_makeham(male_group)

install.packages("minpack.lm")
library(minpack.lm)

fit_gompertz_makeham <- function(data) {
  # Define starting values for the parameters
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  
  # Fit the model using nlsLM
  model <- nlsLM(`q x` ~ A + B * exp(C * `Age(x)`), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  
  # Return the fitted model
  return(model)
}
model_female_individual <- fit_gompertz_makeham(female_individual)
model_male_individual <- fit_gompertz_makeham(male_individual)
model_female_group <- fit_gompertz_makeham(female_group)
model_male_group <- fit_gompertz_makeham(male_group)

# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(minpack.lm)

# Set working directory
setwd("C:/Users/hp/Documents/Gompertz Makeham model")
getwd()  # Confirm the working directory
list.files()  # Check if files are in the directory

# Import datasets
female_individual <- read_excel("Mortality Female - individual.xlsx")
male_individual <- read_excel("Mortality Male - individual.xlsx")
female_group <- read_excel("Mortality Female - Group lives.xlsx")
male_group <- read_excel("Mortality Male - Group lives.xlsx")

# Verify that the data has been loaded correctly
head(female_individual)
head(male_individual)
head(female_group)
head(male_group)

# rena
# Check the structure and column names of the data to get the correct column names
names(female_individual)
names(male_individual)
names(female_group)
names(male_group)

# Define a function to fit the Gompertz-Makeham model using nlsLM
fit_gompertz_makeham <- function(data) {
  # Define starting values for the parameters
  start_values <- list(A = 0.0001, B = 0.0001, C = 0.1)
  
  # Fit the model using nlsLM
  model <- nlsLM(`q x` ~ A + B * exp(C * `Age(x)`), data = data, start = start_values,
                 control = nls.lm.control(maxiter = 500))
  
  # Return the fitted model
  return(model)
}


# Fit models to each dataset
model_female_individual <- fit_gompertz_makeham(female_individual)
model_male_individual <- fit_gompertz_makeham(male_individual)
model_female_group <- fit_gompertz_makeham(female_group)
model_male_group <- fit_gompertz_makeham(male_group)

# Function to plot the fit
plot_fit <- function(data, model, title) {
  # Add the fitted values to the data
  data$fitted_values <- predict(model)
  
  # Create the plot
  ggplot(data, aes(x = `Age(x)`)) +
    geom_point(aes(y = `q x`), color = "blue") +  # Plot observed values
    geom_line(aes(y = fitted_values), color = "red") +  # Plot fitted values
    labs(title = title, x = "Age", y = "Mortality Rate (`q x`)") +
    theme_minimal()
}

# Plot fits for each dataset
plot_fit(female_individual, model_female_individual, "Female Individual Mortality")
plot_fit(male_individual, model_male_individual, "Male Individual Mortality")
plot_fit(female_group, model_female_group, "Female Group Mortality")
plot_fit(male_group, model_male_group, "Male Group Mortality")

# Check model summaries
summary(model_female_individual)
summary(model_male_individual)
summary(model_female_group)
summary(model_male_group)





