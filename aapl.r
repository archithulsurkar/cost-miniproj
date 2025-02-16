# Load libraries
library(ggplot2)
library(car)
# Read dataset
aapl_data <- read.csv("AAPL.csv", stringsAsFactors = FALSE)
aapl_data$Date <- as.Date(aapl_data$Date, format = "%Y-%m-%d")

# Exploratory Data Analysis
pairs(~Close + Open + High + Low + Volume, data=aapl_data)
correlation_matrix <- cor(aapl_data[, c("Close", "Open", "High", "Low", "Volume")])
print(correlation_matrix)

# Linear Regression Model
model <- lm(Close ~ Open + High + Low + Volume, data=aapl_data)
summary(model)

# Visualization
plot(aapl_data$Close, predict(model), main="Actual vs Predicted Close Price", xlab="Actual Close Price", ylab="Predicted Close Price")
abline(0, 1, col="red")

plot(model$residuals, main="Residual Plot", ylab="Residuals", xlab="Index")
abline(h=0, col="blue")


summary(aapl_data)


hist(aapl_data$Close, main="Distribution of Close Prices", xlab="Close Price", col="lightblue")
plot(density(aapl_data$Close), main="Density Plot of Close Prices")




vif(model)

qqnorm(model$residuals)
qqline(model$residuals, col="red")

plot(model$fitted.values, model$residuals, main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")


library(Metrics)
predicted <- predict(model)
actual <- aapl_data$Close

rmse(actual, predicted)
mae(actual, predicted)

library(corrplot)
corrplot(correlation_matrix, method="color", type="lower", tl.col="black")


