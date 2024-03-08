install.packages("stats")
install.packages("Rcpp")
install.packages("ggplot2")
install.packages("forecast")

library("Rcpp")
library("ggplot2")
library("stats")
library("forecast")

path <- file.path("C:", "Users", "User", "Downloads", fsep="\\")
setwd(path)
dataset.raw <- read.csv("DataN2O.csv")
dataset.ts <- ts(data = dataset.raw[,2], start = c(2001, 1), frequency=12)

'#Question 1 Time Series Plot'
#Plot of Data against Observation Times
plot(dataset.ts, main="Time Series of N2O Concentration", ylab="parts per billion (ppb)", xlab="Observation Time")

#Plot of 1st Difference of Data against Observation Time 
dataset.firstdiff=diff(dataset.ts)
plot(dataset.firstdiff, main="Time Series of 1st Differences of N2O Concentration",ylab="parts per billion (ppb)", xlab="Observation Time")

'#Question 2 ACF and PACF Plot'
acf_data = acf(ts(dataset.ts), lag.max = 20, plot = TRUE, main="ACF of N2O Concentration")
acf_firstdiff = acf(ts(dataset.firstdiff), lag.max = 20, plot = TRUE,main="ACF of First Differences of N2O Concentration")

pacf_data = pacf(ts(dataset.ts), lag.max = 20, plot = TRUE, main="PACF of N2O Concentration")
pacf_firstdiff = pacf(ts(dataset.firstdiff), lag.max = 20, plot = TRUE,main="PACF of First Differences of N2O Concentration")

'#Question 4 Fit ARIMA Model'
arima_model=Arima(dataset.ts,c(4,1,2),include.constant = TRUE)
summary(arima_model)

'#Question 5 Forecast'
arima_forecast=forecast(arima_model,h=36,level=c(95))
plot(arima_forecast,main="Plot of Forecast of N20 Concentration for the Next 3 Years",ylab="parts per billion (ppb)")

