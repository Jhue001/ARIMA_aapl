
# Forecasting APPLE stock price using ARIMA
# 5 years of weekly data
# 252 rows of training data, 10 rows of testing data

###########################################################################

#Load libraries

library(MASS)
library(forecast)
library(tseries)

#load data
myData <- read.csv("AAPL.csv")

#log prices and subset training
lntrain <- log(myData$Close[1:(nrow(myData)-10)])

#ACF PACF DICKEY-FULLER
acf(lntrain)
pacf(lntrain)
adf.test(lntrain)
#reject null hypothesis - non-stationary

#difference
difflntrain <- diff(lntrain)
acf(difflntrain)
pacf(difflntrain)
adf.test(difflntrain)
#stationary, likely to be ARIMA(0,1,0)

#autoarima
myts <- ts(lntrain, start = c(2016,20), frequency = 52)
fit <- auto.arima(myts)

#plot
plot(myts, type='l')
title('AAPL price')

#forecast
predictedln <- forecast(fit, h = 10)
plot(predictedln)
finalforecast <- exp(predictedln$mean)

#compare 
lntest <- ts(log(myData$Close[253:262]), start = c(2021,11) , frequency = 52)
lines(lntest)

#MSE
df <- data.frame(myData$Close[253:262], finalforecast)
names(df) <- c("Actual", "Forecasted")
percentage_error = ((df$Actual-df$Forecasted)/df$Actual)
mean( percentage_error)


#Ljung-box _ serial correlation test
Box.test(fit$residuals, lag=5, type = "Ljung-Box")