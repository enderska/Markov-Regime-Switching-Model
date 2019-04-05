# Linear Time Series Modelling

# Modelling Ifo Business Climate Index with Markov regime-switching models
# Evaluating Forecasting Performance of MS Models with linear Models (AR, MA, ARMA)
# Package 'MSwM' Univariate Autoregressive Markov Switching Models for Linear and Generalized Models by using the EM algorithm.

#install.packages("MSwM")
#install.packages("tsDyn")
#install.packages("mFilter")
#install.packages("lmtest")
library(lmtest)
library(tseries)
library(forecast)
library(MSwM)

# set working directory
setwd ("/Users/katharinaenders/Documents/Uni/Seminare/Financial Econometrics")
ifo <- read.csv2(file="ifo_index.csv",header=T)
ifo <- ts(as.numeric(ifo[,2]), start=c(1991,1), frequency = 12)

#rm(list=ls())
#par(mfrow=c(1,1))

# Stationartiy Check
adf.test(ifo, alternative="stationary")
# p-value < .05 hence data is stationary

par(mfrow=c(3,1))
acf(ifo, main="ACF")
pacf(ifo, main="PACF")



#------------------------ ARIMA Building & Forecasting --------------------#

#AR(4) fit

ifo.model <- Arima(window(ifo, end=c(2014,10)),order=c(4,0,0))
tsdiag(ifo.model) 
coeftest(ifo.model) 
ifo.model 
tsdisplay(residuals(ifo.model))
Box.test(resid(ifo.model),lag=6, type='Ljung') 

#plot forecast
plot(forecast(ifo.model, h=30))
lines(ifo)

#apply fitted model to later data
ifo.model2 <- Arima(window(ifo, start=c(2014,11)), model=ifo.model)

#insample forecast
accuracy(ifo.model)

#outsample forcast 
accuracy(ifo.model2)


#---------------------------ARMA(2,1)----------------------------#

ifo.model3 <- Arima(window(ifo, end=c(2014,10)),order=c(2,0,1))
tsdiag(ifo.model3) 
coeftest(ifo.model3) 
ifo.model3 
tsdisplay(residuals(ifo.model3))
Box.test(resid(ifo.model3),lag=6, type='Ljung') 

#plot forecast
plot(forecast(ifo.model3, h=30))
lines(ifo)

#apply fitted model to later data
ifo.model4 <- Arima(window(ifo, start=c(2014,11)), model=ifo.model3)

#insample forecast
accuracy(ifo.model3)

#outsample forcast 
accuracy(ifo.model4)

#---------------------------ARMA(2,2)----------------------------#

ifo.model5 <- Arima(window(ifo, end=c(2014,10)),order=c(2,0,2))
tsdiag(ifo.model5) 
coeftest(ifo.model5) 
ifo.model5 
tsdisplay(residuals(ifo.model5))
Box.test(resid(ifo.model5),lag=6, type='Ljung') 

#plot forecast
plot(forecast(ifo.model5, h=30))
lines(ifo)

#apply fitted model to later data
ifo.model6 <- Arima(window(ifo, start=c(2014,11)), model=ifo.model5)

#insample forecast
accuracy(ifo.model5)

#outsample forcast 
accuracy(ifo.model6)