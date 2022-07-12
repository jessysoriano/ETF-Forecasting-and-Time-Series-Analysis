## install.packages('quantmod')
### install.packages('tseries') 
#### install.packages('timeSeries')
#### install.packages('forecast')
install.packages('xts')

library(xts)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)

#Pull data from Yahoo Finance ICLN ETF
x = getSymbols('ICLN', from='2017-10-04', to='2022-10-04')
class(ICLN)
sum(is.na(x))


ICLN_Close_Prices = ICLN[,4]

plot(ICLN_Close_Prices)
#### class(ICLN_Close_Prices)


##### "Outlier Treatment" [Insert Here]


par(mfrow=c(1,1))

#### Residual tests 
Acf(ICLN_Close_Prices, main='ACF for Differenced Series')
Pacf(ICLN_Close_Prices, main='PACF for Differenced Series')

print(adf.test((ICLN_Close_Prices)))

auto.arima(ICLN_Close_Prices, seasonal = FALSE)

#### fitA is Auto-Arima
fitA = auto.arima(ICLN_Close_Prices, seasonal= FALSE)
tsdisplay(residuals(fitA), lag.max = 40, main='(2,1,2) Model Residuals')

#### First lag order of 10
fitB = arima(ICLN_Close_Prices, order = c(1,1,10))
tsdisplay(residuals(fitB), lag.max = 40, main='(1,1,10) Model Residuals')

#### d = 2 and use ACF and PACF to deduce lag level 
fitC = arima(ICLN_Close_Prices, order = c(1,2,28))
tsdisplay(residuals(fitC), lag.max = 40, main='(1,2,28) Model Residuals')

fitD = arima(ICLN_Close_Prices, order = c(1,1,38))
tsdisplay(residuals(fitD), lag.max = 40, main='(1,1,38) Model Residuals')



###### Residual Treatment [Insert Here]


par(mfrow=c(2,2))

#### 100 day term for forecast 
term<-250
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)

fcast3 <- forecast(fitC, h=term)
plot(fcast3)

fcast4 <- forecast(fitD, h=term)
plot(fcast4)

### 100 - MAPE is accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)



###### "Model Improvement + Model Building" [Insert Here]





