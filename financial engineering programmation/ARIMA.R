library(Ecdat)
library(forecast)
data("Macrodat")
fedFunds_rate=Macrodat[,3]
plot(fedFunds_rate)
plot(diff(fedFunds_rate))
plot(diff(log(fedFunds_rate)))
acf(as.vector(diff(log(fedFunds_rate))),main="as ts")
fit1 = auto.arima(log(fedFunds_rate), ic="bic",
trace = TRUE, seasonal=FALSE)
fit2 = auto.arima(log(fedFunds_rate), ic="aicc",
                  trace = TRUE, seasonal=FALSE)
summary(fit2)