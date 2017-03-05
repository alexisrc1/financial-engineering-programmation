####hw4
R = read.csv("GDP.csv")

date=as.Date(R$DATE,"%m/%d/%Y")
#time series !!
logGDP<-ts(log(R[,2]),start=c(1947,1),frequency=4,names="time series")
###

plot(logGDP,type="l")
plot(diff(logGDP),type="l",main="time series differentiated")
#looks stationary !
plot(diff(diff(logGDP)),type="l")
#ACF
acf(diff(logGDP), main = "ACF")
acf(logGDP, main = "ACF, very slow decrease, we will need to derivate...")

#Q2
Box.test(diff(logGDP)) #small p-values, reject weak white noise hypothesis
Box.test(diff(logGDP),lag=12)
#Q3

library(forecast)
fit1=auto.arima(logGDP,ic="bic",trace=T,seasonal = F)
fit2=auto.arima(logGDP,ic="aicc",trace=T,seasonal = F)
Box.test(fit1$residual,lag=12,fitdf=2) #no random walk

acf(fit1$residual, main = "acf residuals, ARIMA(1,2,1)")
plot(fit2$residual)
#Q4
