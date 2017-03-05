#HW3
R = read.csv("hw3returns.csv")
fitLM<- lm(R$MCD~R$SP500)
plot(R$SP500,R$MCD)


cor(R$MCD,R$SP500)

sort(R$MCD)[length(R$MCD)]
#we get the line corresponding to the biggest return
#for mcdonald
R[R$MCD==sort(R$MCD)[length(R$MCD)],][c(1,19,29)]
R[R$MCD==sort(R$MCD)[length(R$MCD)-1],][c(1,19,29)]
R[R$MCD==sort(R$MCD)[length(R$MCD)-2],][c(1,19,29)]
#we get the smallest line corresponding to the biggest return
#for mcdonald
R[R$MCD==sort(R$MCD)[1],][c(1,19,29)]
R[R$MCD==sort(R$MCD)[2],][c(1,19,29)] 
R[R$MCD==sort(R$MCD)[3],][c(1,19,29)]


summary(fitLM)
par(mfrow = c(1, 2))
qqnorm(R$MCD, datax = TRUE, main = "MCD returns", ylim = c(-0.1, 0.1))
qqline(R$MCD, datax = TRUE, col = "red", lwd = 2)
qqnorm(fitLM$residuals, datax = TRUE, main = "residuals", ylim = c(-0.1, 0.1))
qqline(fitLM$residuals, datax = TRUE, col = "red", lwd = 2)

#Q3
var(R$MCD)
var(fitLM$residuals)

fit_t_residual=fitdistr(fitLM$residuals,"t")
fit_t_residual

legend("topleft", c("nu = 8.2", "nu = 3.7"), lty = 1,
       col = c("black", "red"), lwd = 2)
t = seq(10, 25, length = 201)
plot(t, dstd(t, mean = -2.3*10^(-5), sd = 1.5, nu = 2.2), type = "l",
     ylab = "density(t)",

     xlab = "t", lwd = 2, ylim = c(0, 0.0005))
lines(t, dstd(t, mean = 1, sd = 1.5, nu = 5),
      col = "red", lwd = 2)
abline(h = 0)
legend("topleft", c("nu = 2.2", "nu = 5"), lty = 1,
col = c("black", "red"), lwd = 2)

#Q4
library(MASS)
fit_t = fitdistr(R$MCD, "t")
fit_t

#Q5
start=c(mean(R$MCD),sd(R$MCD),4)
library(fGarch)
logLik=function(theta)
{
  -sum(log(dstd(R$MCD,mean=theta[1],sd=theta[2],nu=theta[3])))
}
mle=nlminb(start,logLik)

thetahat=mle$par
library(numDeriv)
info=hessian(logLik,thetahat)
se=sqrt(diag(solve(info)))

######
x<-R$SP500
y<-R$MCD
start=c(1,0.0005,0.009,8)
LogLik<-function(theta){
  Res=x*theta[1]+theta[2]
  R=dstd(R$MCD,mean=Res,sd=theta[3],nu=theta[4])
  -sum(log(R))
}
mle=nlminb(start,LogLik)

thetahat=mle$par
library(numDeriv)
Info=hessian(LogLik,thetahat)
se=sqrt(diag(solve(Info)))

#CI at 95.4%
upperCL=thetahat+2*se
lowerCL=thetahat-2*se

fitdistr(fitLM$residuals,"t")

fit <- mle(LogLik, start=list(theta1=1,theta2=0.05,theta3=0.05,theta4=4))


############
N=100
x <- runif(N)
y <- 5 * x + 3 + rnorm(N)
LL <- function(beta0, beta1, mu, sigma) {
  # Find residuals
  #
  R = y - x * beta1 - beta0
  #
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(log(R))
}

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))

#Q6
library(forecast)
CPI = read.csv("CPIAUCSL.csv")
CPI = CPI[-(1:276),]
head(CPI)
logCPI = ts(log(CPI[,"CPIAUCSL"]),start=c(1970,1),
            frequency=12,names="inflation rate")
fit1<-auto.arima(logCPI,ic="bic",trace=T,seasonal=F)
fit2<-auto.arima(diff(logCPI),ic="bic",trace=T,seasonal=F)
fit3<-auto.arima(diff(diff(logCPI)),ic="bic",trace=T,seasonal=F)
#forecasts
predict1<-forecast(fit1,h=120)
predict2<-forecast(fit2,h=120)
predict3<-forecast(fit3,h=120,level = c(60,99.9))
par(mfrow=c(3,1))
plot(predict1)
plot(predict2)
plot(predict3)
