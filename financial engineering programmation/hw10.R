## HW 10
yield=function(par,price,t){
  (par/price)^(1/(2*t))-1
}
listprices=c(987.1,959.2,923.3,898.2)
spotrates=yield(1000,listprices,c(0.5,1,1.5,2))
spotrates

data =read.delim("D:/Cornell_2016-2017/OR_tools_for_FE/hw10data.txt")
date = as.Date(data$Date,"%m/%d/%y")
head(data)
tail(data)
par(mfrow=c(3, 4))
for (i in 2:12){
plot(date, data[,i], type = "l",
     ylab = "yield",main = names(data)[i])}

###

data2 = data[,-c(2,12)]
date2 = as.Date(data2$Date,"%m/%d/%y")
par(mfrow = c(3, 3))
for (i in 2:10){
plot(date2 ,data2[,i], type = "l",
     ylab = "", main = names(data2)[i], xlab = "year")
}
graphics.off

T = c(0.25, 0.5, 1,  2,  3,  5,  7, 10, 20)
K = 5
data2[,1]=date2
n = length(date2)
Selectdates = floor(c(1, (1:K) * n / K))
par(mfrow = c(1, 1))
plot(T, data2[Selectdates[1],-1], type = "b", lty = 1, lwd = 2,
     ylab = "Yield", xlab = "T", col = 1,ylim=c(0,15))
for (i in 2:(K + 1))
{
lines(T, data2[Selectdates[i],-1], type = "b",
      lty = i, lwd = 2, col = i)
}
legend("topright", as.character(date2[Selectdates]) ,
       lty = 1:K, lwd = 2, cex = 1, col = 1:(K + 1))

#########
# 4)
dat <- read.csv("D:/Cornell_2016-2017/OR_tools_for_FE/ZeroPricesHW10.csv")

T = dat$t
price = dat$prices
NelsonSiegel = function(theta)
{
  yield = theta[1] + (theta[2] + (theta[3]/theta[4])) *
    (1 - exp( -theta[4] * T)) / (theta[4] * T) -
    (theta[3] / theta[4]) * exp( -theta[4] * T)
}
error = matrix(nrow = 1000,ncol = 1)
thetas = matrix(nrow = 1000,ncol = 4)
for (i in 1:1000)
{
  start= c(runif(1,0.02,0.08),runif(1,0.02,0.08),
           runif(1,0.02,0.08),runif(1,-0.03,0.1))
  fit_NS = optim(start, fn = function(theta){
    diff=price - 1000 * exp (- T * NelsonSiegel(theta)) 
    sum(diff^2)})
  thetas[i,]=fit_NS$par
  error[i]=sum((price - 1000 * exp (- T * NelsonSiegel(fit_NS$par)))^2)
}
smallestIndice=error==min(error)
smallesttheta=thetas[smallestIndice,]

NSforward = function(theta)
{
  forward = theta[1] + (theta[2]+theta[3]*T)*exp(-theta[4]*T)
}
plot(T,NelsonSiegel(smallesttheta),type = "l",lwd = 2,ylab = "rate",ylim = c(0.04,0.1))
lines(T,NSforward(smallesttheta),type="l",lwd=2,col="red")
actualYield = log(1000/price) / T
points(T,actualYield)



