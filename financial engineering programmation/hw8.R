#hw8
data <- read.csv("C:/Users/alexis/Desktop/Cornell_2016-2017/OR_tools_for_FE/aaplCalls.csv", sep=";")
head(data)
price = data$Ask
K = data$Strike
T = data$T/365  #  convert from days to years
r = 0.00340
s0 = 117.4

#1)
BlackScholes = function(S, T, t, K, sigma, r)
{
  d1 = (log(S/K) + (r + sigma^2/2) * (T - t)) / (sigma * sqrt(T - t))
  d2 = d1 - sigma * sqrt(T - t)
  C = pnorm(d1) * S - pnorm(d2) * K * exp(-r*(T - t))
  C
}
source("option_utilities.R") 
n = 126
Volatility = matrix(nrow=n,ncol=1) 
for (i in 1:n) 
{
  result = uniroot(function(sigma) BlackScholes(s0,T[i],0,K[i],sigma,r)- price[i], c(0.000001,50))
  Volatility[i]=result$root
}
sort = sort(unique(T)) 
par(mfrow=c(2,4))
for (i in 1:length(sort)) 
{
  ind = (T==sort[i])
  plot(K[ind],Volatility[ind],type="b",xlab="K",ylab="Volatility",
       main=paste("T=",eval(round(sort[i],4)),"years"))
}
library(mgcv)
fit = gam(Volatility ~ te(K,T)) 
vis.gam(fit,plot.type="contour", n.grid = 30,
        color="terrain",nlevels=25,
        main = "implied volatility surface",
        xlab = "strike price (K)",
        ylab = "time until maturity (T)")

#3)
r=.01
sigma=0.12
S0 = 80
K = 82
n = 500


Price = matrix(78000)
for (i in 1:78000)
{
  logS = log(S0) + (r-sigma^2/2) * (1:n)/n + sigma/sqrt(n) * cumsum(c(0,rnorm(n)))
  S = exp(logS)
  Saverage = sum(S)/(n+1)
  Price[i] = max(Saverage-K,0)/exp(r)
}
sd(Price)/sqrt(78000)

mean(Price)
