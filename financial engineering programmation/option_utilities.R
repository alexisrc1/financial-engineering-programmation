#Question 1
prices = function(n, m, sigma, s0)
{
  logprice = log(s0) + sigma*(2*(0:m)-m)/sqrt(n)
  exp(logprice)
}
BlackScholes = function(S, T, t, K, sigma, r)
{
  d1 = (log(S/K) + (r + sigma^2/2) * (T - t)) / (sigma * sqrt(T - t))
  d2 = d1 - sigma * sqrt(T - t)
  C = pnorm(d1) * S - pnorm(d2) * K * exp(-r*(T - t))
  C
}

values<-c(50,100,500,1000,1500,2000)
for (k in values){
n = k
s0 = 100
sigma = 0.14
r = 0.03
K = 101
Sd=s0*exp(-sigma/sqrt(n))
Su=s0*exp(sigma/sqrt(n))
q = (s0*exp(0.03/n)-Sd)/(Su-Sd)
#BlackScholes(s0,T,0,K,sigma,r)
#source("option_utilities.R")
finalOptionPrices = pmax(prices(n, n, sigma, s0) - K, 0)
value = finalOptionPrices
valueMat = matrix(nrow = n + 1, ncol = n + 1)
valueMat[n + 1, 1:(n + 1)] = value
for (i in n:1)  # (i - 1) is the step number, so we go from step (n - 1) to step 0
{
  value = value[1:i] * (1-q) + value[2:(i + 1)] * q
value = value * exp(-r/n)
valueMat[i,1:length(value)] = value
}
options(digits = 5)
valueMat  #  prices of the call option on the binomial tree
print("Black&Scholes")
print(BlackScholes(s0, 1, 0, K, sigma, r))#  price from B-S
print(c(paste("for n =",k)))
print(sum(finalOptionPrices*dbinom(0:n, n, q)) * exp(-r)) # method (e)
}
#Question 2
prices = function(n, m, sigma, s0)
{
  logprice = log(s0) + sigma*(2*(0:m)-m)/sqrt(n)
  exp(logprice)
}
BlackScholes = function(S, T, t, K, sigma, r)
{
  d1 = (log(S/K) + (r + sigma^2/2) * (T - t)) / (sigma * sqrt(T - t))
  d2 = d1 - sigma * sqrt(T - t)
  C = pnorm(d1) * S - pnorm(d2) * K * exp(-r*(T - t))
  C
}

#values<-c(50,100,500,1000,1500,2000)
#for (i in values){
n = 2500
s0 = 100
sigma = 0.14
r = 0.03
K = 101
Sd=s0*exp(-sigma/sqrt(n))
Su=s0*exp(sigma/sqrt(n))
q = (s0*exp(0.03/n)-Sd)/(Su-Sd)

source("option_utilities.R")
finalOptionPrices = pmax(-(prices(n, n, sigma, s0) - K), 0)
value = finalOptionPrices
valueMat = matrix(nrow = n + 1, ncol = n + 1)
valueMat[n + 1, 1:(n + 1)] = value
for (i in n:1)  # (i - 1) is the step number, so we go from step (n - 1) to step 0
{
  value = value[1:i] * (1-q) + value[2:(i + 1)] * q
  value = value * exp(-r/n)
  valueMat[i,1:length(value)] = value
}
options(digits = 5)
valueMat[1,1]  #  prices of the call option on the binomial tree
BlackScholes(s0, 1, 0, K, sigma, r)+K*exp(-r)-s0  #  price from B-S
sum(finalOptionPrices*dbinom(0:n, n, q)) * exp(-r) # method (e)

#question 3
prices = function(n, m, sigma, s0)
{
  logprice = log(s0) + sigma*(2*(0:m)-m)/sqrt(n)
  exp(logprice)
}
BlackScholes = function(S, T, t, K, sigma, r)
{
  d1 = (log(S/K) + (r + sigma^2/2) * (T - t)) / (sigma * sqrt(T - t))
  d2 = d1 - sigma * sqrt(T - t)
  C = pnorm(d1) * S - pnorm(d2) * K * exp(-r*(T - t))
  C
}

#values<-c(50,100,500,1000,1500,2000)
#for (i in values){
n = 1000
s0 = 100
sigma = 0.14
r = 0.03
K = 101
Sd=s0*exp(-sigma/sqrt(n))
Su=s0*exp(sigma/sqrt(n))
q = (s0*exp(0.03/n)-Sd)/(Su-Sd)

source("option_utilities.R")
finalOptionPrices = pmax(-(prices(n, n, sigma, s0) - K), 0)
value = finalOptionPrices
valueMat = matrix(nrow = n + 1, ncol = n + 1)
valueMat[n + 1, 1:(n + 1)] = value
for (i in n:1)  # (i - 1) is the step number, so we go from step (n - 1) to step 0
{
  value = value[1:i] * (1-q) + value[2:(i + 1)] * q
  value = max(value * exp(-r/n),K-prices(n,n,sigma,s0)[i])
  valueMat[i,1:length(value)] = value
}
options(digits = 5)
valueMat[1,1]  #  prices of the call option on the binomial tree
BlackScholes(s0, 1, 0, K, sigma, r)+K*exp(-r)-s0  #  price from B-S
sum(finalOptionPrices*dbinom(0:n, n, q)) * exp(-r) # method (e)



