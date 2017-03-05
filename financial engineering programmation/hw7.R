########## HW7 

#####  Put code here to specify the values of S, T, t, K, sigma, and r  #####
#############################################################################
T=1
t=0
K=80
S=77
sigma=0.285
r=0.01
Niter = 1e08
BlackScholesSim = function(S, T, t, K, sigma, r, Niter = 1e08, seed = 4360)
{
set.seed(seed)
Z = rnorm(Niter)
X  = S*exp(-sigma^2/2*(T-t) + sigma*sqrt(T-t)*Z) - K*exp(-r*(T-t))
Ind = as.numeric(X>0)
payoff = X*Ind
c(mean(payoff),sd(payoff),Niter)
}
t1 = proc.time()
sim_results = BlackScholesSim(S,T,t,K,sigma,r)
t2= proc.time()
t2-t1

#Q4
bs = function(S,T,t,K,sigma,r)
{
  d1=(log(S/K)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
  d2=d1-sigma*sqrt(T-t)
  C=pnorm(d1)*S-pnorm(d2)*K*exp(-r*(T-t))
  C
}
#using the previous values :
bs(S,T,t,K,sigma,r)

#Q5
put<- bs(S,T,t,K,sigma,r)+K*exp(-r*t)-S

#Q6
mu=0.05
sig=0.25
iter=5000
stock<-S*exp(rnorm(iter,mean=mu/2,sd=sig/sqrt(2)))
callprice=bs(stock,T,t+1/2,K,sigma,r)
logreturn<-log(callprice/bs(S,T,t,K,sigma,r))
c(mean(logreturn),var(logreturn))
hist(logreturn)









