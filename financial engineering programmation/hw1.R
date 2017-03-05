returns <- read.csv("C:/Users/alexis/Desktop/OR_tools_for_FE/returns.csv")
attach(returns)
class(Date)
Date<-as.Date(Date)
class(Date)
head(returns)
tail(returns)
mode(returns)
class(returns)
summary(returns)
str(returns)

stock.names<-c("AAPL","BAX","INLC","JNJ","SYY","SP500")
returns2<-returns[,stock.names]
plot(returns2)
plot(as.ts(returns2))

library(psych)
pairs.panels(returns2,breaks=50)

n=length(SP500)
plot(SP500[-n],SP500[-1],xlab=expression(r[t-1]),
     ylab=expression(R[t]),xlim=c(-0.12,0.12),
     ylim=c(-0.12,0.12))

options(digits=3)
cov(returns2)
cor(returns2)
colMeans(returns2)

pnorm(log(0.99),mean=0.0008,sd=0.015)
pnorm(log(0.99),mean=5*0.0008,sd=sqrt(5)*0.015)

#monte-carlo
niter<-50000
minPrice<-matrix(nrow=niter,ncol=1)
set.seed(4630)
for (i in 1:niter)
{logR=rnorm(45,mean=0.008,sd=0.016)
prices=19000*exp(cumsum(logR))
minPrice[i]=min(prices)
}
prob<-mean((minPrice<18700)) #probability
se_prob=sqrt((prob*(1-prob))/niter)
m=mean(minPrice)
SE=sd(minPrice)/sqrt(niter)

qnorm(0.99)
sd((minPrice<18700))/sqrt(niter)

