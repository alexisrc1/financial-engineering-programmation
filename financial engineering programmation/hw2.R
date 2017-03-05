#Random Walk
n=2530;time=1:n;set.seed(132035) #253 business days par an, donc pour 10 ans
p=log(100)+cumsum(rnorm(n,mean=0.03/253,sd=0.25/sqrt(253)))
pdf("SimRandWalk.pdf", width = 7, height = 6)
par(mfrow = c(2, 1))

plot(time, exp(p), type = "l",main = "geometric random walk", ylab = "prices")


#t-densities

library(fGarch)
t=seq(-7,7,length=201)
plot(t,dstd(t,mean = 1,sd=1.5,nu=2.2),lwd=2,type="l")
lines(t, dstd(t, mean = 1, sd = 1.5, nu = 5),col="red",lwd=2)
#lines pour faire sur un seul graphique les 2
abline(v = 1)
abline(h = 0)

#Black mondays
return=diff(log(returns[,29]))
#fitdistrib
library(MASS)
rr=diff(returns[,29])
fit_t=fitdistr(rr,"t")$estimate
        

#homework2


load("stock.data.RData")
head(Returns)
r = Returns$F
n = length(r)

#Q1
#histogram
h<-hist(r, breaks=12, col="blue", xlab="returns",
        main="Histogram of returns",ylim = c(0,1600))
xfit<-seq(-0.2,0.2,length=40)
yfit<-dnorm(xfit,mean=mean(r),sd=sd(r))
yfit <- yfit*diff(h$mids[1:2])*n 
lines(xfit, yfit, col="red", lwd=2) 
#skewness?
library(e1071)
skewness(r)

#Q2
library(MASS)
fit_t<-fitdistr(r,"t")$estimate
mu=fit_t[1]
nu=fit_t[3]
sd=fit_t[2]*(nu/(nu-2))

#Q3
library(fGarch)
t=seq(-3*mu,3*mu,length=201)
plot(t,dstd(t,mean=mu,sd=sd,nu=nu),type="l",ylab="density(t)",xlab="t",lwd=2)

t_quantiles=qt((1:(n-1))/n,df=nu)
qqplot(r,t_quantiles)
fit=lm(qt(c(1/4,3/4),df=nu)~quantile(r,c(1/4,3/4)))
abline(fit,col="red",lwd=2)


plot(Date, r) # time series plot of log returns
yearSeq = seq(Date[1], Date[length(Date)], by='years') # 1-year increments
#   over the range of available dates
axis.Date(side = 1, dates, at=yearSeq,format = "%Y")  # label the
#x-axis by 1-year increments
abline(v=yearSeq, col='red') # mark the years with vertical lines
