####exemple Quadratic programming
library(quadprog)
Stock_FX_Bond <- read.csv("C:/Users/alexis/Desktop/Cornell 2016-2017/OR_tools_for_FE/cours_exemple/Stock_FX_Bond.csv")
data=Stock_FX_Bond
prices=data[,c(3,5,7,9,11)]
n=nrow(prices)
r=log(prices[-1,]/prices[-n,])
plot(as.ts(r))
n=nrow(r)
m=ncol(r)

mean_vect=colMeans(r)
cov_mat=cov(r)
sd_vect=apply(r,2,sd)
mufree=0.0002
Amat=cbind(rep(1,m),mean_vect)
muP=seq(0.0001,0.001,length=300)
sdP=muP
wei <-matrix(0,nrow=300,ncol=m)

for (i in 1:length(muP))  #  loop
{
result =
solve.QP(Dmat=cov_mat,dvec=rep(0,m),
         Amat=Amat,bvec=c(1,muP[i]),meq=2)
sdP[i] = sqrt(2*result$value)
wei[i,] = result$solution
}

plot(sdP,muP,type="l",ylim=c(0.0001,0.0012),xlim=c(0,.0275))

############## avec contraintes
library(quadprog)
Stock_FX_Bond <- read.csv("C:/Users/alexis/Desktop/Cornell 2016-2017/OR_tools_for_FE/cours_exemple/Stock_FX_Bond.csv")
data=Stock_FX_Bond
prices=data[,c(3,5,7,9,11)]
n=nrow(prices)
r=log(prices[-1,]/prices[-n,])
plot(as.ts(r))
n=nrow(r)
m=ncol(r)

mean_vect=colMeans(r)
cov_mat=cov(r)
sd_vect=apply(r,2,sd)
mufree=0.0002
Amat=cbind(rep(1,m),mean_vect,diag(1,nrow=m))
muP=seq(1.0001*mean_vect,0.9999*max(mean_vect),length=300)
sdP=muP
wei <-matrix(0,nrow=300,ncol=m)

for (i in 1:length(muP))  #  loop
{
  result =
    solve.QP(Dmat=cov_mat,dvec=rep(0,m),
             Amat=Amat,bvec=c(1,muP[i],rep(0,m)),meq=2)
  sdP[i] = sqrt(2*result$value)
  wei[i,] = result$solution
}


########################################## linear prog
dat=read.csv("C:\\Users\\alexis\\Desktop\\Cornell 2016-2017\\OR_tools_for_FE\\cours_exemple\\StockData.csv",header=T)
returns=dat[,1:5]
mean_vect=colMeans(returns)
cov_mat=cov(returns)
M=length(mean_vect)
B1=0.3
B2=0.15 #upper and lower bounds for w

AmatLP1 = cbind(diag(1,nrow=M),matrix(0,nrow=M,ncol=M))
AmatLP2 = cbind(matrix(0,nrow=M,ncol=M),diag(1,nrow=M))
AmatLP3 = c(rep(1,M),rep(-1,M))
AmatLP = rbind(AmatLP1,AmatLP2,AmatLP3)
bvecLP = c(rep(B1,M),rep(B2,M),1)
cLP = c(mean_vect,-mean_vect)
const.dir = c(rep("<=",2*M),"=")

library(linprog)
resultLP_min = solveLP(cvec=cLP,bvec=bvecLP,Amat=AmatLP,
lpSolve=T,const.dir = const.dir,maximum=FALSE)
resultLP_max = solveLP(cvec=cLP,bvec=bvecLP,Amat=AmatLP,
lpSolve=T,const.dir = const.dir,maximum=TRUE)


