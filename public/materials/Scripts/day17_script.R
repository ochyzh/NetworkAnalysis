library(devtools)
install_github("ochyzh/lsgm")
library(lsgm)
?lsgm

data(W)
data(toy_data)

loglik<-function(b0,Y){
X<-rep(1,length(Y))
theta<-as.matrix(X)%*%b0
p_i <-exp(theta)/(1+exp(theta))
LL<-Y*log(p_i)+(1-Y)*log(1-p_i)
ell<- -1*sum(LL)
return(ell)
}

b0<-0
m0<-optim(par=b0, loglik, Y=toy_data$Y)
m0<-glm(data=toy_data, Y~1, family=binomial())

loglik<-function(pars,Y,X,W){
  betas<-pars[-length(pars)]
  eta<-pars[length(pars)]
  theta<-as.matrix(X)%*%betas
  kappa<-exp(theta)/(1+exp(theta))
  A_i=log(kappa/(1-kappa))+eta*W%*%(Y-kappa)
  p_i <-exp(A_i)/(1+exp(A_i))
  PL<-Y*log(p_i)+(1-Y)*log(1-p_i)
  ell<- -1*sum(PL)
  return(ell)
}

m1<-optim(par=c(0,0), loglik, Y=toy_data$Y, W=W)

library(networkdata)
data("ally_data")

X<-cbind(1,log(ally_data$tot_trade+1))
Y<-ally_data$defense

m1<-optim(par=c(0,0,0), loglik, Y=Y,X=X, W=W)
