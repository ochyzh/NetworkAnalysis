#library(devtools)
#install_github("ochyzh/networkdata")
library(networkdata)
data(lsgm_data)


#Logit likelihood:
MyLogLike<-function(Y,par){
  X<-rep(1,length(Y)) #constant
  xbeta<-as.matrix(X)%*%par
  p<-exp(xbeta)/(1+exp(xbeta))
  loglike<-Y*log(p)+(1-Y)*(log(1-p))
  sum_ll= -sum(loglike)
  return(sum_ll)
}


#Likelihood for LSGM with intercept only:
loglik<-function(par,W,Y){
  b0<-par[1] #intercept
  eta<-par[2] #spatial coefficient
  xbeta<-as.matrix(X)%*%b0  #
  kappa<-exp(xbeta)/(1+exp(xbeta)) #logit of Xb
  A_i=log(kappa/(1-kappa))+eta*W%*%(Y-kappa) #Eqn 2
  p_i<- exp(A_i)/(1+exp(A_i)) #Eqn 1, also Eqn 4
  PL<-Y*log(p_i)+(1-Y)*log(1-p_i) #Eqn 3
  ell <- -sum(PL)
  #cat("ell",ell, fill=TRUE)
  return(ell)
}

par=0
m0<-optim(par=c(0),MyLogLike,Y=Y)
m0
m0<-glm(Y~1, family=binomial)

mean(Y)
plogis(m0$par) #reverse logistic transformation
phat=exp(m0$par)/(1+exp(m0$par))
phat

m1<-optim(par=c(0,0),loglik,W=Wmat,Y=Y)
m1
