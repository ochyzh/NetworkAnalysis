mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
all<- NULL


y<- mydata$admit
x<-mydata$gre

alpha<- -5 #we don't know so we'll try different guesses
beta<- .0005

log_odds<- alpha +beta*x
odds<- exp(log_odds)
prob<-odds/(1+odds)
log_like<- y*log(prob)+(1-y)*log(1-prob)
sum_ll<- sum(log_like)

results<- cbind.data.frame("alpha"=alpha, "beta"=beta, "sum_ll"=sum_ll)
all<- rbind(all, results)

#Program the likelihood:
MyLogLike<-function(Y,X,par){
  xbeta<-X%*%par
  p<-exp(xbeta)/(1+exp(xbeta))
  loglike<-Y*log(p)+(1-Y)*(log(1-p))
  sum_ll= -sum(loglike)
  return(sum_ll)
}

X<-cbind(1,x)

MyLogLike(Y=y, X=X, par=c(-5,.0005))

par=rep(0,2)
myres <- optim(par,            # starting value for prob
               MyLogLike,      # the log-likelihood function
               method="BFGS",               # optimization method
               hessian=TRUE,                # return numerical Hessian
               control=list(reltol=1e-10),
               X=X,Y=y)                 # the data
myres$par


summary(m1<-glm(admit~gre, data=mydata,
                family=binomial))


plogis(coef(m1)[1]+coef(m1)[2]*600)
plogis(coef(m1)[1]+coef(m1)[2]*800)

library(networkdata)
data(lsgm_data)

loglik_lsgm<-function(par,Y,W){
  b0<-par[1] #starting value for constant
  eta<-par[2] #starting value for spatial paramenter
  X<-rep(1,length(Y)) #constant
  xbeta<-as.matrix(X)%*%b0 #natural parameter
  kappa<-exp(xbeta)/(1+exp(xbeta)) #logit of Xb
  A_i=log(kappa/(1-kappa))+eta*W%*%(Y-kappa) #Eqn 2--natural parameter function
  p_i<- exp(A_i)/(1+exp(A_i)) #Eqn 1, also Eqn 4
  PL<-Y*log(p_i)+(1-Y)*log(1-p_i) #Eqn 3, log psedo-likelihood
  ell <- -sum(PL) #sum log pseudolikelihoods
  cat("ell",ell, fill=TRUE) #print sum to screen
  return(ell)
}


m1<-optim(par=c(0,0),loglik_lsgm,W=Wmat,Y=Y)

m1$par

spatbin.genone<-function(coeffs,w,curys){
  b0<-coeffs[1]
  eta<-coeffs[2]
  X<-rep(1,length(Y))
  xbeta<-as.matrix(X)%*%b0
  kappa<-exp(xbeta)/(1+exp(xbeta))
  A_i=log(kappa/(1-kappa))+eta*w%*%(curys-kappa)
  p_i<- exp(A_i)/(1+exp(A_i))
  y<- rbinom(n=length(curys), size=1, prob=p_i)
  return(y)
}


spatbin.onegibbs<-function(coeffs,w,curys){
  cnt<-0
  n<-length(curys)
  newys<-NULL
  repeat{
    cnt<-cnt+1
    ny<-spatbin.genone(coeffs=coeffs,w=w,curys=curys)
    curys[cnt]<-ny[cnt]
    if(cnt==n) break
  }
  newys<-curys
  return(newys)
}


spatbin.genfield<-function(coeffs,w,y0s,M){
  curys<-y0s
  cnt<-0
  res<-as.data.frame(y0s)
  repeat{
    cnt<-cnt+1
    newys<-spatbin.onegibbs(coeffs=coeffs,w=w,curys=curys)
    curys<-newys
    res<-cbind(res,curys)
    if(cnt==M) break
  }
  return(res)
}





n<-length(Y)
y0s=rbinom(n=n, size=1, prob=.5)
sims<-spatbin.genfield(coeffs=m1$par,w=Wmat,y0s=y0s,M=1000)
#Take every 10th simulated network, i.e. burnin=10, thinning=10
sims<-sims[,seq(from=10, to=ncol(sims),by=10)]

sim_est<-function(Y){
  res<-optim(par=m1$par,loglik_lsgm,W=Wmat,Y=as.matrix(Y))
  return(c(res$par,res$convergence))
}
library(parallel)
sim_est<-do.call("rbind",mclapply(sims, sim_est))

sim_est<-sim_est[sim_est[,3]==0,]

#Get sds of the estimates:
boot_se<-apply(sim_est,2,sd)
mytable<-cbind("coeff"=m1$par,"se"=boot_se[-3],"z-value"=(m1$par/boot_se[-3]))
mytable



