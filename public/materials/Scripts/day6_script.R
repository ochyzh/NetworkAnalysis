library(tidyverse)
library(magrittr)
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")


#Program the likelihood:
MyLogLike<-function(Y,X,par){
  xbeta<-X%*%par #xbeta
  p<-exp(xbeta)/(1+exp(xbeta)) #write out p(y=1|x)
  loglike<-Y*log(p)+(1-Y)*(log(1-p))  #logged likelihood for one obs
  sum_ll= -sum(loglike) #sum of logged likelihoods
  return(sum_ll)
}

X<- mydata %>% mutate(cons=1, rank2=as.numeric(rank==2),
                      rank3=as.numeric(rank==3),
                      rank4=as.numeric(rank==4)) %>%
  select(cons, gre, gpa, rank2, rank3, rank4) %>% as.matrix()
Y<-mydata$admit
par=rep(0,6)


myres <- optim(par,            # starting value for prob
               MyLogLike,      # the log-likelihood function
               method="BFGS",               # optimization method
               hessian=TRUE,                # return numerical Hessian
               control=list(reltol=1e-10),    # tolerance
               X=X,Y=Y)                 # the data
myres$par


summary(m1<-glm(admit~gre+ gpa+ factor(rank), data=mydata,
                family=binomial))
cbind(myres$par, coef(m1))
