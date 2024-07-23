library(networkdata)
#Open the data:
data("ally_data")
ally_data$tot_trade<-log(ally_data$tot_trade+1)
ally_data<-ally_data[ally_data$year==2007,]
ally_data[1:5,]

#Prepare W:
W2007 <- W
W2007[1:5,1:5]
#W2007[,1]<-NULL  #remove first column from similarity matrix
W2007[is.na(W2007)]<-0
W2007<-W2007/10
W2007<-as.matrix(W2007)

#Likelihood
loglik<-function(par,X,W,Y){
  b0<-par[1]
  b1<-par[2]
  eta<-par[3]
  xbeta<-b0+b1*X
  kappa<-exp(xbeta)/(1+exp(xbeta)) #logit of Xb
  A_i=log(kappa/(1-kappa))+eta*W%*%(Y-kappa) #Eqn 2
  p_i<- exp(A_i)/(1+exp(A_i)) #Eqn 1, also Eqn 4
  PL<-Y*log(p_i)+(1-Y)*log(1-p_i) #Eqn 3
  ell <- -sum(PL)
  #cat("ell",ell, fill=TRUE)
  return(ell)
}

X=ally_data$tot_trade
Y=ally_data$defense
m1<-optim(par=c(0,0,0),loglik,X=X,W=W2007,Y=Y)
m1

spatbin.genone<-function(coeffs,X,w,curys){
  b0<-coeffs[1]
  b1<-coeffs[2]
  eta<-coeffs[3]
  xbeta<- b0+b1*X
  kappa<-exp(xbeta)/(1+exp(xbeta))
  A_i=log(kappa/(1-kappa))+eta*w%*%(curys-kappa)
  p_i<- exp(A_i)/(1+exp(A_i))
  y<- rbinom(n=length(curys), size=1, prob=p_i)
  return(y)
}
spatbin.onegibbs<-function(coeffs,X,w,curys){
  cnt<-0
  n<-length(curys)
  newys<-NULL
  repeat{
    cnt<-cnt+1
    ny<-spatbin.genone(coeffs=coeffs,X=X,w=w,curys=curys)
    curys[cnt]<-ny[cnt]
    if(cnt==n) break
  }
  newys<-curys
  return(newys)
}

spatbin.genfield<-function(coeffs,X,w,y0s,M){
  curys<-y0s
  cnt<-0
  res<-as.data.frame(y0s)
  repeat{
    cnt<-cnt+1
    newys<-spatbin.onegibbs(coeffs=coeffs,X=X,w=w,curys=curys)
    curys<-newys
    res<-cbind(res,curys)
    if(cnt==M) break
  }
  return(res)
}


n<-length(Y)
y0s=rbinom(n=n, size=1, prob=.5)
sims<-spatbin.genfield(coeffs=m1$par,X=X,w=W2007,y0s=y0s,M=100)
#Take every 10th simulated network, i.e. burnin=10, thinning=10
sims<-sims[,seq(from=10, to=ncol(sims),by=10)]
#saveRDS(sims, "sims.rds")


#sims<-readRDS("data/sims.rds")
sim_est<-function(Y){
  res<-optim(par=m1$par,loglik,X=X,W=W2007,Y=as.matrix(Y))
  return(c(res$par,res$convergence))
}
library(parallel)
sim_est<-do.call("rbind",mclapply(sims, sim_est))
#Drop results if didn't converge (models that converged have convergence=0)
sim_est<-sim_est[sim_est[,4]==0,]
#saveRDS(sim_est,"./data/sim_est.rds")
boot_se<-apply(sim_est,2,sd)
mytable<-cbind("coeff"=m1$par,"se"=boot_se[-4],"z-value"=(m1$par/boot_se[-4]))
mytable


library(devtools)
install_github("ochyzh/lsgm")
library(lsgm)
data(W)
data(toy_data)
lsgm(Y=as.matrix(toy_data$Y),W=W,X=as.data.frame(toy_data$X))



#ERGMs
library(statnet)
data('sampson')
samplike


adjMat = as.matrix.network(samplike)

network::get.vertex.attribute(samplike, 'group')
samplike %v% 'group'

set.seed(6886)
vertexSize = degree(samplike, cmode = 'indegree')/2
p<-plot(samplike,
        displaylabels = TRUE,
        # size of nodes based on vector vertexSize
        vertex.cex = vertexSize,
        # color of nodes based on vertex attribute: group
        vertex.col = 'group'
)

m1 = ergm(samplike ~ edges)
summary(m1)

plogis(coef(m1))

m2 = ergm(samplike ~ edges + nodematch('group'))
summary(m2)

xbeta<- -2+1*2.6481
plogis(xbeta)

plogis(coef(m2)[1]+1*coef(m2)[2]) #prob same group tie
plogis(coef(m2)[1]+0*coef(m2)[2]) #prob diff group tie



plogis(coef(m2)[1]+0*coef(m2)[2]) #
m3 = ergm(samplike ~ edges + nodematch('group') +
            mutual)

plogis(coef(m3)[1]+1*coef(m3)[2]+1*coef(m3)[3])#prob same group tie
plogis(coef(m3)[1]+0*coef(m3)[2]+1*coef(m3)[3])#prob same group tie

library(sna)
data(coleman)



net1<- coleman[1,,]


m4 = ergm(samplike ~
            edges + nodematch('group', diff=TRUE) + nodefactor('group')+
            mutual + idegree1.5)
mcmc.diagnostics(m4)

summary(m4)


set.seed(6886)
simNets = simulate(m3, nsim = 5)
# Define a plotting function:
plotSimNet = function(net, label){
  set.seed(6886)
  plot(net, displaylabels = FALSE,
       vertex.cex = degree(net, cmode = 'indegree')/2, edge.col = "black",
       vertex.col = 'group', coord=p )
  title(label) }

simNets = simulate(m4, nsim = 5)
par(mfrow = c(2, 3))
simNets[[6]] = samplike
labels = c(paste0("sim",1:5), 'actual')
lapply(1:length(simNets), function(i){
  plotSimNet(simNets[[i]], labels[i]) })







