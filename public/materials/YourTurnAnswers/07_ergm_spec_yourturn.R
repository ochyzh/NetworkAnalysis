library(statnet)
library(devtools)
#install_github("ochyzh/networkdata")
library(networkdata)

#Prior code:
data(legnet)
mynet<-network(el, matrix.type="edgelist",
               directed=TRUE, loops=FALSE)
edist <- as.matrix(edist)
# Define network attribute
set.network.attribute(mynet,"dist",edist)
set.vertex.attribute(mynet,"ideol",dwnom$dwnom)


#Your Turn: Plot the cosponsorship network and estimate an ergm


plot(mynet, label = network.vertex.names(mynet))
#an ergm with edges, ideological, and geographic homophily:
est1 <- ergm(mynet~edges+absdiff("ideol")+edgecov("dist"))
summary(est1)

# Check goodness of fit
# compute GOF
set.seed(1234)
par(mfrow=c(3,2))
gof1 <- gof(est1,control=control.gof.ergm(nsim=200))
# plot observed values against boxplots of simulated
plot(gof1)

set.seed(45)
# add popularity effect
est2 <- ergm(mynet~edges+absdiff("ideol")+edgecov("dist")+istar(2),control=control.ergm(MCMC.samplesize=1000))
summary(est2)
# Check MCMC performance
mcmc.diagnostics(est2)

# Check goodness of fit
set.seed(1234)
gof2 <- gof(est2,control=control.gof.ergm(nsim=200))
par(mfrow=c(3,2))
plot(gof2)



# add transitivity
set.seed(45)
est3 <- ergm(mynet~edges+absdiff("ideol")+edgecov("dist")+istar(2)+triangle,control=control.ergm(MCMC.samplesize=1000))
mcmc.diagnostics(est3)

# try GWESP
set.seed(45)
est4 <- ergm(mynet~edges+absdiff("ideol")+edgecov("dist")+istar(2)+gwesp(0,fixed=T),control=control.ergm(MCMC.samplesize=5000))
# Check for degeneracy
mcmc.diagnostics(est4)

# Increase MCMC sample size
set.seed(45)
est5 <- ergm(mynet~edges+absdiff("ideol")+edgecov("dist")+istar(2)+gwesp(0,fixed=T),control=control.ergm(MCMC.samplesize=10000))
# Check for degeneracy
mcmc.diagnostics(est5)


set.seed(1234)
gof5 <- gof(est5,control=control.gof.ergm(nsim=200))
par(mfrow=c(3,2))
plot(gof5)

summary(est5)
