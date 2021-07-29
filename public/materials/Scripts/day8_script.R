library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)

library(statnet)
library(sna)

data(sampson)


m3<-ergm(samplike~edges+nodematch('group')+mutual)

m4<-ergm(samplike~edges+nodematch('group')+mutual+
           idegree1.5)
summary(m4)

gofM4<-gof(m4, GOF=~idegree+odegree+espartners+distance-model)
par(mfrow=c(2,2))
plot(gofM4)

AIC(m3)

round(sapply(list(m3,m4,m5),AIC),0)
round(sapply(list(m3,m4),BIC),0)

m5<-ergm(samplike~edges+nodematch('group')+mutual+
           idegree1.5+triangles)
summary(m5)

m6<-ergm(samplike~edges+nodematch('group')+mutual+
           idegree1.5+gwesp(decay=.5,fixed=TRUE))
summary(m6)


data(legnet)
#DV
mynet<-network(el,matrix.type="edgelist",directed=TRUE,
               loops=FALSE)
#E covariates
edist<-as.matrix(edist)
set.network.attribute(mynet, "dist", edist)

list.network.attributes(mynet)

#N covariates:
set.vertex.attribute(mynet,"ideol",dwnom$dwnom)

plot(mynet, label=network.vertex.names(mynet))

est1<-ergm(mynet~edges+ absdiff("ideol")+edgecov('dist'))
summary(est1)
gof1<-gof(est1, GOF=~idegree+odegree+espartners+distance-model)
par(mfrow=c(2,2))
plot(gof1)


est2<-ergm(mynet~edges+ absdiff("ideol")+
             edgecov('dist')+istar(2))
summary(est2)
mcmc.diagnostics(est2)

est3<-ergm(mynet~edges+ absdiff("ideol")+
             edgecov('dist')+istar(2)+triangles)
summary(est3)

est4<-ergm(mynet~edges+ absdiff("ideol")+
             edgecov('dist')+istar(2)+
             gwesp(decay=.5, fixed=T),
           control=control.ergm(MCMC.samplesize=10000))
summary(est4)
mcmc.diagnostics(est4)

data(gadeData)
table(gadeData$coopActions)

gadeData$coopBin<-as.numeric(gadeData$coopActions>0)
table(gadeData$coopBin)


actors<- sort(unique(c(gadeData$Var1,gadeData$Var2)))
gadeData<-sort(gadeData)

dyadVars<-names(gadeData)[c(12,5:8)]



