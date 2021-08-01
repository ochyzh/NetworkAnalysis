library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)

library(statnet)
library(sna)
library(ergm.count)

data(gadeData)
table(gadeData$coopActions)

gadeData$coopBin<-as.numeric(gadeData$coopActions>0)
table(gadeData$coopBin)


actors<- sort(unique(c(gadeData$Var1,gadeData$Var2)))
gadeData<-sort(gadeData)

dyadVars<-names(gadeData)[c(12,5:8)]


n<-length(actors)
p<-length(dyadVars)

dyadArray<-array(0,dim=c(n,n,p),
                 dimnames=list(actors,actors,dyadVars))

dyadArray[1:10,1:10,1]

for(param in dyadVars){
  for(i in 1:nrow(gadeData)){
    a1=gadeData$Var1[i]
    a2=gadeData$Var2[i]
    val=gadeData[i,param]
    dyadArray[a1,a2,param]=val
  }
}

#Node covariates
nodeVars<-names(gadeData)[9:11]
nodeData<-unique(gadeData[,c('Var1',nodeVars)])
rownames(nodeData)<-nodeData$Var1
nodeData<-nodeData[,-1]

#DV:
net<-as.network(dyadArray[,,'coopBin'], directed=FALSE,
                loops=FALSE,matrix.type='adjacency')

#Node attrs
network::set.vertex.attribute(net,'averageId.node',
                     nodeData$averageId.node)
network::set.vertex.attribute(net,'size.node',
                              nodeData$size.node)
network::set.vertex.attribute(net,'spons_actor.node',
                              nodeData$spons_actor.node)

#Dyad cov:

set.network.attribute(net,'loc.dyad',dyadArray[,,'loc.dyad'])
set.network.attribute(net,'spons.dyad',dyadArray[,,'spons.dyad'])

plot(net,label=network.vertex.names(net))

m0<-ergm(net~edges+nodecov('averageId.node')+
           nodecov('size.node')+
           nodecov('spons_actor.node')+
           absdiff('averageId.node')+
           absdiff('size.node')+
           edgecov('loc.dyad')+
           edgecov('spons.dyad'))
summary(m0)

m1<-ergm(net~edges+nodecov('averageId.node')+
           nodecov('size.node')+
           nodecov('spons_actor.node')+
           absdiff('averageId.node')+
           absdiff('size.node')+
           edgecov('loc.dyad')+
           edgecov('spons.dyad')+
           gwesp(decay=.5,fixed=TRUE))
summary(m1)

AIC(m0)

gofM1=gof(m1,GOF=~degree+espartners+distance-model)
plot(gofM1)

#Count ergms
gadeData$coopActions<-round(gadeData$coopActions^2)
actors<- sort(unique(c(gadeData$Var1,gadeData$Var2)))
gadeData<-sort(gadeData)

dyadVars<-names(gadeData)[c(3,5:8)]


n<-length(actors)
p<-length(dyadVars)

dyadArray<-array(0,dim=c(n,n,p),
                 dimnames=list(actors,actors,dyadVars))

dyadArray[1:10,1:10,1]

for(param in dyadVars){
  for(i in 1:nrow(gadeData)){
    a1=gadeData$Var1[i]
    a2=gadeData$Var2[i]
    val=gadeData[i,param]
    dyadArray[a1,a2,param]=val
  }
}

#Node covariates
nodeVars<-names(gadeData)[9:11]
nodeData<-unique(gadeData[,c('Var1',nodeVars)])
rownames(nodeData)<-nodeData$Var1
nodeData<-nodeData[,-1]

#DV:
net<-as.network(dyadArray[,,'coopActions'], directed=FALSE,
                loops=FALSE,matrix.type='adjacency',ignore.eval=FALSE,
                names.eval="coopActions")

as.matrix(net, attrname = "coopActions")[1:5, 1:5]
#Node attrs
network::set.vertex.attribute(net,'averageId.node',
                              nodeData$averageId.node)
network::set.vertex.attribute(net,'size.node',
                              nodeData$size.node)
network::set.vertex.attribute(net,'spons_actor.node',
                              nodeData$spons_actor.node)

#Dyad cov:

set.network.attribute(net,'loc.dyad',dyadArray[,,'loc.dyad'])
set.network.attribute(net,'spons.dyad',dyadArray[,,'spons.dyad'])
set.network.attribute(net,'soopActions',dyadArray[,,'coopActions'])
plot(net, edge.lwd=.25*dyadArray[,,"coopActions"])

m0 <- ergm(net ~ sum +
             nodecov('averageId.node') +
             nodecov('size.node') +
             nodecov('spons_actor.node') +
             absdiff('averageId.node') +
             absdiff('size.node') +
             edgecov('loc.dyad') +
             edgecov('spons.dyad'),
           response = "coopActions", reference = ~Poisson)
mcmc.diagnostics(m0)

summary(m0)

