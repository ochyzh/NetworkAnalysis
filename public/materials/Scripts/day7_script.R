#install.packages("statnet")
library(statnet)
library(sna)

data(sampson)
m3<-ergm(samplike~edges+nodematch('group')+mutual)
summary(m3)

#What is the prob of a tie for two nodes that
#are in diff groups and there is no tie from i to j?
xbeta<-m3$coef[1]
p<-exp(xbeta)/(1+exp(xbeta))
plogis(coef(m3)[['edges']])

#What is the prob of a tie for two nodes that
#are same group and there is no tie from i to j?
xbeta<-m3$coef[1]+m3$coef[2]
p<-exp(xbeta)/(1+exp(xbeta))
plogis(coef(m3)[['edges']]+coef(m3)[['nodematch.group']])

#What is the prob of a tie for two nodes that
#are same group and there is a tie from i to j?
xbeta<-m3$coef[1]+m3$coef[2]+m3$coef[3]
p<-exp(xbeta)/(1+exp(xbeta))

#What is the prob of a tie for two nodes that
#are diff groups and there is a tie from i to j?
xbeta<-m3$coef[1]+m3$coef[3]
p<-exp(xbeta)/(1+exp(xbeta))

mcmc.diagnostics(m3)
simNets<-simulate(m3, nsim=5)


p<-plot(samplike, vertex.cex=degree(samplike,
        cmode="indegree")/2,
        vertex.col='group')
plot(simNets[[1]], vertex.cex=degree(simNets[[1]],
        cmode="indegree")/2,
     vertex.col='group', coord=p)
plot(simNets[[2]], vertex.cex=degree(simNets[[2]],
                                     cmode="indegree")/2,
     vertex.col='group', coord=p)


gofM3<-gof(m3, GOF=~idegree+odegree+espartners+distance-model)
par(mfrow=c(2,2))
plot(gofM3)



#Your Turn
data(coleman)
friends<-as.network.matrix(coleman[1,,],
                           matrix.type="adjacency",
                           directed=TRUE)
m11<-ergm(friends~edges)
summary(m11)
plogis(coef(m11))

m12<-ergm(friends~edges+mutual)
summary(m12)

plogis(coef(m12)[['edges']]+coef(m12)[['mutual']])




