
#install_github("ochyzh/networkdata")
library(networkdata)
library(statnet)

data("allyData")

#IV:
class(war)
length(war)
class(war[[1]])
dim(war[[1]])
war[[1]][1:3,1:3]

class(contiguity)
dim(contiguity)
contiguity[1:3,1:3]

#DV:
class(ally)
length(ally)
class(ally[[1]])

list.vertex.attributes(ally[[1]])

allyDyn<-networkDynamic(network.list=ally, start=0, end=9)
par(mfrow=c(1,2))
p<-plot(network.extract(allyDyn, at=0),main="1991")
plot(network.extract(allyDyn, at=9),main="2000", coord=p)

library(ndtv)
render.d3movie(allyDyn, filename="AlliesNetwork.html")

library(tsna)
tSnaStats(allyDyn,"gden")
tSnaStats(allyDyn,"gtrans")

tErgmStats(allyDyn, "~edges+triangle")

war1<-war
for (i in 1:10){
  war1[[i]]<-network(war[[i]])
}
war1[[1]]
WarDyn<-networkDynamic(network.list=war1)
tErgmStats(WarDyn, "~ edges+triangle")


par(mfrow=c(1,2))
p<-plot(network.extract(WarDyn, at=0),main="1991")
plot(network.extract(WarDyn, at=9),main="2000", coord=p)

render.d3movie(WarDyn, filename="WarNetwork.html")

library(btergm)
m1<-btergm(ally~ edges+edgecov(war)+edgecov(contiguity)+
             nodecov('polity')+absdiff('polity')+
             nodecov('cinc')+absdiff('cinc')+
             gwesp(.5,fixed=TRUE)+
             timecov())
summary(m1)

data(duqueData)

class(dipl_ties)
length(dipl_ties)
class(dipl_ties[[1]])

for (i in 1:8){
  dipl_ties[[i]]<-network(as.matrix(dipl_ties[[i]]))
}


for (i in 1:8){
  set.network.attribute(dipl_ties[[i]],'vertex.pid','vertex.names')
}

diplDyn<-networkDynamic(network.list=dipl_ties,
                        vertex.pid='vertex.names')
par(mfrow=c(1,2))
plot(network.extract(diplDyn, at=0),main="1970")
plot(network.extract(diplDyn, at=6),main="2005")

class(contig[[1]])
contig[[1]][1:3,1:3]

class(allies[[1]])

for (i in 1:8){
  contig[[i]]<-as.matrix(contig[[i]])
  allies[[i]]<-as.matrix(allies[[i]])
}

set.vertex.attribute(dipl_ties[[1]], "dem",
                     polity$dem_dum[polity$year==1970])
set.vertex.attribute(dipl_ties[[2]], "dem",
                     polity$dem_dum[polity$year==1975])
set.vertex.attribute(dipl_ties[[3]],"dem",polity$dem_dum[polity$year==1980])
set.vertex.attribute(dipl_ties[[4]],"dem",polity$dem_dum[polity$year==1985])
set.vertex.attribute(dipl_ties[[5]],"dem",polity$dem_dum[polity$year==1990])
set.vertex.attribute(dipl_ties[[6]],"dem",polity$dem_dum[polity$year==1995])
set.vertex.attribute(dipl_ties[[7]],"dem",polity$dem_dum[polity$year==2000])
set.vertex.attribute(dipl_ties[[8]],"dem",polity$dem_dum[polity$year==2005])

dipl_ties[[1]] %v% "dem"


library(btergm)
m1<-btergm(dipl_ties~edges+ istar(2)+ostar(2)+mutual+
             triangle+
             absdiff("dem")+
             nodeicov('dem')+
             edgecov(allies)+
             edgecov(contig))

