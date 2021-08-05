#Your Turn: TERGMs
#Re-specify the model to account for delayed reciprocity and stability.
#Estimate your new model.

#library(devtools)
#install_github("ochyzh/networkdata")
library(networkdata)
library(statnet)

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


for (i in 1:8){
  contig[[i]]<-as.matrix(contig[[i]])
  allies[[i]]<-as.matrix(allies[[i]])
}

network::set.vertex.attribute(dipl_ties[[1]], "dem",
                     polity$dem_dum[polity$year==1970])
network::set.vertex.attribute(dipl_ties[[2]], "dem",
                     polity$dem_dum[polity$year==1975])
network::set.vertex.attribute(dipl_ties[[3]],"dem",polity$dem_dum[polity$year==1980])
network::set.vertex.attribute(dipl_ties[[4]],"dem",polity$dem_dum[polity$year==1985])
network::set.vertex.attribute(dipl_ties[[5]],"dem",polity$dem_dum[polity$year==1990])
network::set.vertex.attribute(dipl_ties[[6]],"dem",polity$dem_dum[polity$year==1995])
network::set.vertex.attribute(dipl_ties[[7]],"dem",polity$dem_dum[polity$year==2000])
network::set.vertex.attribute(dipl_ties[[8]],"dem",polity$dem_dum[polity$year==2005])


library(btergm)
m2<-btergm(dipl_ties~edges+ istar(2)+ostar(2)+mutual+
             triangle+
             absdiff('dem')+
             nodeicov('dem')+
             edgecov(allies)+
             edgecov(contig)+
             delrecip()+
             memory(type="stability"))
summary(m2)
