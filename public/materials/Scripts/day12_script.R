library(statnet)
library(latentnet)
library(devtools)
install_github("ochyzh/newtorkdata")
library(networkdata)

data("Kirkland2012")
AMnet01
as.matrix(AMnet01)[1:10,1:10]

class(same.dist01)
same.dist01[is.na(same.dist01)]<-0
same.dist01[1:5,1:5]

class(same.party01)
same.party01[1:5,1:5]

set.network.attribute(AMnet01,"samedist",same.dist01)
set.network.attribute(AMnet01,"sameparty",same.party01)

interact01[1:5,1:5]
interact01(is.na(interact01))<-0
set.network.attribute(AMnet01,"interact01",interact01)

#Treating the network as binary:
m1<-ergmm(AMnet01~edgecov("sameparty")+
            edgecov("samedist")+
            edgecov("interact01")+euclidean(d=2))
mcmc.diagnostics(m1)

as.matrix(AMnet01, attrname = "values")[1:10,1:10]

m2<-ergmm(AMnet01~edgecov("sameparty")+
            edgecov("samedist")+
            edgecov("interact01")+euclidean(d=2), family="Poisson",
          response="values")
mcmc.diagnostics(m2)

zPos<-summary(m1)$'pmean'$Z
head(zPos)

library(ggplot2)
library(tidyverse)
library(magrittr)

ggplot()+geom_point(aes(x=zPos[,1], y=zPos[,2]))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+xlab("X-coordinate")+
  ylab("Y-coordinate")

distype<-NC01 %>% group_by(dist) %>%
  summarise(distype=length(dist)) %>%
  ungroup() %>% mutate(distype=ifelse(distype==1,0,1))

NC01<-left_join(NC01, distype, by="dist")

ggplot()+geom_point(aes(x=zPos[,1], y=zPos[,2],
                        size=as.factor(NC01$distype)))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+xlab("X-coordinate")+
  ylab("Y-coordinate")

data("Dyadic_COW_4.0")
class(TRADE)

tradenet<-network(as.matrix(TRADE),directed=TRUE,
                  matrix.type="adjacency",ignore.eval=FALSE,
                  names.eval="volumes")

as.matrix(tradenet, attrname = "volumes")[1:10,1:10]

network::list.edge.attributes(tradenet)

y.var=4*sd(as.matrix(TRADE),na.rm=TRUE)

m1<-ergmm(tradenet~euclidean(d=2),
          family="normal",
          response="volumes",
          fam.par=list(prior.var=y.var,
                       prior.var.df=1))

summary(m1)

mcmc.diagnostics(m1)
saveRDS(m1, "data/m1.rds")
m1<-readRDS("data/m1.rds")




