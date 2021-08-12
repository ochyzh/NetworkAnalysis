library(amen)
library(network)
data(sampson)

Y<-as.matrix(samplike) %>% as.data.frame()

#Need to construct nodematch:
grps<-cbind.data.frame(name=samplike %v% "vertex.names",group=samplike %v% "group")
as.matrix(samplike) %>% as.data.frame() %>% mutate(name1=row.names(.)) %>%
  pivot_longer(cols=-name1, names_to="name2", values_to="Y")  %>% left_join(grps, by=c("name1"="name")) %>% mutate(group1=group, .keep="unused") %>% left_join(grps, by=c("name2"="name")) %>% mutate(group2=group,  nodematch=as.numeric(group1==group2)) %>% select(name1, name2, nodematch) %>%
  pivot_wider(names_from="name2", values_from="nodematch") %>% as.data.frame()->nodematch
row.names(nodematch)<-nodematch[,1]
nodematch<-nodematch[,-1]

lfmFit = ame(Y, Xdyad=as.matrix(nodematch),
             family='bin', symmetric=FALSE,
             seed=6886,
             # restrict SRM parameters
             cvar=TRUE, rvar=TRUE, dcor=TRUE,
             R=2,
             plot=FALSE, print=FALSE
)


summary(lfmFit)
saveRDS(lfmFit, "data/lfmFit_sampson.rds")
