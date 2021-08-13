library(tidyverse)
library(magrittr)

library(amen)
library(network)
library(sna)
library(ergm)
data(sampson)

class(samplike)

grp<-cbind.data.frame(name=samplike %v% "vertex.names",group=samplike %v% "group")

Y<-as.matrix(samplike) %>% as.data.frame()

Y %>% mutate(name1=row.names(.)) %>%
  pivot_longer(-name1, names_to="name2", values_to="Y") %>%
  left_join(grp, by=c("name1"="name"))%>%
  left_join(grp, by=c("name2"="name")) %>%
  mutate(nodematch=as.numeric(group.x==group.y)) %>%
  select(name1, name2, nodematch) %>%
  pivot_wider(names_from=name2, values_from = nodematch) %>%
  as.data.frame() -> nodematch

nodematch<-nodematch[,-1]

lfmFit = ame(Y, Xdyad=as.matrix(nodematch),
             family='bin', symmetric=FALSE,
             seed=6886,
             # restrict SRM parameters
             cvar=TRUE, rvar=TRUE, dcor=TRUE,
             R=2,
             plot=FALSE, print=FALSE
)

