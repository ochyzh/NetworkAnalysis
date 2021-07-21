library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)
library(tidyverse)
library(magrittr)
library(sna)
library(igraph)


data("defAlly")
defMat<-defAlly %>%
  filter(year==2012) %>%
  select(ccode1, ccode2, defAlly) %>%
  pivot_wider(names_from=ccode2, values_from=defAlly) %>%
  select(c(ccode1,`2`), everything())

mynames<-defMat$ccode1
rownames(defMat)<-defMat$ccode1

defMat<-defMat[,-1]
rownames(defMat)<-mynames


data(flo)
g<-graph_from_adjacency_matrix(flo,mode="undirected",
                               diag=FALSE)
plot(g)

cDegree<-degree(g)
L0<-layout_with_fr(g)
plot(g, vertex.size=5*cDegree, layout=L0)

CloseCent<-closeness(g)

par(mfrow=c(1,2))
plot(g, vertex.size=5*cDegree, layout=L0)
plot(g, vertex.size=1000*CloseCent, layout=L0)
