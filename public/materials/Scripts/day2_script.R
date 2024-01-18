install.packages("network")
library(network)
data(flo)

library(igraph)
g = graph_from_adjacency_matrix(flo, mode='undirected', diag=FALSE)
plot(g)

cDegree<-degree(g,  loops=FALSE)
plot(g, vertex.size=5*cDegree)
d = distances(g)

sort(degree(g,  loops=FALSE), decreasing=TRUE)
sort(closeness(g), decreasing=TRUE)
sort(betweenness(g), decreasing=TRUE)
sort(eigen_centrality(g)$vector, decreasing=TRUE)

install.packages("devtools")
library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)
data(defAlly)
library(tidyverse)
#Get unique actors:
cntries<-unique(defAlly$ccode1)

defMat<- defAlly |>
  dplyr::select(ccode1, ccode2, defAlly) |>
  pivot_wider(names_from="ccode2", values_from="defAlly") |>
  dplyr::select(ccode1, "2", everything()) |>
  as.matrix()

rownames(defMat)<-cntries
defMat<- defMat[,-1]
defMat[is.na(defMat)]<-0
diag(defMat)<- NA












