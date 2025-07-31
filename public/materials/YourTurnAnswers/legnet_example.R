library(tidyverse)
library(statnet)
library(devtools)
#install_github("ochyzh/networkdata")
library(networkdata)

data(legnet)

mynet<-network(el, matrix.type="edgelist",
               directed=TRUE, loops=FALSE)
net<-as.matrix(mynet)
net<-net[order(rownames(net)),order(colnames(net))]
mynet<-network(net, matrix.type="adjacency",
               directed=TRUE, loops=FALSE)

mynet %v% "vertex.names"
colnames(edist)
edist<-edist[-4,-4]

dwnom<-dwnom[-4,]
# Convert the object "edist" which contains euclidean distance (units in lat/long), to a matrix:
edist <- as.matrix(edist)
# Define network attribute
set.network.attribute(mynet,"dist",edist, vertex.pid=rownames(edist))

# Define object "dwnom" (ideology) as a vertex attribute
#detach("package:igraph", unload=TRUE) the below command seems to clash with igraph
set.vertex.attribute(mynet,"ideol",dwnom$dwnom, , vertex.pid=dwnom$labs)

#with igraph
library(igraph)
g<- as.matrix(mynet)
g<- graph_from_adjacency_matrix(g, mode='directed', diag=FALSE)
LO<- layout_with_fr(g)
plot(g,edge.arrow.size=1, vertex.color='blue', vertex.label=NA)

#with network
network::plot.network(mynet, vertex.cex=5)

m1<-ergm(mynet~edges)
summary(m1)
m2<-ergm(mynet~edges+
               absdiff("ideol")+edgecov("dist"))
summary(m2)





