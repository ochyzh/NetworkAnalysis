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














