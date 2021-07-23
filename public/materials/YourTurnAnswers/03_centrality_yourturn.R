#Calculate number of friends (degree centrality) for each person in Coleman's data first wave
library(sna)
library(igraph)
data(coleman)
d1 = graph_from_adjacency_matrix(coleman[1,,], mode='directed', diag=FALSE)
deg<-igraph::degree(d1, mode='out', loops=FALSE)
deg<-as.data.frame(deg)
deg$from_id<-as.numeric(row.names(deg))
deg<-deg %>% select(from_id,deg)
deg[1:5,]
