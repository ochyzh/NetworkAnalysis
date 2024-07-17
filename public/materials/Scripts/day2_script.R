library(igraph)
library(network)
data(flo) #this dataset is available from the -network- package
flo[1:5,1:5]


g = graph_from_adjacency_matrix(flo, mode='undirected', diag=FALSE)
g<- delete_vertices(g, V(g)[degree(g) == 0])
plot(g)

sort(igraph::degree(g), decreasing=TRUE)[1:6]
sort(igraph::closeness(g), decreasing=TRUE)[1:6]
sort(igraph::betweenness(g), decreasing=TRUE)[1:6]
sort(igraph::eigen_centrality(g)$vector, decreasing=TRUE)[1:6]

library(igraph) #Zachary's karate club dataset is built into the igraph package.
karate <- make_graph("Zachary") #loads the data from the igraph package
mf<-max_flow(karate, source=V(karate)[1],
             target=V(karate)[34])
V(karate)$color<- ifelse(V(karate) %in%
                           mf$partition1, "red","pink")
plot(karate, edge.color="black", vertex.frame.color="black")
mf$partition1
mf$partition2

library(sna)
data(coleman)
#make into an igraph object
friends<-graph_from_adjacency_matrix(coleman[2,,],
                    mode="undirected", diag=FALSE)
friends <- igraph::delete_vertices(friends ,
                  which(igraph::degree(friends)==0))
LO = layout_with_fr(friends) #Layout
cfg<-cluster_fast_greedy(friends)
modularity(cfg)

cfg$membership
plot(cfg, friends, layout=LO, main="Greedy")


wc <- cluster_walktrap(friends) #community structure via short random walks
modularity(wc)

ceb<-cluster_edge_betweenness(friends)
modularity(ceb)






