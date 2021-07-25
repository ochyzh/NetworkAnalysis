library(igraph)
library(sna)
karate<-make_graph("Zachary")

mf<-max_flow(karate, source=V(karate)[1],
             target=V(karate)[34])
V(karate)$color<-ifelse(V(karate) %in% mf$partition1,
                        "red","blue")
plot(karate)


friends<-graph_from_adjacency_matrix(coleman[2,,],
                    mode="undirected")

friends<-igraph::delete.vertices(friends,which(igraph::degree(friends)==0))
plot(friends)
L0<-layout_with_fr(friends)
cfg<-cluster_fast_greedy(friends)

modularity(cfg)
cfg$membership

plot(cfg, friends, layout=L0)

cw<-cluster_walktrap(friends)
modularity(cw)

par(mfrow=c(1,2))
plot(cfg, friends, layout=L0, main="Greedy")
plot(cw, friends, layout=L0, main="CW")

eb<-cluster_edge_betweenness(friends)
modularity(eb)
par(mfrow=c(1,3))
plot(cfg, friends, layout=L0, main="Greedy")
plot(cw, friends, layout=L0, main="CW")
plot(eb, friends, layout=L0, main="Btw")

