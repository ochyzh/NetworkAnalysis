library(igraph) #Zachary's karate club dataset is built into the igraph package.
karate <- make_graph("Zachary") 
mf<-max_flow(karate, source=V(karate)[1], target=V(karate)[34])
V(karate)$color<- ifelse(V(karate) %in%  mf$partition1, 
                         "red","pink")
plot(karate, edge.color="black", vertex.frame.color="black")
mf$partition1
mf$partition2

library(sna)
data(coleman)
#make into an igraph object
friends<-graph_from_adjacency_matrix(coleman[2,,], mode="undirected", diag=FALSE)
friends <- igraph::delete.vertices(friends , which(igraph::degree(friends)==0))
LO = layout_with_fr(friends) #Layout
cfg<-cluster_fast_greedy(friends)
modularity(cfg)


wc <- cluster_walktrap(friends) #community structure via short random walks
modularity(wc)

emailnet<-read.csv("emailnet.csv")
employees<-read.csv("EmployeeRecords.csv")

email1 <-emailnet |> dplyr::filter(day==6)

g<-graph_from_data_frame(email1, directed="TRUE")
plot(g, vertex.label=NA)

E(g)$Subject





