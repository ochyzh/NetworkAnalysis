library(tidyverse)
library(igraph)
email<-read.csv("emailnet.csv")
email_Jan6<- email |> dplyr::filter(day==6 & to!=From) 
#This is a directed network, but make undirected to detect communities:
g = igraph::graph_from_data_frame(email_Jan6, directed=FALSE)
#Make edges unweighted
g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
plot(g_simple, vertex.label=NA, vertex.size=5,edge.arrow.size=.3)
cfg<-cluster_fast_greedy(g_simple)
plot(cfg, g_simple, main="Greedy", vertex.label=NA)
LO<- layout_with_fr(g_simple)

walktrap<-cluster_walktrap(g_simple)
between<-cluster_edge_betweenness(g_simple)
greedy<-cluster_fast_greedy(g_simple)

modularity(walktrap)
modularity(between)
modularity(ceb)


par(mfrow=c(2,2),mar=c(0,0,1,0))
plot(walktrap, g_simple, main="Short Random Walks", vertex.label=NA, layout=LO)
plot(between, g_simple, main="Betweeness", vertex.label=NA, layout=LO)
plot(greedy, g_simple, main="Greedy", vertex.label=NA, layout=LO)