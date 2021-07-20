#install.packages("devtools")
#install.packages("igraph")
library(devtools)
install_github("ochyzh/networkdata")
library(networkdata) #course-specific data package
library(igraph)


data("highlandPonies")
ponies<-as.matrix(highlandPonies[1:17,2:18])

#Convert matrix into an igraph object:
pGraph<-graph_from_adjacency_matrix(ponies,
          weighted=TRUE, mode="undirected", diag=FALSE )

plot(pGraph, vertex.color="turquoise", vertex.size=25)

#A nicer plot:
V(pGraph)$color <- ifelse(V(pGraph)$name %in% c("WT", "WH", "WS"), "azure1", "turquoise")
ponyPlot<- plot(pGraph,
                edge.arrow.size=.2,
                edge.color="black",
                vertex.frame.color="black",
                vertex.label=V(pGraph)$names,
                vertex.label.color="black", layout=layout_with_fr,
                edge.width=E(pGraph)$weight/5, edge.curved=.08)
