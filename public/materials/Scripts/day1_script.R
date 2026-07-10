install.packages("devtools")
library(devtools)
pak::pak("ochyzh/networkdata")
library(networkdata)
library(igraph)
data(highlandPonies)



ponies<-as.matrix(highlandPonies[1:17, 2:18])
ponies[is.na(ponies)] <- 0 # Replace NAs with 0

pGraph <- graph_from_adjacency_matrix(ponies, weighted=TRUE,
                                      mode="directed",
                                      diag=FALSE)

V(pGraph)$color <- ifelse(V(pGraph)$name %in% c("WT", "WH", "WS"), "azure1", "turquoise")
ponyPlot<- plot(pGraph,
                edge.arrow.size=.2,
                edge.color="black",
                vertex.frame.color="black",
                vertex.label=V(pGraph)$names,
                vertex.label.color="black", layout=layout_with_fr,
                edge.width=E(pGraph)$weight/5, edge.curved=.08)

