library(igraph)
library(sna)
data(coleman) #Use friendship data
coleman<-coleman[1,,]
#convert to an -igraph- object, we'll treat it as a directed graph for now:
coleman<-graph_from_adjacency_matrix(coleman, mode="directed", diag=FALSE) 
edge_density(coleman)

plot(coleman)

library(statnet)
data('sampson')
samplike
network::get.vertex.attribute(samplike, 'group')
samplike %v% 'group' # returns the same result as above

vertexSize = degree(samplike, cmode = 'indegree')/2
p<-plot(samplike,
        displaylabels = TRUE,
        # size of nodes based on vector vertexSize
        vertex.cex = vertexSize,
        # color of nodes based on vertex attribute: group
        vertex.col = 'group'
)


m1 = ergm(samplike ~ edges)
summary(m1)

plogis(-0.9072)

network.density(samplike)


