install.packages("statnet")
library(statnet)

data(sampson)
samplike

adjMat<-as.matrix.network(samplike)

as.network.matrix(adjMat, matrix.type = "adjacency",
                  directed=TRUE)

network::get.vertex.attribute(samplike, 'group')

vertexSize<-degree(samplike, cmode="indegree")
plot(samplike, vertex.cex=vertexSize/2, vertex.col='group')

m1<-ergm(samplike~edges)
summary(m1)

plogis(coef(m1)[['edges']])

m2<-ergm(samplike~edges+nodematch('group'))
summary(m2)

plogis(coef(m2)[['edges']])
