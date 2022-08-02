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

m2 = ergm(samplike ~ edges + nodematch('group'))
m2
plogis(-2.001+2.648)
plogis(-2.001)

m3 = ergm(samplike ~ edges + nodematch('group') +
            mutual)
m3
plogis(-2.234+1.969+1.446)
plogis(-2.234+1.969)

library(sna)
data(coleman) #Use friendship data
coleman<-coleman[1,,]

coleman<-as.network.matrix(coleman)
m1<-ergm(coleman~edges +mutual)
m1

plogis(-3.722 +
         3.764 )

mcmc.diagnostics(m1)
m4 = ergm(coleman~edges +mutual,
         control=control.ergm(
           seed=6886,
           MCMC.samplesize=10000
         )
)
mcmc.diagnostics(m4)

set.seed(6886)
simNets = simulate(m3, nsim = 5)
# Define a plotting function:
plotSimNet = function(net, label){
  set.seed(6886)
  plot(net, displaylabels = FALSE, vertex.cex = degree(net, cmode = 'indegree')/2, edge.col = "black",
       vertex.col = 'group', coord=p )
  title(label) }
par(mfrow = c(2, 3))
# add actual network to list of sim nets
# for comparison
simNets[[6]] = samplike
labels = c(paste0("sim",1:5), 'actual')
lapply(1:length(simNets), function(i){
plotSimNet(simNets[[i]], labels[i]) })


m5 = ergm(samplike ~
            edges + nodematch('group') +
            mutual + idegree1.5 +
            triangles
)

mcmc.diagnostics(m5)
summary(m5)



library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)
data(legnet)
mynet<-network(el, matrix.type="edgelist",
               directed=TRUE, loops=FALSE)
