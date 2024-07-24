set.seed(45765) #since a simulation involves randomness, set the seed for
#reproducibility.
#Step i--start with a matrix of 73 unconnected nodes.
N=73 #Set the number of nodes:
rnet<-matrix(0, nrow=N,ncol=N)
#Step ii:
p<-243/5256
for (i in 1:N) {
  for (j in 1:N){
    if (i!=j) {
      rnet[i,j]=as.numeric(runif(1)<p)
    }}}


library(statnet)
data(coleman)
coleman<- as.network.matrix(coleman[1,,],	matrix.type='adjacency', directed=TRUE)
rnet<- as.network.matrix(rnet, matrix.type='adjacency',	directed=TRUE)
summary(coleman~ edges+idegree(6)+idegree(7)+ triangles+ mutual+ostar(2)+istar(2))
summary(rnet~edges+idegree(6)+ idegree(7)+triangles+ mutual+ostar(2)+istar(2))

data(sampson)

m3 = ergm(samplike ~ edges + nodematch('group') +
            mutual)

m4 = ergm(samplike ~
            edges + nodematch('group', diff=TRUE) + nodefactor('group')+
            mutual + idegree1.5+ triangles)
mcmc.diagnostics(m4)

summary(m4)


set.seed(6886)

# Define a plotting function:
plotSimNet = function(net, label){
  set.seed(6886)
  plot(net, displaylabels = FALSE,
       vertex.cex = degree(net, cmode = 'indegree')/2, edge.col = "black",
       vertex.col = 'group', coord=p )
  title(label) }

simNets = simulate(m4, nsim = 5)
par(mfrow = c(2, 3))
simNets[[6]] = samplike
labels = c(paste0("sim",1:5), 'actual')
lapply(1:length(simNets), function(i){
  plotSimNet(simNets[[i]], labels[i]) })


set.seed(6886)
gofM4 = gof(
  m4,
  # specify stats to compare against (- indicates remove)
  GOF=~idegree + odegree + espartners + distance - model
)

# we'll compare against four plots, so set up plotting window
par(mfrow = c(2, 2))
plot(gofM4)

AIC(m4, m3)
BIC(m4, m3)


m6 = ergm(samplike ~
            edges + nodematch('group', diff=TRUE) +
            mutual +
            dgwesp(decay = .5,fixed = TRUE, type="OTP")
)
mcmc.diagnostics(m6)
summary(m6)


library(networkdata)
data(legnet)
mynet<-network(el, matrix.type="edgelist",
               directed=TRUE, loops=FALSE)
# Convert the object "edist" which contains euclidean distance (units in lat/long), to a matrix:
edist <- as.matrix(edist)
# Define network attribute
set.network.attribute(mynet,"dist",edist)
set.vertex.attribute(mynet,"ideol",dwnom$dwnom)


