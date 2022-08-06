library(devtools)
install_github("ochyzh/networkdata")
#install.packages("ergm.count")
library(statnet)
library(ergm.count)
library(networkdata)

data(gadeData)
# data characs
actors = sort(unique(c(gadeData$Var1, gadeData$Var2)))
gadeData<-sort(gadeData)
gadeData$coopActions<-round(gadeData$coopActions^2)
#These are the dyadic variables. They
#must be in matrix form.
dyadVars = names(gadeData)[c(3,5:8)]
n = length(actors) ; p = length(dyadVars)

# create empty arr object for all dyad vars
dyadArray = array(0,
                  dim=c(n,n,p),
                  dimnames=list(actors,actors,dyadVars)
)
# loop through and fill in
for(param in dyadVars){
  for(i in 1:nrow(gadeData)){
    a1 = gadeData$Var1[i]
    a2 = gadeData$Var2[i]
    val =gadeData[i,param]
    dyadArray[a1,a2,param] = val
  }
}

# These are node-level variables.
nodeVars = names(gadeData)[9:11]
nodeData = unique(gadeData[,c('Var1',nodeVars)])
rownames(nodeData) = nodeData$Var1
nodeData = nodeData[actors,c(-1)]
# The DV must be a network object
net = as.network(
  dyadArray[,,'coopActions'],
  directed=FALSE, loops=FALSE,
  matrix.type='adjacency',
  ignore.eval = FALSE,
  names.eval = "coopActions"
)

# Set node attributes
for(param in nodeVars){
  network::set.vertex.attribute(net, param, nodeData[,param])
}

# Set network attributes:
set.network.attribute(net,'loc.dyad',dyadArray[,,'loc.dyad'])
set.network.attribute(net,'spons.dyad',dyadArray[,,'spons.dyad'])
# We can view the attribute as a sociomatrix.
as.matrix(net, attrname = "coopActions")[1:10, 1:10]

m2 <- ergm(net ~ sum +
             nodecov('averageId.node') +
             nodecov('size.node') +
             nodecov('spons_actor.node') +
             absdiff('averageId.node') +
             absdiff('size.node') +
             edgecov('loc.dyad') +
             edgecov('spons.dyad')+
             transitiveweights("min","max","min")+
             nodecovar(transform="sqrt"),
           response = "coopActions", reference = ~Poisson)


# Simulate from model fit:
simNets <- simulate(m2, nsim = 3)




# add actual network to list of sim nets
# for comparison
simNets[[4]] = net
labels = c(paste0("sim",1:3), 'actual')
lapply(1:length(simNets), function(i){
  plotSimNet(simNets[[i]], labels[i]) })

#Plot the original network to get the layout:
set.seed(6886)
p<-plot(net, edge.col = "black", usecurve = TRUE,
        edge.curve = 0, edge.lwd=.25*dyadArray[,,"coopActions"],
        displaylabels = TRUE)

# Define a plotting function:
plotSimNet = function(net, label){
  set.seed(6886)
  plot(net, edge.col = "black", usecurve = TRUE,
       edge.curve = 0, edge.lwd=.1*dyadArray[,,"coopActions"],
       displaylabels = TRUE, coord=p)
  title(label) }

par(mfrow = c(2, 2))
# add actual network to list of sim nets
# for comparison
simNets[[4]] = net
labels = c(paste0("sim",1:3), 'actual')
lapply(1:length(simNets), function(i){
  plotSimNet(simNets[[i]], labels[i]) })

# Simulate from model fit:
simNets1000 <- simulate(m2, monitor = ~ nodecovar(transform="sqrt"),
                        nsim = 1000, output = "stats")

#How prevalent are k-stars in the observed network?
obsNet<-summary(net~sum+transitiveweights("min","max","min")+nodecovar(transform="sqrt"),
                response = "coopActions")
par(mfrow = c(2, 2))
#1st col.=sum
plot(density(simNets1000[,1]), main="")
abline(v = obsNet[1])
title("Sum")

# 9th col. = transitiveweights
plot(density(simNets1000[,9]), main="")
abline(v = obsNet[2])
title("Transitive Weights")

# 10th col. = nodesqrtcovar
plot(density(simNets1000[,10]), main="")
abline(v = obsNet)
title("Nodesqrtvar")

#How prevalent are k-stars in the observed network?
obsNet<-summary(net~sum+transitiveweights("min","max","min")+nodecovar(transform="sqrt"), response = "coopActions")

par(mfrow = c(2, 2))
#1st col.=sum
plot(density(simNets1000[,1]), main="")
abline(v = obsNet[1])
title("Sum")

# 9th col. = transitiveweights
plot(density(simNets1000[,9]), main="")
abline(v = obsNet[2])
title("Transitive Weights")

# 10th col. = nodesqrtcovar
plot(density(simNets1000[,10]), main="")
abline(v = obsNet[])
title("Nodesqrtvar")

