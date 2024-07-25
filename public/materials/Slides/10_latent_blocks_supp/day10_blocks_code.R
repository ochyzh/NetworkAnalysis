########################
# Day 4
# community detection and 
# blockmodels 
########################

####
# path setup
pth = '~/Teaching/icpsr/nets/datasets/'
####

####
# packages
library(scales)
library(reshape2)
library(igraph)
library(blockmodels)
library(RColorBrewer)
####

####
## datasets
load(paste0(pth, 'day10_blocks.rda'))
####

# we'll start by working with some data from 
# a fun journal of conflict resolution paper
# by emily gade et al
# mainly taking a tangent away from the trade example
# because many of these clustering algos struggle
# with weighted networks
# https://journals.sagepub.com/doi/pdf/10.1177/0022002719826234
dim(gadeData)
gadeData[1:5,1:5]

# to get our first set of communities we'll use helper
# functions from the igraph package

# lets start with a measure of communities based on 
# edge betweenness
# convert Y to graph object
g = graph_from_adjacency_matrix(
  gadeData,
  mode='undirected', 
  weighted=NULL, 
  diag=FALSE )
ebComm = cluster_edge_betweenness(g)

# who got assigned where? 
membership(ebComm)

# we can also visualize the results
plot(
  ebComm, 
  g
  )

# to understand how well this clustering algo
# did we can use a modularity score
# modularity scores ranges from -1 to 1, 
# where higher values indicate stronger 
# community structure 
ebCommScore = modularity(ebComm, g)
ebCommScore

# there are lots of other approaches, another
# is the leading eigenvector approach from
# newman
leComm = cluster_leading_eigen(g)
leCommScore = modularity(leComm, g)
leCommScore

####
# blockmodels

# lets go back to our trade data
# first construct our dv

# org dv
yMat = acast(trade, Var1 ~ Var2, value.var='trade')

# lets do a sbm in which we partial out the effect of distance
partMod = lm(trade ~ distance, data=trade)
trade$trade_noDist = partMod$residuals

# lets make ymat the residuals instead
yMat = acast(trade, Var1 ~ Var2, value.var='trade_noDist')

# the blockmodels function doesnt like NAs anywhere
# in the matrix, so we need to set even our diagonals
# to zero
diag(yMat) = 0

# gaussian stochastic blockmodel
set.seed(6886)
sbm = BM_gaussian('SBM', yMat)

# to estimate model run
sbm$estimate()

# output here is a bit different
sbm$show()

# lets pull out the community assignments
mem = sbm$memberships[[5]]$Z
memCat = apply(mem, 1, function(x){which(max(x)==x)})
memCat

# create graph object
diag(yMat) = NA
yGraph = igraph::graph.adjacency(yMat, 
                                 mode='directed', 
                                 weighted=TRUE, 
                                 diag=FALSE
)

# add node attributes
V(yGraph)$size = rescale(
  apply(yMat, 2, sum, na.rm=TRUE), c(10, 16) )

# Colors
cols = brewer.pal(5, 'Set1')
nodeColors = cols[memCat]

# we want to layout our nodes, so that
# they are somewhat reflective of community
# assignments, here's a helper function
# that tries to accomplish this
commPlotHelper = function(m, graph){
  el <- igraph::as_edgelist(graph, names = FALSE)
  m1 <- m[el[, 1]]
  m2 <- m[el[, 2]]
  res <- m1 != m2
  if (!is.null(names(m1))) {
    names(res) <- paste(names(m1), names(m2), sep = "|")
  }
  return(res) }

weights = commPlotHelper(memCat, yGraph)
set.seed(6886)
commLayout = layout_with_fr(yGraph, weights=weights+1)

# and putting the pieces together
plot(yGraph, 
     layout=commLayout,
     vertex.color=cols[memCat], 
     vertex.label.color='white',
     vertex.size=V(yGraph)$size,
     vertex.label.cex =.75,
     edge.color='grey20',
     edge.width=E(yGraph)$weight,
     edge.arrow.size=.2,
     asp=FALSE
)

# lets add in some dyadic covariates 
# sbm package
library(sbm)
library(reshape2)

# starting data
head(trade)

# construct y
yMat = acast(
  data=trade,
  Var1 ~ Var2, 
  value.var='trade'
)

# construct our dyadic array
idVars = c('Var1', 'Var2')
dVars = c('conflicts', 'distance', 'shared_igos')
tLong = melt(
  trade[,c(idVars, dVars)], id=idVars)
dyadArr = acast(
  data=tLong, 
  formula=Var1 ~ Var2 ~ variable, 
  value.var='value'
)

# 
dyadList = list()
dyadList[[1]] = dyadArr[,,'conflicts']
dyadList[[2]] = dyadArr[,,'distance']
dyadList[[3]] = dyadArr[,,'shared_igos']

# lets run a simple sbm
sbm = estimateSimpleSBM(
  yMat,
  model = "gaussian",
  directed = TRUE,
  dimLabels = c(node = "country"),
  covariates = dyadList[3] )
### well that was a fail, we tried every dtadic
# var and their algo kept crashing, booo
# sbm, this will happen to you :(

# lets try one other package that 
# allows us to accomodate dyadic covariates
library(NetMix)

# example code
## Load datasets
data("lazega_dyadic")
data("lazega_monadic")

#
head(lazega_dyadic)
head(lazega_monadic)

## Estimate model with 2 groups
## Setting to `hessian=TRUE` increases computation time
## but is needed if standard errors are to be computed.
lazega_mmsbm <- mmsbm(
  SocializeWith ~ Coworkers,
  ~ School + Practice + Status,
  senderID = "Lawyer1",
  receiverID = "Lawyer2",
  nodeID = "Lawyer",
  data.dyad = lazega_dyadic,
  data.monad = lazega_monadic,
  n.blocks = 2,
  mmsbm.control = list(
    seed = 123,
    conv_tol = 1e-2,
    hessian = FALSE))

#
summary(lazega_mmsbm)

#
plot(lazega_mmsbm)
