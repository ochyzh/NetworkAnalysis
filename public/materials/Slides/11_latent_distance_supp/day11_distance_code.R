# day 5
# LSM Examples #
# Polity Homophily

# set path, or in this case working directory
setwd("~/Teaching/icpsr/nets/datasets/")

### datasets
# Read in the Alliances data
# Getting Network Data into R - flexible use of CSV
# # Read in vertex dataset
# allyV = read.csv("allyVLD.csv",stringsAsFactors=F)

# # Read in edgelist
# allyEL = read.csv("allyEL.csv", stringsAsFactors=F)

# # Read in contiguity
# contig = read.csv("contiguity.csv",stringsAsFactors=F,row.names=1)
# contig = as.matrix(contig)

# or just load in day5.rda
load('day11_distance.rda')
###########

###########
# packages
library(network)
library(latentnet)
library(ergm)
library(sna)
###########

head(allyV)
head(allyEL)

###########
# Using the network package to get data ready

# (1) Initialize network
AllyNet = network.initialize(nrow(allyV),dir=F)

# (3) Set vertex labels
network.vertex.names(AllyNet)  = allyV$stateabb

# (4) Add in the edges
# Note, edgelist must match vertex labels
AllyNet[as.matrix(allyEL)]  = 1

# (5.1) Store attributes
set.vertex.attribute(AllyNet,"created",allyV$styear)
set.vertex.attribute(AllyNet,"polity",allyV$polity/sd(allyV$polity))

# (5.2) Store network attribute
set.network.attribute(AllyNet,"contiguous",as.matrix(contig))
###########

###########
# Estimate null model and model with covariates

# fit model with just latent effects
set.seed(6886)
# these have already been run for you 
# and saved in the day5.rda file
# est2d = ergmm(
#     AllyNet~euclidean(d=2),
#     control=ergmm.control(
#         sample.size=5000,interval=10)
#         )
mcmc.diagnostics(est2d)
summary(est2d)

set.seed(6886)
# Construct inverse contig distance
igdist = 1/(geodist(contig)$gdist+1)
# polity distance
polDist = as.matrix(dist(allyV$polity,upper=T,diag=T))

# Run the model with covariates
# est2dc = ergmm(
#     AllyNet~euclidean(d=2)+edgecov(contig)+edgecov(igdist)+nodecov("polity")+edgecov(polDist),
#     control=ergmm.control(sample.size=5000,interval=10))
mcmc.diagnostics(est2dc)
summary(est2dc)
###########

###########
# compare goodness of fit
par(mfrow=c(2,3))
plot(gof(est2d,GOF=~degree+distance+esp))

#par(mfrow=c(2,2))
plot(gof(est2dc,GOF=~degree+distance+esp))

est8d = ergmm(
    AllyNet~euclidean(d=8),
    control=ergmm.control(
        sample.size=5000,interval=10)
        )

par(mfrow=c(2,3))
plot(gof(est2d,GOF=~degree+distance+esp))

#par(mfrow=c(2,2))
plot(gof(est8d,GOF=~degree+distance+esp))

# how do we this simulation process on our own

# simulate some data data from our model
preds = simulate(
  est2d, nsim=1000
)

# each simulation will give me a predicted
# network and for each of these networks
# i want to calculate the level of
# actor heterogeneithy
gof_stats <- lapply(
  preds$'networks', function(x){
    adj = as.sociomatrix(x)
    diag(adj) = NA
    actorSums = rowSums(adj, na.rm=TRUE)
    gofStat = sd(actorSums)
    return(gofStat)
  })
gof_stats = unlist(gof_stats)

# compare with the level of actor 
# heterogeneity in my true dataset
net_orig = as.sociomatrix(AllyNet)
diag(net_orig) = NA
actorSums_net_orig = rowSums(net_orig, na.rm=TRUE)
gofStat_net_orig = sd(actorSums_net_orig)

summary(gof_stats)
###########

###########
# Extract the best guess at the positions
# of countries in the latent space
Z = est2d$mkl$Z

head(Z)

# generate plot
pdf(file="alliances.pdf",height=5,width=5.5,pointsize=10)
par(mar=c(3,3,.1,4),xpd=TRUE)
gplot(AllyNet,gmode="graph",coord=Z,label=network.vertex.names(AllyNet),label.cex=.4,displayisolates=F, edge.col=rgb(100,100,100,maxColorValue=250,alpha=50))
axis(1)
axis(2)
dev.off()
###########