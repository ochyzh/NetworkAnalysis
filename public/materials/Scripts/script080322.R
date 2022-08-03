library(statnet)
library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)
data(legnet)
mynet<-network(el, matrix.type="edgelist", 
               directed=TRUE, loops=FALSE)

class(mynet)

# Convert the object "edist" which contains euclidean distance (units in lat/long), to a matrix:
edist <- as.matrix(edist)
# Define network attribute
set.network.attribute(mynet,"dist",edist)


# Define object "dwnom" (ideology) as a vertex attribute
#detach("package:igraph", unload=TRUE) the below command seems to clash with igraph
set.vertex.attribute(mynet,"ideol",dwnom$dwnom)
dev.off()
plot(mynet,displaylabels = TRUE,
     # size of nodes based on vector vertexSize
     vertex.cex = degree(mynet, cmode = 'indegree')/2)

m1<-ergm(mynet ~ edges+ edgecov('dist')+ absdiff('ideol'))


gofM1 = gof(
  m1, 
  # specify stats to compare against (- indicates remove)
  GOF=~idegree + odegree + espartners + distance - model
)
# we'll compare against four plots, so set up plotting window
par(mfrow = c(2, 2))
plot(gofM1)

m2<-ergm(mynet ~ edges+ edgecov('dist')+ absdiff('ideol')+istar(2))
mcmc.diagnostics(m2)

m3<-ergm(mynet ~ edges+ edgecov('dist')+ absdiff('ideol')+istar(2)+ triangle)

m4<-ergm(mynet ~ edges+ edgecov('dist')+ absdiff('ideol')+istar(2)+ 
           gwesp(decay=.5, fixed=TRUE))

m5 = ergm(mynet ~ edges+ edgecov('dist')+ absdiff('ideol')+istar(2)+ gwesp(decay=.5, fixed=TRUE), 
         control=control.ergm(
           seed=6886,
           MCMC.samplesize=10000
         )
)




#Gade examle
data(gadeData)
gadeData

table(gadeData$coopActions) #the dv takes the following values.
hist(gadeData$coopActions)
gadeData$coopBin<-as.numeric(gadeData$coopActions>0)
table(gadeData$coopBin)

# data characs
actors = sort(unique(c(gadeData$Var1, gadeData$Var2)))
gadeData<-sort(gadeData)
#These are the dyadic variables. They
#must be in matrix form.
dyadVars = names(gadeData)[c(12,5:8)]
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
    val = gadeData[i,param]
    dyadArray[a1,a2,param] = val
  }
}

dyadArray[,,2]

nodeVars = names(gadeData)[9:11]
nodeData = unique(gadeData[,c('Var1',nodeVars)])
rownames(nodeData) = nodeData$Var1
nodeData = nodeData[actors,c(-1)]

