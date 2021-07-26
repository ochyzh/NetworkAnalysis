## Random Networks
## Your Turn 1
#Plot the original and the random networks side by side

#Loading libraries
library(igraph)
library(sna)
library(ggplot2)
data(coleman)

#Transforming the data
coleman<-graph_from_adjacency_matrix(coleman[1,,], mode="directed")
set.seed(4376)
N=73
p=243/5256

#Generating the random network
rnet<-matrix(0,nrow=N,ncol=N)
rnet[1:5,1:5]
for (i in 1:N){
  for (j in 1:N){
    if (i!=j){
      rnet[i,j]=ifelse(runif(1)<p,1,0)
    }
  }
}


#Plotting:
par(mfrow=c(1, 2), mar=c(0,0,1,0))
LO = layout_with_fr(coleman) #Layout
plot(coleman, layout=LO,main="Actual",vertex.label=NA,vertex.size=5, edge.arrow.size=0.2, edge.color="black",vertex.color="turquoise")
plot(g, layout=LO,main="Random",vertex.label=NA,vertex.size=5, edge.arrow.size=0.2, edge.color="black",vertex.color="turquoise")
dev.off()

## Your Turn 2
#Follow the steps from the transitivity example to test whether the level of reciprocity in the friendship network ($r=0.51$) is greater than what you would expect it to be if the network was random. Generate 1000 random networks with the same number of nodes, $N$, and the same probability of any two nodes forming a link, $p$, as those in the observed friendship network. Plot the distribution of the values of reciprocity for these 1000 networks and compare it with the value of $r=0.51$.


#Step i--start with a matrix of 73 unconnected nodes.
N=73 #Set the number of nodes:
p<-243/5256
rec<-rep(NA,1000)

for (k in 1:1000){
  rnet<-matrix(0, nrow=N,ncol=N)
  for (i in 1:N) {
    for (j in 2:N){
      if (i!=j) {
        rnet[i,j]=as.numeric(runif(1)<p)
      }}}
  rec[k]<-reciprocity(graph_from_adjacency_matrix(rnet, mode="directed", weighted=NULL)) #Reciprocity
}





