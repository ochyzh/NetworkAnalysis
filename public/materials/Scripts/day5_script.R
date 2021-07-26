library(igraph)
library(sna)
library(ggplot2)
data(coleman)

coleman<-graph_from_adjacency_matrix(coleman[1,,], mode="directed")
transitivity(coleman)

sna::gtrans(as.matrix(get.adjacency(coleman)))

set.seed(4376)
N=73
p=243/5256

rnet<-matrix(0,nrow=N,ncol=N)
rnet[1:5,1:5]
for (i in 1:N){
  for (j in 1:N){
    if (i!=j){
      rnet[i,j]=ifelse(runif(1)<p,1,0)
    }
  }
}

rnet[1:5,1:5]

trans<-sna::gtrans(rnet)

par(mfrow=c(1,2))
L0<-layout_with_fr(coleman)
plot(coleman, layout=L0, main="Actual", vertex.size=.2,
     edge.arrow.size=.2, vertex.label=NA)
g<-graph_from_adjacency_matrix(rnet, mode="directed")
plot(g, layout=L0, main="Random", vertex.size=.2,
     edge.arrow.size=.2, vertex.label=NA)


trans1000<-rep(NA,1000)

for (k in 1:1000) {
rnet<-matrix(0,nrow=N,ncol=N)
rnet[1:5,1:5]
for (i in 1:N){
  for (j in 1:N){
    if (i!=j){
      rnet[i,j]=ifelse(runif(1)<p,1,0)
    }
  }
}
trans1000[k]<-sna::gtrans(rnet)
}

qplot(x=trans1000, geom="density")+
  geom_vline(aes(xintercept=.46))+
  scale_x_continuous("Transitivity",limits=c(0,.5))

reciprocity(coleman)
