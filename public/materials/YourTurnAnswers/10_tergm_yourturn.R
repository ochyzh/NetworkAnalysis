#Your Turn

for (i in 1:10) {
  war[[i]]<-network(war[[i]])
  #set.network.attribute(war[[i]],'contiguity',contiguity)
}
class(war[[1]])
WarDyn = networkDynamic(network.list=war)

tErgmStats(WarDyn, "~ edges+triangle")

par(mfrow = c(1,2))
p<-plot(
  network.extract(WarDyn, at = 0),
  main = "1991", displaylabels = T)
plot(
  network.extract(WarDyn, at = 9),
  main = "2000", displaylabels = T,coord=p)

library(ndtv)
render.d3movie(WarDyn,
               plot.par=list(displaylabels=T),filename="WarNetwork.html", launchBrowser=FALSE )
