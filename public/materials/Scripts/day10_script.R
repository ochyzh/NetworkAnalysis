library(ggplot2)
library(tidyverse)

ggplot()+geom_line(aes(x=seq(1:10), y=seq(1:10)))+
  geom_line(aes(x=seq(1:10), y=seq(1:10)*sqrt(1:10)))


library(statnet)
library(devtools)
#install_github("ochyzh/networkdata")
library(networkdata)

data(legnet)
el<- el |> arrange(V1, V2)
mynet<-network(el, matrix.type="edgelist",
               directed=TRUE, loops=FALSE)
# Convert the object "edist" which contains euclidean distance (units in lat/long), to a matrix:
edist<- as.data.frame(edist) |> dplyr::select(-"BarkleyDeanM") |> dplyr::filter(row.names(edist)!="BarkleyDeanM")
edist <- as.matrix(edist)
# Define network attribute
set.network.attribute(mynet,"dist",edist)
get.network.attribute(mynet,"dist")
get.network.attribute(mynet,"vertex.names")

dwnom<- dwnom |> arrange(labs) |> dplyr::filter(labs!="BarkleyDeanM")
set.vertex.attribute(mynet,"ideol",dwnom$dwnom)

set.vertex.attribute(mynet,"ideol",dwnom$dwnom,
      v=match(mynet %v% "vertex.names",dwnom$labs))
get.vertex.attribute(mynet,"ideol")
get.vertex.attribute(mynet,"vertex.names")
