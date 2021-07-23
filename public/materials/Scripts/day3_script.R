library(sna)
library(igraph)
data(coleman)
library(tidyverse)
library(magrittr)

pGraph<-graph_from_adjacency_matrix(coleman[1,,],
                                    mode="undirected")

plot(pGraph, vertex.label=NA, vertex.size=10)

deg<-igraph::degree(pGraph)

deg<-as.data.frame(deg)
deg$from_id<-as.numeric(row.names(deg))

fdeg<-coleman[1,,] %>%
  as.data.frame() %>%
  mutate(from_id=as.numeric(row.names(coleman[1,,]))) %>%
  pivot_longer(cols=`1`:`73`, names_to = "to_id", values_to="friend") %>%
  filter(friend==1) %>%
  group_by(from_id) %>%
  mutate(idegree=sum(friend),to_id=as.numeric(as.character(to_id))) %>%
  left_join(deg, by=c("to_id"="from_id")) %>%
  mutate(tot_fdeg=sum(deg), ave_fdeg=mean(deg)) %>% ungroup()

fdeg %>% group_by(from_id) %>%
  summarise(ideg=first(idegree), tot_fdeg=first(tot_fdeg),
            ave_fdeg=first(ave_fdeg))

