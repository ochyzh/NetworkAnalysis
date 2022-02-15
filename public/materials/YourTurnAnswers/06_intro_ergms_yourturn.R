library(statnet)
library(sna)

#Your Turn: Estimate and ergm on coleman data

data('coleman')
friends<-as.network.matrix(coleman[1,,],
                           matrix.type="adjacency", directed=TRUE)
m11 = ergm(friends ~ edges)
summary(m11)
plogis(coef(m11)[['edges']])
#Remember the baseline probability of friendships from last class?

m12 = ergm(friends ~ edges +
             mutual
)
summary(m12)
plogis(coef(m12)[['edges']] + coef(m12)[['mutual']])
