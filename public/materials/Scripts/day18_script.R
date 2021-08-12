library(sna)
library(ergm)
library(network)

library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)

data(lazega)
class(lazega)
list.vertex.attributes(lazega)

m1<-ego_ergm(lazega,
             form=c("edges","mutual",'desp(.8,type="OSP")',
                    'desp(.8,type="ISP")'),
             core_size = 1,
             min_size = 5,
             roles = 3,
             forking = FALSE,
             ncpus = 1,
             directed = TRUE,
             edge_covariates = FALSE,
             seed = 12345,
             steps = 50,
             tol = 1e-06)

m1$group.theta

m1$role_assignments

status<-lazega %v% "Status"
table(status[-58], m1$role_assignments[,2])
length(m1$role_assignments[,2])

plot(lazega,
     displaylabels=FALSE,
     vertex.cex=m1$role_assignments[,2],
     vertex.col='Status',
     edge.col="gray")

data(sampson)
m2<-ego_ergm(samplike,
             form=c("edges",
                    "mutual",
                    'desp(.8,type="OSP")',
                    'desp(.8,type="ISP")'),
             core_size = 1,
             min_size = 5,
             roles = 2,
             forking = FALSE,
             ncpus = 1,
             directed = TRUE,
             edge_covariates = FALSE,
             seed = 12345,
             steps = 50,
             tol = 1e-06)

table(samplike %v% "group", m2$role_assignments[,2])
table(samplike %v% "cloisterville", m2$role_assignments[,2])

grp<-samplike %v% "group"
grp<-as.numeric(as.factor(grp))
plot(samplike,
     displaylabels=FALSE,
     vertex.cex=grp,
     vertex.col=2*m2$role_assignments[,2],
     edge.col="gray")

m3<-ergm(samplike~ edges+ nodematch('group'))
summary(m3)

pred_ergm<-predict(m3, conditional=TRUE,
                   type="response",
                   nsim=100,
                   output="matrix")

#install.packages("ROCR")
library("ROCR")
library(tidyverse)
library(magrittr)

as.matrix(samplike)
myDV<-as.matrix(samplike) %>% data.frame() %>%
  mutate(name1=row.names(.)) %>%
  pivot_longer(cols=-name1, names_to="name2", values_to="Y")

pred_ergm %>% data.frame() %>%
  mutate(name1=row.names(.)) %>%
  pivot_longer(cols=-name1, names_to="name2", values_to="p") %>%
  left_join(myDV, by=c("name1","name2"))->mydat

pred<-prediction(mydat$p,mydat$Y)
perf<-performance(pred,"tpr","fpr")
plot(perf, colorize=TRUE)
