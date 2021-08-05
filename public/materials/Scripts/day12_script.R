library(statnet)
library(latentnet)
library(devtools)
install_github("ochyzh/newtorkdata")
library(networkdata)

data("Kirkland2012")
AMnet01
as.matrix(AMnet01)[1:10,1:10]

class(same.dist01)
same.dist01[is.na(same.dist01)]<-0
same.dist01[1:5,1:5]

class(same.party01)
same.party01[1:5,1:5]

set.network.attribute(AMnet01,"samedist",same.dist01)
set.network.attribute(AMnet01,"sameparty",same.party01)

interact01[1:5,1:5]
interact01(is.na(interact01))<-0
set.network.attribute(AMnet01,"interact01",interact01)

m1<-ergmm(AMnet01~edgecov("sameparty")+
            edgecov("samedist")+
            edgecov("interact01")+euclidean(d=2))            )
mcmc.diagnostics(m1)




