library(statnet)
data(faux.mesa.high)
mesa
plot(mesa, vertex.col="Grade")

table(mesa %v% 'Race')
table(mesa %v% 'Sex')
table(mesa %v% 'Grade')

mesa %v% "Grade"<-as.character(mesa %v% "Grade")

m1<-ergm(mesa~ edges+nodefactor("Race",
                      levels=c("Black","Hisp","Other","NatAm"))+
           nodefactor("Sex", levels="F")+
         nodefactor("Grade",levels=c("8","9","10","11","12"))+
           nodematch("Sex")+nodematch("Race",levels=c("White","Hisp",
                                                      "NatAm"), diff=TRUE)+
         nodematch("Grade", diff=TRUE)+gwesp(decay=.25,fixed=TRUE))
mcmc.diagnostics(m1)

summary(m1)

summary(mesa~ edges+nodefactor("Race",
                                levels=c("Black","Hisp","Other","NatAm"))+
           nodefactor("Sex", levels="F")+
           nodefactor("Grade",levels=c("8","9","10","11","12"))+
           nodematch("Sex")+nodematch("Race", diff=TRUE)+
           nodematch("Grade", diff=TRUE)+gwesp(decay=.25,fixed=TRUE))

mixingmatrix(mesa, "Race")

#install_github("ochyzh/networkdata")
library(networkdata)

data("allyData")
class(war)
length(war)
class(war[[1]])
dim(war[[1]])
war[[1]][1:3,1:3]

class(contiguity)
dim(contiguity)
contiguity[1:3,1:3]



