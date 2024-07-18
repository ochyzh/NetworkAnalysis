library(tidyverse)

mydata<-read.csv("./data/covid_data.csv", header=TRUE) 
mydata$trumpmarg[is.na(mydata$trumpmarg)]<-0
contigmat<-read.table("data/contigmat.txt") |> as.matrix()


apply(contigmat,1,sum) 

contigmat1<-contigmat/apply(contigmat,1,sum) #row-standardize
mydata$W_trumpmarg<-contigmat1%*%mydata$trumpmarg

summary(m1<-lm(data=mydata, cases_pc~urb2010+trumpmarg+medinc1317)) #common exposure
summary(m2<-lm(data=mydata, cases_pc~urb2010+trumpmarg+medinc1317+W_trumpmarg)) #homophily


library(spdep)
library(spatialreg)
W1<-mat2listw(contigmat, row.names = NULL, style="W", zero.policy = TRUE)
summary(W1$neighbours)
W2<-nb2listw(W1$neighbours, glist=NULL, style="W", zero.policy=TRUE)
m3 <- lagsarlm(data=mydata, cases_pc~log(totpop1317)+urb2010+trumpmarg+
                 medinc1317, W2, zero.policy=TRUE)
summary(m3)


names<-c("benton","linn","jones","iowa","johnson","cedar")
mymat<-matrix(c(0,1,0,1,0,0,
                1,0,1,0,1,1,
                0,1,0,0,0,1,
                1,0,0,0,1,0,
                0,1,0,1,0,1,
                0,1,1,0,1,0),nrow=6,ncol=6)
dimnames(mymat)<-list(names,names)
mymat<-round(mymat/apply(mymat,1,sum),2)
d<-dplyr::filter(mydata, state=="IA" & county %in% names)


I<- diag(6)
X0<-cbind(1,log(d$totpop1317), d$urb2010, d$trumpmarg, d$medinc1317)
urb<-d$urb2010
urb[4]<-1
X1<-cbind(1,log(d$totpop1317), urb, d$trumpmarg, d$medinc1317)
A<-solve(I-coef(m3)[1]*mymat)
Yhat0<- A%*%(X0%*%coef(m3)[-1])
Yhat1<- A%*%(X1%*%coef(m3)[-1])
Y_ch<-Yhat1-Yhat0
Y_ch

