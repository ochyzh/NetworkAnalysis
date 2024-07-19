library(tidyverse)
library(mapproj)
library(maps)
library(mapdata)
library(spdep)
library(spatialreg)

#Suppose you want to test whether variable urb2010 is spatially clustered.
#Calculate a measure of the average urbanization in neighboring states.

mydata<-read.csv("./data/covid_data.csv", header=TRUE)
mydata$medinc1317[is.na(mydata$medinc1317)]<-0
mydata$trumpmarg[is.na(mydata$trumpmarg)]<-0
contigmat<-read.table("data/contigmat.txt") |> as.matrix()
contigmat1<-contigmat/apply(contigmat,1,sum) #row-standardize

mydata$W_medinc1317<-contigmat1%*%mydata$medinc1317
mydata$W_trumpmarg<-contigmat1%*%mydata$trumpmarg

summary(m1<-lm(data=mydata, urb2010~trumpmarg+medinc1317))
summary(m2<-lm(data=mydata, urb2010~trumpmarg+W_medinc1317))
summary(m3<-lm(data=mydata, urb2010~trumpmarg+W_trumpmarg))
#Estimate a model that accounts for clustering in urbanization.
#Is the effect of neighbor's urbanization positive or negative?
#Is this effect statistically significant?
W1<-mat2listw(contigmat, row.names = NULL, style="W", zero.policy = TRUE)
summary(W1$neighbours)
W2<-nb2listw(W1$neighbours, glist=NULL, style="W", zero.policy=TRUE)
m4 <- lagsarlm(data=mydata, urb2010~log(totpop1317)+trumpmarg+medinc1317,
               W2, zero.policy=TRUE)
summary(m4)



states <- map_data("state")
head(states)

library(ggplot2)
#Set theme options:
theme_set(theme_grey() + theme(axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank(),
                               legend.position="none"))
ggplot() +  geom_path(data=states, aes(x=long, y=lat, group=group),
                      color="black", size=.5)

ggplot() +  geom_polygon(data=states, aes(x=long, y=lat, group=group),
                         color="black", size=.5)+ coord_map()

statereg<- read.csv("./data/statereg.csv")
head(statereg)

states.class.map <- left_join(states, statereg, by = c("region" = "State"))
head(states.class.map)


ggplot() +  geom_polygon(data=states.class.map,
                         aes(x=long, y=lat, group=group,
                             fill = StateGroups), colour = I("black"))+
  coord_map()+theme(legend.position="bottom")



I<- diag(6)
X0<-as.matrix(cbind(1,log(d$totpop1317), d$urb2010, d$trumpmarg, d$medinc1317))

urb<-d$urb2010
urb[4]<-1
X1<-as.matrix(cbind(1,log(d$totpop1317), urb, d$trumpmarg, d$medinc1317))
A<-solve(I-coef(m3)[1]*mymat)

mycoef<-as.matrix(coef(m4))

Yhat0<- A%*%(X0%*%mycoef) #may need to remove first element of mycoeff[-1]; keep this for knitting (RDS works different)
Yhat1<- A%*%(X1%*%mycoef) #may need to remove first element of mycoeff[-1]

Y_ch<-Yhat1-Yhat0
sim<- cbind.data.frame(names,Y_ch)
counties <- map_data("county") |> dplyr::filter(region=="iowa") |>
  left_join(sim, by=c("subregion"="names"))

ggplot() +  geom_polygon(data=counties, aes(x=long, y=lat, group=group,
                        fill=Y_ch^(1/50)),color="black", size=.5)+ coord_map()












