#Your Turn
library(devtools)
#install_github("ochyzh/networkdata")
data("duqueData")
library(tidyverse)
#get the full list of actors:
myactors<-sort(as.numeric(unique(do.call("c",lapply(dipl_ties[1:3],names)))))
n<-length(myactors)

ally<-array(NA, dim=c(n,n,3),
            dimnames=list(myactors,
                          myactors,
                          seq(from=1970,to=1980, by=5)))

for(t in 1:3){
  d<-allies[[t]]
  for(i in 1:nrow(d)){
    for (j in 1:ncol(d)){
      a1 = names(d)[i]
      a2 = colnames(d)[j]
      val = as.numeric(as.character(d[i,j]))
      ally[a1,a2,t] <- val
      ally[a1,a2,t] <- val
    }}
}

ally[1:10,1:10,1]
ally<-varDyadCovar(ally[,,-3])


cont<-array(NA, dim=c(n,n,3),
            dimnames=list(myactors,
                          myactors,
                          seq(from=1970,to=1980, by=5)))
for(t in 1:3){
  d<-contig[[t]]
  for(i in 1:nrow(d)){
    for (j in 1:ncol(d)){
      a1 = names(d)[i]
      a2 = colnames(d)[j]
      val = as.numeric(as.character(d[i,j]))
      cont[a1,a2,t] <- val
      cont[a1,a2,t] <- val
    }}
}

cont[1:10,1:10,1]

cont<-varDyadCovar(cont[,,-3])

d<-expand.grid(myactors,seq(from=1970,to=1980,by=5)) %>%
  left_join(polity,by=c("Var1"="ccode","Var2"="year")) %>%
  select(Var1, Var2,dem=dem_dum) %>% pivot_wider(names_from=Var2, values_from=dem)
d<-as.matrix(d[-1])
dem<-varCovar(d[,-3], centered=TRUE)

# Put the variables together in the data set for analysis
Sienactors<-sienaNodeSet(158,"actors",myactors)
mydata <- sienaDataCreate(dipl,cont,ally,dem)
mydata

#Specify the Model
myeff1 <- getEffects(mydata)
myeff1
#effectsDocumentation(myeff1)
myeff1 <- includeEffects(myeff1,transTies)
myeff1 <- includeEffects(myeff1, X,interaction1="cont")
myeff1 <- includeEffects(myeff1, X,interaction1="ally")
myeff1 <- includeEffects(myeff1, simX, interaction1="dem")

myalgorithm <- sienaAlgorithmCreate(projname = 'Dties')
ans <- siena07(myalgorithm, data = mydata, effects = myeff1, batch=TRUE, returnDeps=TRUE)
#saveRDS(ans,"data/ans.rds")
