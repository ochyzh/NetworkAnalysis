#1. For this exercise, you will work with the friendship data from `library(RSiena)`. Run the following code to load the data.

library(statnet)
library(tsna)
library(ndtv)
library(RSiena) #to get the data

friend.data.w1 <- s501
friend.data.w2 <- s502
friend.data.w3 <- s503
drink <- s50a
smoke <- s50s


#2. Format the friendship data from as a `networkDynamic` object.
# The dependent variable must be a `list` of `network` objects

class(friend.data.w1)
dim(friend.data.w1)
isSymmetric(friend.data.w1)


#need to combine `friend.data.w1`, `friend.data.w2`, and `friend.data.w3` into a list of network objects
library(statnet)
friends<-list(as.network(friend.data.w1, directed=TRUE),
              as.network(friend.data.w2, directed=TRUE),
              as.network(friend.data.w3, directed=TRUE))

length(friends)
class(friends[[1]])
dim(friends[[1]])

friendsDyn = networkDynamic(network.list=friends)

#3. Summarize changes in friendships (edges), triangles, and number of nodes with indegrees 1 nd 2, over time.
tErgmStats(friendsDyn, "~ edges+triangle+idegree(c(1,2))")

#4. Plot the three waves of the friends network side-by-side using the `network.extract` function.
par(mfrow = c(1,3))
p<-plot(
  network.extract(friendsDyn, at = 0),
  main = "Wave 1", displaylabels = F)
plot(
  network.extract(friendsDyn, at = 1),
  main = "Wave 2", displaylabels = F,coord=p)
plot(
  network.extract(friendsDyn, at = 2),
  main = "Wave 3", displaylabels = F,coord=p)
#5. Make a quick movie of the friends network over time using the `render.d3movie` function.

render.d3movie(friendsDyn,
               plot.par=list(displaylabels=T),filename="friends.html", launchBrowser=FALSE )

## Your Turn 1

#1. Set up `smoke` and `drink` as vertex attributes to the friendship network.

set.vertex.attribute(friends[[1]],"drink",drink[,1])
set.vertex.attribute(friends[[2]],"drink",drink[,2])
set.vertex.attribute(friends[[3]],"drink",drink[,3])

set.vertex.attribute(friends[[1]],"smoke",smoke[,1])
set.vertex.attribute(friends[[2]],"smoke",smoke[,2])
set.vertex.attribute(friends[[3]],"smoke",smoke[,3])

#2. Estimate a temporal `ergm` that models friendships as a function of
library(btergm)
Tergm_friends<-btergm(friends ~ edges +
                       nodeicov("drink")+nodeocov("drink")+ #- drinking
                       nodeicov("smoke")+nodeocov("smoke")+# and smoking behaviors,
                       absdiff("drink")+#- homophily (people make friends with those with similar drinking and smoking habits),
                       absdiff("smoke")+
                       mutual + #- reciprocity (why?)
                       triangles+ #- triad closure (why?)
                       istar(2) + ostar(2)+ #- popularity
                      delrecip()+ memory(type = "stability"),
                      R=1000)

summary(Tergm_friends)


#3. Add temporal terms to model stability and delayed reciprocity.

Tergm_friends1<-btergm(friends ~ edges +
                        nodeicov("drink")+nodeocov("drink")+ #- drinking
                        nodeicov("smoke")+nodeocov("smoke")+# and smoking behaviors,
                        absdiff("drink")+#- homophily (people make friends with those with similar drinking and smoking habits),
                        absdiff("smoke")+
                        mutual + #- reciprocity (why?)
                        triangles+ #- triad closure (why?)
                        istar(2) + ostar(2)+ #- popularity
                        memory(type = "stability", lag = 1)+
                        delrecip(mutuality = FALSE, lag = 1),
                      R=1000)

summary(Tergm_friends1)

