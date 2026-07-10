#Knecht data example
library(RSiena)
library(btergm)
data(knecht)

class(friendship[[1]])

friend.data.w1 <- friendship[[1]]
friend.data.w2 <- friendship[[2]]
friend.data.w3 <- friendship[[3]]
friend.data.w4 <- friendship[[4]]

# Plot
library(statnet)
plot(as.network(friend.data.w1))

#Set up the data
friends <- sienaDependent(
  array( c( friend.data.w1, friend.data.w2, friend.data.w3,friend.data.w4 ),
         dim = c( 26, 26, 4 ) ) )
delinqbeh <- sienaDependent(delinquency, type = "behavior" )

demographics$religious<-as.numeric(demographics$religion %in% c(1,3))
relig <- coCovar(demographics$religious)
NBdata <- sienaDataCreate( friends, relig, delinqbeh)
NBeff <- getEffects( NBdata )

NBeff <- includeEffects(NBeff, simX,
                        interaction1 = "relig" )
NBeff <- includeEffects(NBeff, egoX, altX, simX,
                        interaction1 = "delinqbeh" )

NBeff <- includeEffects(NBeff, name = "delinqbeh",
                        avAlt,indeg,outdeg,
                        interaction1 = "friends" )

#1. Run the new model

myalgorithm2 <- sienaAlgorithmCreate(projname = 'delinq01' )
m2 <- siena07(myalgorithm2, data = NBdata,
              effects = NBeff, batch=TRUE,returnDeps=TRUE )
m2



