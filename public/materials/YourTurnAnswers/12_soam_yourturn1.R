#Previous code necessary for this to run:
library(RSiena)
friend.data.w1 <- s501
friend.data.w2 <- s502
friend.data.w3 <- s503
drink <- s50a
smoke <- s50s
friendship <- sienaDependent(
  array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
         dim = c( 50, 50, 3 ) ) )
drinkingbeh <- sienaDependent( drink, type = "behavior" )
smoke1 <- coCovar( smoke[ , 1 ] )
NBdata <- sienaDataCreate( friendship, smoke1, drinkingbeh)
NBeff <- getEffects( NBdata )

NBeff <- includeEffects(NBeff, simX,
                        interaction1 = "smoke1" )
NBeff <- includeEffects(NBeff, egoX, altX, simX,
                        interaction1 = "drinkingbeh" )

NBeff <- includeEffects(NBeff, name = "drinkingbeh",
                        avAlt,indeg,outdeg,
                        interaction1 = "friendship" )

#1. Run the new model

myalgorithm2 <- sienaAlgorithmCreate(projname = 's50CoEv_2' )
m2 <- siena07(myalgorithm2, data = NBdata,
              effects = NBeff, batch=TRUE,returnDeps=TRUE)

#2. What is the probability of a reciprocated tie?
#We need to set all the variables in the network equation to some
#meaningful values. For example, if we set smoke1 similarity=1,
#drinking beh alter=0 (remember that 0 is the mean of the demeaned variable),
#drinkingbeh ego=0, and drinkingbeh similarit=1 (same drinking behavior)
exp(-2.4804+ 2.7742+0.2967+1.3202)/(1+exp(-2.4804+ 2.7742+0.2967+1.3202))

#3. What is the probability of a reciprocated tie for two actors with the same
#drinking habits?
# Again, we need to set all the covariates to some values. Depending on what
#values you choose, you may end up with the same answer as the previous question.
exp(-2.4804+ 2.7742+0.2967+1.3202)/(1+exp(-2.4804+ 2.7742+0.2967+1.3202))

#4. What is the range of the drinking variable? What is it's mean?  What is the inflection point in the parabola
#that is fitted to the trend in drinking behavior?
mean(drink)
range(drink)

#-1.2946=2*1.5840x
#x=-0.408649 on the uncentered scale, which is 2.7 (-0.408649+3.113333) on the centered scale.


#5. What is the probability of someone moving from drinking=3 to drinking=2, without accounting for homophily?
exp(-1.2946*(2-3)-1.5840*(2-3)^2)/(1+exp(-1.2946*(2-3)-1.5840*(2-3)^2))

#6. Suppose an individual has 2 friends, one with a drink score of 5 and another with a drink score of 2.
#What is the probability that this individual will move from drinking=3 to drinking=2, accounting for homophily?

#This change would result in no change in the average score of friends, which is
#(7+2)/2=3.5 or 0.5 on the demeaned scale.
exp(-1.2946*(2-3)-1.5840*(2-3)^2+3.3212*0.5)/(1+exp(-1.2946*(2-3)-1.5840*(2-3)^2+3.3212*0.5))
