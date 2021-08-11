library(RSiena)

friend.data.w1<-s501
friend.data.w2<-s502
friend.data.w3<-s503

drink<-s50a
smoke<-s50s

friendship<-sienaDependent(array(c(friend.data.w1,
                                   friend.data.w2,
                                   friend.data.w3),
                                 dim=c(50,50,3)))
friendship

class(friendship)

drinkingbeh<-sienaDependent(drink, type="behavior")

smoke1<-coCovar(smoke[,1])

NBdata<-sienaDataCreate(friendship,drinkingbeh,smoke1)

NBeff<-getEffects(NBdata)

effectsDocumentation(NBeff)

NBeff<-includeEffects(NBeff, simX, interaction1 = "smoke1")
NBeff<-includeEffects(NBeff, egoX, altX, simX,
                      interaction1="drinkingbeh")
NBeff<-includeEffects(NBeff, name="drinkingbeh", avAlt, outdeg,
                      indeg,interaction1="friendship" )


myalgorithm1<-sienaAlgorithmCreate(projnam='s50_NB')
NBans<-siena07(myalgorithm1, data=NBdata, effects=NBeff, batch=TRUE)

m2<-readRDS("data/m2.rds")

exp(-1.2946*(-1)-1.5840*(-1)^2+3.3212*0.5)/(1+exp(-1.2946*(-1)-1.5840*(-1)^2+3.3212*0.5))
summary(m2)

mean(drink)
range(drink)

#-1.2946*x-1.5840*x^2
# dy/dx=-1.2946+2*(-1.5840)*x=0
#-3.168x=1.2946
#x=-.4
#z=2.6
