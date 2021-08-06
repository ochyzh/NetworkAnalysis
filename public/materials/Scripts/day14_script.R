library(amen)
data(IR90s)

gdp<-IR90s$nodevars[,2]

topgdp<-which(gdp>=sort(gdp, decreasing=TRUE)[30])
Y<-log(IR90s$dyadvars[topgdp,topgdp,2]+1)
class(Y)


m1<-ame(Y, family='nrm', symmetric=FALSE,
        cvar=FALSE, rvar=FALSE, dcor=FALSE,
        R=2, plot=FALSE, print=FALSE)


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

NBeff<-includeEffects(NBeff, transTrip, transRecTrip)
NBeff<-includeEffects(NBeff, egoX, altX,  interaction1="smoke1")


myalgorithm1<-sienaAlgorithmCreate(projnam='s50_NB')
NBans<-siena07(myalgorithm1, data=NBdata, effects=NBeff, batch=TRUE)
