######################################################################
# R script for illustrating the use of composition change directives #
######################################################################

# load RSiena commands:
library(RSiena)

# set working directory to where the data are:
setwd("<put appropriate path here>")
list.files()

# read the data sets for the assignment:
friendship.net1 <- as.matrix(read.table("klas12p-friends-waveA.dat",na.strings="9"))
friendship.net2 <- as.matrix(read.table("klas12p-friends-waveB.dat",na.strings="9"))
friendship.net3 <- as.matrix(read.table("klas12p-friends-waveC.dat",na.strings="9"))
friendship.net4 <- as.matrix(read.table("klas12p-friends-waveD.dat",na.strings="9"))
primary.net <- as.matrix(read.table("klas12p-primary.dat",na.strings="9"))
demographics <- as.matrix(read.table("klas12p-demographics.dat",na.strings="0"))

# find out number of actors in data sets:
numberActors <- dim(demographics)[1]
numberActors

###################################################
# FIRST ANALYSIS USING STRUCTURAL ZERO TREATMENT: #
###################################################

# identify dependent variable for the analysis:
friendship.SZ <- sienaNet(array(c(friendship.net1,friendship.net2,
  friendship.net3,friendship.net4),dim=c(numberActors,numberActors,4)))

# identify covariates for the analysis:
sex.M <- coCovar(demographics[,1])
primary <- coDyadCovar(primary.net)

# combine data for the analysis:
Data.SZ <- sienaDataCreate(friendship.SZ,sex.M,primary)

# get effects table for model specification:
Effects.SZ <- getEffects(Data.SZ)

# generate initial descriptive outputfile:
print01Report(Data.SZ, modelname="Composition-illustration-SZ")

# Take a look at the generated output file "Composition-illustration-SZ.out"
# to see how RSiena interpreted the data with structural zero codes.
# 
# From here on proceed "as usual".
# E.g., like this...

# Pick a model specification:
Effects.SZ <- includeEffects(Effects.SZ,X,interaction1='primary')
Effects.SZ <- includeEffects(Effects.SZ,sameX,interaction1='sex.M')
Effects.SZ <- includeEffects(Effects.SZ,transTrip,cycle3,inPop)
Effects.SZ

# Create model object and estimate:
Model.SZ <- sienaModelCreate(projname='klas12pResults-SZ',cond=FALSE)
Results.SZ <- siena07(Model.SZ,data=Data.SZ,effects=Effects.SZ)
Results.SZ
# If you like, you can here add a line saving the results as RData variables.

########################################################
# SECOND ANALYSIS USING COMPOSITION CHANGE DIRECTIVES: #
########################################################

# change structural zeros to normal zeros:
friendship.net1[friendship.net1==10] <- 0
friendship.net2[friendship.net2==10] <- 0
friendship.net3[friendship.net3==10] <- 0
friendship.net4[friendship.net4==10] <- 0

# identify dependent variable for the analysis:
friendship.CC <- sienaNet(array(c(friendship.net1,friendship.net2,
  friendship.net3,friendship.net4),dim=c(numberActors,numberActors,4)))

# read composition change directives:
composition <- sienaCompositionChangeFromFile("klas12p-composition.dat")
composition
# It is assumed that all entries and exits happen at the midpoint of the
# unobserved time period between observation moments.

# combine data for the analysis:
Data.CC <- sienaDataCreate(friendship.CC,sex.M,primary,composition)

# get effects table for model specification:
Effects.CC <- getEffects(Data.CC)

# generate initial descriptive outputfile:
print01Report(Data.CC, modelname="Composition-illustration-CC")

# Take a look at the generated output file "Composition-illustration-CC.out"
# to see how RSiena interpreted the directives provided.
# 
# From here on proceed "as usual".

# E.g., as above this...

# Pick a model specification:
Effects.CC <- includeEffects(Effects.CC,X,interaction1='primary')
Effects.CC <- includeEffects(Effects.CC,sameX,interaction1='sex.M')
Effects.CC <- includeEffects(Effects.CC,transTrip,cycle3,inPop)
Effects.CC

# Create model object and estimate:
Model.CC <- sienaModelCreate(projname='klas12pResults-CC',cond=FALSE)
Results.CC <- siena07(Model.CC,data=Data.CC,effects=Effects.CC)
Results.CC
# Again, you might want to add a line saving the results as RData variables.
