########################
# Day 3, Part 2
# Running our first SRM to account for
# first and second order dependencies
########################

####
# path setup
# set your own path
# pth = '~/Teaching/icpsr/nets/datasets/'
pth = '~/Teaching/icpsr/nets/2025/09_srm/day9_srm/'
####

####
# packages
library(amen)
library(ggplot2)
library(tidyr)
library(reshape2)
library(gridExtra)
####

####
# data files to load
# loads in a number of objects including some
# data files that we will use for modeling
# and outputs of ame so that you dont need to run
# them on your own
load(paste0(pth, 'day9_srm.rda'))
####

####
# for better or worse, amen is very different in the
# input it expects and how it structures output

# we're starting with a typical df object
head(trade)
####

####
# lets start with the dv
# for cross-sectional network the amen
# package expects a n x n matrix for the DV

# in the other script I showed
# how to do it lazily with a for
# loop and a preallocated object,
# this time lets use the acast
# function from reshape2
# (note i'm avoiding tidyr because
# it outputs a data.frame/tibble)
yMat = acast(
	data=trade,
	formula=Var1 ~ Var2,
  value.var='trade' )

# now lets construct our dyadic variables
# for cross-sectional networks the amen
# package expects a n x n x pd array for
# the dyadic covariates, where pd
# refers to the number of dyadic variables

# to construct this we'll first reshape our
# data, so that each of the dyadic variables
# is going down a column
ids = c('Var1', 'Var2')
dyadVars = c('conflicts', 'distance', 'shared_igos')
dyadVarLong = melt(
  trade[,c(ids, dyadVars)], id=c('Var1', 'Var2'))

# now lets turn them into an array using
# acast again
dyadArr = acast(
  data=dyadVarLong,
  formula=Var1~Var2~variable,
  value.var='value' )

# last lets prep our nodal variables
# for cross-sectional networks the amen
# package expects a n x pr/c matrix for the
# the nodal variables
# ame also allows you to input sender and
# receiver specific nodal variables
nVars = c('pop','gdp','polity')
sendVars = paste0(nVars, '1')
recVars = paste0(nVars, '2')
sendLong = unique(trade[,c('Var1', sendVars)])
sMat = data.matrix(sendLong[,-1])
rownames(sMat) = sendLong$Var1

recLong = unique(trade[,c('Var2', recVars)])
rMat = data.matrix(recLong[,-1])
rownames(rMat) = recLong$Var2

# now lets just make sure that the countries
# show up in the same order for each of our objects
# we'll use the ordering of actors in yMat as a guide
actors = rownames(yMat)

# apply this ordering scheme to dyadArr
dyadArr = dyadArr[actors,actors,]

# and to the matrices for sender and
# receiver covariates
sMat = sMat[actors,]
rMat = rMat[actors,]

# lets save these objects because
# we'll be using them in later scripts
save(
  yMat,
  dyadArr,
  sMat,
  rMat,
  file=paste0(pth, 'ameCrossSec_v2.rda')
)
####

####
# lets prep our data for analysis
# using netify
library(netify)

net = netify(
  input = trade, 
  actor1 = 'Var1',
  actor2 = 'Var2',
  symmetric = FALSE, 
  weight = 'trade',
  nodal_vars = c('pop1', 'gdp1', 'polity1'),
  dyad_vars = c('conflicts', 'distance', 'shared_igos'),
  dyad_vars_symmetric = c(TRUE, TRUE, TRUE)
)

# now we have created a netify object
# yay us

# lets prepare this for or first 
# network model using the amen 
# package
# to do this we will use the 
# prep_for_amen fn from the
# netify package
amen_data = to_amen(net)

# lets extract every element from
# amen_data into its own object
# for easier passing to the amen fnction
# so that we can run our first social
# relations model
yMat = amen_data$Y
dyadArr = amen_data$Xdyad
sMat = amen_data$Xrow
rMat = amen_data$Xcol
####

####
# great we're all set with the data

# now lets start simple and get a feel for ame
# we'll start by turning off all the net
# related parameters and run what is essentially
# a simple bayesian linear regression
# should not take more than 30 secs

# commenting it out because i've already run it for
# you and there's an objected called fitNoNet that
# gets loaded in when you load in the day3b.rda
# files
  # fitNoNet = ame(
  #   Y=yMat,
  #   Xdyad=dyadArr,
  #   Xrow=sMat,
  #   Xcol=rMat,
  #   family='nrm',
  #   rvar=FALSE,
  #   cvar=FALSE,
  #   dcor=FALSE,
  #   R=0,
  #   intercept=TRUE,
  #   symmetric=FALSE,
  #   seed=6886,
  #   nscan=10000,
  #   burn=5000,
  #   odens=10,
  #   plot=FALSE,
  #   print=FALSE,
  #   gof=TRUE
  # )
####

####
# ame output
names(fitNoNet)

# BETA
# posterior samples for reg coefs
head(fitNoNet$BETA)

# VC
# posterior samples for vc params
head(fitNoNet$VC)

# YPM
# posterior mean of Y (predicted values)
dim(fitNoNet$YPM)
fitNoNet$YPM[1:5,1:5]

# GOF
# goodness of fit stats
# first row represents observed
# remaining rows represent gof
# stat for every predicted network
head(fitNoNet$GOF)
####

####
# first steps after running an ame model
# check if the model converged
# construct trace plots for beta estimates
beta = fitNoNet$BETA

# reorg data
beta = data.frame(beta)
beta$iter = 1:nrow(beta)
betaLong = melt(beta, id='iter')

# viz
ggplot(betaLong, aes(x=iter, y=value, group=variable)) +
  geom_line() +
  facet_wrap(~variable, scales='free_y', ncol=1) +
  theme_bw() +
  theme(
    axis.ticks=element_blank(),
    panel.border=element_blank()
  )

# helper function from me in the
# ameHelpers file that does something similar but
# with a bit more info
paramPlot(fitNoNet$BETA[,1:5])
paramPlot(fitNoNet$BETA[,6:10])
####

####
# next step after you've ensured
# the trace plots look okay is to check gof
# in our case keep in mind that we are at
# this point just running a bayesian
# linear regression
head(fitNoNet$GOF)

# we'll focus on sd.rowmean, sd.colmean
# dyad.dep, and trans.dep for this workshop
gof = fitNoNet$GOF[,-4]
head(gof)
actualVals = fitNoNet$GOF[1,]
gof = gof[-1,]

# reorg for plotting
gof = data.frame(gof)
gof$iter = 1:nrow(gof)
gofLong = melt(gof, id='iter')

# add in a column for the actual values
gofLong$actual = NA
gStats = unique(char(gofLong$variable))
for(g in gStats){
  gofLong$actual[gofLong$variable==g] = actualVals[g] }

# now lets plot
ggplot(gofLong, aes(x=value)) +
  geom_histogram() +
  geom_vline(aes(xintercept=actual), color='red') +
  facet_wrap(~variable, scales='free_y') +
  theme_bw() +
  theme(
    axis.ticks=element_blank(),
    panel.border=element_blank()
  )

# again a helper function from ameHelpers
# just need to make sure to remove the cycles calc
gofPlot(
  fitNoNet$GOF[,-4],
  symmetric=FALSE)
####

####
# okay, so the networks that we simulated from our
# model are not doing a good job in capturing the
# gof stats that we're targeting, lets run
# a model with the srm parameters added in
# now lets add in the srm parameters

# commenting it out because i've already run it for
# you and there's an objected called fitSRM that
# gets loaded in when you load in the day3b.rda
# files
# fitSRM = ame(
#   Y=yMat,
#   Xdyad=dyadArr,
#   Xrow=sMat,
#   Xcol=rMat,
#   family='nrm',
#   rvar=TRUE,
#   cvar=TRUE,
#   dcor=TRUE,
#   R=0,
#   intercept=TRUE,
#   symmetric=FALSE,
#   seed=6886,
#   nscan=10000,
#   burn=5000,
#   odens=10,
#   plot=FALSE,
#   print=FALSE,
#   gof=TRUE
# )
####

####
# check convergence
paramPlot(fitSRM$BETA[,1:5])
paramPlot(fitSRM$BETA[,6:10])
####

####
# commenting it out because i've already run it for
# you and there's an objected called fitSRM that
# gets loaded in when you load in the day3b.rda
# files
# fitSRM_v2 = ame(
#   Y=yMat,
#   Xdyad=dyadArr,
#   Xrow=sMat,
#   Xcol=rMat,
#   family='nrm',
#   rvar=TRUE,
#   cvar=TRUE,
#   dcor=TRUE,
#   R=0,
#   intercept=TRUE,
#   symmetric=FALSE,
#   seed=6886,
#   nscan=200000,
#   burn=20000,
#   odens=10,
#   plot=FALSE,
#   print=FALSE,
#   gof=TRUE
# )
####

####
# repeat checks, first check for
# convergence
paramPlot(fitSRM_v2$BETA[,1:5])
paramPlot(fitSRM_v2$BETA[,6:10])

# can also use formal tests from 
# the coda package
library(coda)

# examples of diagnostic checks
heidel.diag(mcmc(fitSRM$BETA))

# now lets see how we're doing wrt
# to the gof stats in the observed
# network
gofPlot(fitSRM_v2$GOF[,-4], FALSE)
head(fitSRM_v2$GOF)
####

####
# great, our model seems to be performing
# better on the gofStats that we've discussed so far
# before dealing with where we are still off lets
# talk about the other output that this model
# generated for us

# as a reminder
names(fitSRM_v2)

# first lets check the posterior samples for
# the vc params again
head(fitSRM_v2$VC)

# we can use the paramPlot function to summarize
# these results
paramPlot(fitSRM_v2$VC)

# and there's more, we also now have
# sender and receiver random effects from the model
# as well
head(fitSRM_v2$APM)
head(fitSRM_v2$BPM)

# we can visualize these as well
muEff = fitSRM_v2$APM
muDf = data.frame(mu=muEff)
muDf$id = rownames(muDf)
muDf$id = factor(muDf$id, levels=muDf$id[order(muDf$mu)])
muDf$ymax = with(muDf, ifelse(mu>=0,mu,0))
muDf$ymin = with(muDf, ifelse(mu<0,mu,0))
ggplot(muDf, aes(x=id, y=mu)) +
  geom_point() +
  geom_linerange(aes(ymax=ymax,ymin=ymin)) +
  xlab('') + ylab('Sender Effects') +
  theme(
    axis.ticks=element_blank(),
    axis.text.x=element_text(angle=45, hjust=1, size=4)
  )

# there's a function in ameHelpers that we can
# use to construct this viz as well called
# abPlot, it's pretty simple
abPlot(fitSRM_v2$APM, 'Sender Effects')

# but now lets put all the pieces of
# that we estimated from the srm
# together
grid.arrange(
  paramPlot(fitSRM_v2$VC),
  arrangeGrob(
    abPlot(fitSRM_v2$APM, 'Sender Effects'),
    abPlot(fitSRM_v2$BPM, 'Receiver EFfects')
  ), ncol = 2)
####

####
# and of course, the actual regression coef
# results!

# there's a great function in the amen package
# to quickly get a summary of parameter estimates
summary(fitSRM_v2)

# we can compare this to the model where we
# did not inlcude the srm parameters
summary(fitNoNet)

# you can also easily calculate these from
# the beta matrix in the output as well
# with a more bayesian flavor
summChain = function(x){
  mu = mean(x)
  qt90s = quantile(x, probs=c(0.05, 0.95))
  qt95s = quantile(x, probs=c(0.025, 0.975))
  out = c(
    lo95 = qt95s[1],
    lo90 = qt90s[1],
    mu = mean(x),
    hi95 = qt95s[2],
    hi90 = qt90s[2] )

  #
  return(out) }

#
round(t(apply(fitSRM_v2$BETA, 2, summChain)), 2)
round(t(apply(fitNoNet$BETA, 2, summChain)), 2)

# and then proceed to generate tables
# via your favorite .tex table generator
# or hopefully via a viz instead
####

####
# what happens if we run a sr-r-m model
# but there is no extra network dependence
# to test this we'll set up a simulated
# dataset in which there is no extra 
# network dependence and see if we can
# still recover the true effects 
# with the, in this case, unnecessary
# extra mechanics of the sr-r-m
# spoiler: yes

# set up some fake dyadic data
simData = expand.grid(letters,letters, stringsAsFactors = FALSE)
# remove i-i observations
simData = simData[simData$Var1 != simData$Var2,]

# add x1
set.seed(6886)
simData$x1 = rnorm(nrow(simData))

# add x2
simData$x2 = rnorm(nrow(simData))

# create a y
simData$y = -2 + 3*simData$x1 + 3.2*simData$x2 + rnorm(nrow(simData))

# 
yMat = acast(
  data=simData, 
  formula=Var1 ~ Var2, 
  value.var = 'y'
)

#
idVars = c('Var1', 'Var2')
dyadVars = c('x1', 'x2')
simDataLong = melt(simData[,c(idVars, dyadVars)], id=idVars)
xDyad = acast(
  simDataLong, 
  formula=Var1 ~ Var2 ~variable,
  value.var='value'
)

# 
srmMod = ame(
  Y=yMat, 
  Xdyad = xDyad, 
  family='nrm',
  symmetric=FALSE,
  nscan=10000, 
  burn=500,
  odens=25, 
  seed=6886,
  plot=FALSE, 
  print=FALSE
)

summary(srmMod)
paramPlot(srmMod$BETA)
####

####
# and what if we wanted to run parallel chains? 
# we'll do this using the sim data from the section above

# first we need the help of a couple of packages
library(doParallel)
library(foreach)

# next we decide how many threads
# we want to give up to our computer
cl = makeCluster(4)

# then we register these with our computer, to tell
# it that using the current R process we will be 
# setting up four additional R environments
registerDoParallel(cl)

# now we run the parallelized code, this is much
# like a typical lapply statement, foreach by
# default will output the results as a list object

# one thing to keep in mind is that the objects in your
# current global environment will be made available
# to the separate R sessions that are being set up
# but you will need to explicitly load in any packages
# in our case here we just need to load the amen
# package

# you can either comment in the code below and run yourself (it should be quick)
# or you can just use the included srmChain object from the day3b.rda file

# srmChain = foreach(
#   seed=1:4, .packages=c('amen')) %dopar% {
      
#   # run model
#   fit = ame(
#     Y=yMat, 
#     Xdyad = xDyad, 
#     family='nrm',
#     symmetric=FALSE,
#     nscan=1000, 
#     burn=500,
#     odens=10, 
#     seed=seed, # set a different seed each time
#     plot=FALSE, 
#     print=FALSE
#   )
#   return(fit)
    
#   }
# stopCluster(cl)
# save(srmChain, file='~/Teaching/icpsr/nets/lectures/day3b/code/results/srmChain.rda')

# we can use the gelman.diag test
# from the coda pkg when we have multiple chains
library(coda)

# first we need to take the BETA results
# from every model we've run and combine them
# into a list of mcmc objects
mcmcList = lapply(srmChain, function(x) mcmc(x$BETA))
gelman.diag(mcmcList)

# approx convergence is indicated from this diagnostic
# when the psrf is close to 1, in this case we would def
# meet that criteria

# and if we desire we can use some functions from the
# bayesplot package that help us to do some quick plotting
library(bayesplot)

# e.g., trace plots with multiple chains
mcmc_trace(mcmcList)

# and coefficent plots
mcmc_intervals(mcmcList)

# this is kind of a contrived case so things work out really nicely, 
# with a small number of iterations and chains (though four is usuall
# adequate)

# if i missed anything dont hesitate to let me know!
####



