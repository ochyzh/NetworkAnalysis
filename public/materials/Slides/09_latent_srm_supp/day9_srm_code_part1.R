########################
# Day 3, Part 1
# SRM Motivation, diagnosing
# first and second order interdependencies 
# between observations 
########################

####
# path setup
pth = '~/Teaching/icpsr/nets/datasets/'
####

####
# packages
library(MASS)
library(ggplot2)
library(reshape2)
library(patchwork)
####

####
# load data
# enters a number of objects into the workspace
# we're going to be focusing on a directed dyadic
# df called trade
load(paste0(pth, 'day9_srm.rda'))
####

####
# trade ols estimation

# cross-sectional
# directed dyadic df
head(trade)

# model to fit
form = formula(
  trade ~
  polity1 + polity2 + conflicts + distance + shared_igos
  )
form

# ols estimation
ols = lm(form, data=trade)
summary(ols)$'coefficients'
####

####
# org resids so we can check for structure
# (could also just use your preferred solution
# from igraph, data.table, or dplyr)

# pull out errors from trade
trade$olsError = ols$residuals

# construct sociomatrix out of errors
# define actors
actors = unique(c(trade$Var1, trade$Var2))
n = length(actors)

# set up matrix in which to store resids
E = matrix(NA, nrow=n,ncol=n, dimnames=list(actors,actors))
for(ii in 1:nrow(trade)){
  a1 = trade$Var1[ii]
  a2 = trade$Var2[ii]
  val = trade$olsError[ii]
  E[a1, a2] = val }

# take a glimpse
E[1:3,1:3]
####

####
# check resids for structure

# start off with a simple check for recip
cor(c(E), c(t(E)), use='complete.obs')

# sender/receiver heterogeneity
rowErr = apply(E, 1, mean, na.rm=TRUE)
colErr = apply(E, 2, mean, na.rm=TRUE)

sort(rowErr)[1:3]
sort(colErr)[1:3]

sd(rowErr)
sd(colErr)
####

####
# visualize the patterns
errDF = melt(E)

# get rid of diags
errDF = na.omit(errDF)

# sender side
# sort by mean of err
errDF$Var1 = factor(errDF$Var1, levels=names(sort(rowErr)))

# viz
rowErrorGG = ggplot(errDF, aes(x=Var1,y=value)) +
  geom_boxplot() + geom_jitter(alpha=.3) +
  xlab('') + ylab('Residual by Sender') +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
    )

# receiver side
errDF$Var2 = factor(errDF$Var2, levels=names(sort(colErr)))
colErrorGG = ggplot(errDF, aes(x=Var2,y=value)) +
  geom_boxplot() + geom_jitter(alpha=.3) +
  xlab('') + ylab('Residual by Receiver') +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
    )

# arrange via patchwork
rowErrorGG / colErrorGG
####

####
# would we see this if we had correctly
# specified the model?
# lets run a quick sim study to check

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

# run model
olsSim = lm(y ~ x1 + x2, data=simData)

# repeat process of checking resids for structure
simData$olsError = olsSim$residuals
actors = unique(c(simData$Var1, simData$Var2))
n = length(actors)
E = matrix(NA, nrow=n,ncol=n, dimnames=list(actors,actors))
for(ii in 1:nrow(simData)){
  E[simData$Var1[ii], simData$Var2[ii]] = simData$olsError[ii] }

# check for pattern
cor(c(E), c(t(E)), use='pairwise.complete.obs')
rowErr = apply(E, 1, mean, na.rm=TRUE)
colErr = apply(E, 2, mean, na.rm=TRUE)

# org for plotting
errDF = melt(E) ; errDF = na.omit(errDF)

# viz sender
errDF$Var1 = factor(errDF$Var1, levels=names(sort(rowErr)))
rowErrorSimGG = ggplot(errDF, aes(x=Var1,y=value)) +
  geom_boxplot() + geom_jitter(alpha=.3) +
  xlab('') + ylab('Residual by Sender') +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
    )

# viz receiver
errDF$Var2 = factor(errDF$Var2, levels=names(sort(colErr)))
colErrorSimGG = ggplot(errDF, aes(x=Var2,y=value)) +
  geom_boxplot() + geom_jitter(alpha=.3) +
  xlab('') + ylab('Residual by Receiver') +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=45,hjust=1)
    )

# arrange
rowErrorSimGG / colErrorSimGG
####

####
# network measures of model fit
# using simulations to test gof of model

# first simulate some predictions from the model
set.seed(6886)
nSims = 1000
betaDraws = MASS::mvrnorm(n = nSims, mu = coef(ols), vcov(ols))
xMatrix = data.matrix(cbind(1, trade[,names(coef(ols))[-1]]))
preds = xMatrix %*% t(betaDraws)

# lets start by working with one prediction
# extract one prediction from simulated model
trade$yhat = preds[,1]

# get actor vector
actors = sort(unique(c(trade$Var1, trade$Var2)))
n=length(actors)

# organize adjacency matrix
yhatMat = matrix(NA, nrow=n, ncol=n, dimnames=list(actors,actors))
for(ii in 1:nrow(trade)){
  yhatMat[trade$Var1[ii],trade$Var2[ii]] = trade$yhat[ii]
}

# organize dv into adjacency matrix
Y = matrix(NA, nrow=n, ncol=n, dimnames=list(actors,actors))
for(ii in 1:nrow(trade)){
  a1 = trade$Var1[ii]
  a2 = trade$Var2[ii]
  val = trade$trade[ii]
  Y[a1,a2] = val }

# now lets see how our simulations from the model are doing
# in resembling the observed network

# sender heterogeneity
sd(apply(yhatMat, 1, mean, na.rm=TRUE))
sd(apply(Y, 1, mean, na.rm=TRUE)) # Y is a sociomatrix of trade$trade

# receiver heterogeneity
sd(apply(yhatMat, 2, mean, na.rm=TRUE))
sd(apply(Y, 2, mean, na.rm=TRUE))

# reciprocity
cor(c(yhatMat), c(t(yhatMat)),use='pairwise.complete.obs')
cor(c(Y), c(t(Y)),use='pairwise.complete.obs')
####

####
# just looking at one simulation is obviously silly
# so lets compare them all

# first some functions to help
# quick fn to get our adj mats
adjMat = function(data, valVar, actor1Var, actor2Var){

  # create actor vec
  actors = unique(c(data[[actor1Var]], data[[actor2Var]]))
  n = length(actors)

  # set up empty mat
  mat = matrix(NA, nrow=n, ncol=n, dimnames=list(actors,actors))

  # lazy fill in
  for( ii in 1:nrow(data) ){
    a1 = data[[actor1Var]][ii]
    a2 = data[[actor2Var]][ii]
    val = data[[valVar]][ii]
    mat[a1,a2] = val }

  #
  return(mat) }

# merge preds from each model into trade
colnames(preds) = paste0('sim', 1:ncol(preds))
trade = cbind(trade, preds)

# apply functions to generate a df object
# that has gofstats for every simulation
gofDF = matrix(NA, nrow=nSims, ncol=3,
  dimnames=list(NULL, c('rowSD', 'colSD', 'recip')))
for(ii in 1:nrow(gofDF)){
  predMat = adjMat(trade, paste0('sim', ii), 'Var1', 'Var2')
  gof = gofstats(predMat)
  gofDF[ii,] = gof  }

# visualize result
ggData = melt(gofDF)
# ggData$Var2 = char(ggData$Var2)

# calc actual values
ggData$actual = NA
ggData$actual[ggData$Var2=='rowSD'] = sd(apply(Y, 1, mean, na.rm=TRUE))
ggData$actual[ggData$Var2=='colSD'] = sd(apply(Y, 2, mean, na.rm=TRUE))
ggData$actual[ggData$Var2=='recip'] = cor(c(Y), c(t(Y)), use='pairwise.complete.obs')

# viz
ggplot(ggData, aes(x=value)) +
  facet_wrap(~Var2, scales='free') +
  geom_histogram() +
  xlab('') + ylab('') +
  geom_vline(aes(xintercept=actual), color='red') +
  theme_bw() +
  theme(
    axis.ticks=element_blank(),
    panel.border=element_blank()
    )
####
