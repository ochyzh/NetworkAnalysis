library(tidyverse)
library(magrittr)
library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)

#Can you transform `defAlly` into a matrix?

data(defAlly)
head(defAlly)

cntries = unique(defAlly$ccode1[defAlly$year==2012])

defMat = defAlly |>
  dplyr::filter(year==2012) |>
  dplyr::select(ccode1, ccode2, defAlly) |>
  pivot_wider(names_from=ccode2, values_from=defAlly) |>
  data.matrix()

rownames(defMat) = cntries
defMat = defMat[,-1]
defMat[is.na(defMat)] = 0
diag(defMat) = NA

defMat[1:20,1:20]


#with a loop:
defMat1<- as.data.frame(matrix(0, nrow=length(cntries),ncol=length(cntries)))
names(defMat1) <- row.names(defMat1) <- cntries

for (i in 1:nrow(defAlly)){
  ccode1<- as.character(defAlly$ccode1[i])
  ccode2<- as.character(defAlly$ccode2[i])
  val<- defAlly$defAlly[i]
  defMat1[ccode1,ccode2]<-val
}
diag(defMat1) = NA
defMat1[1:20,1:20]
