library(tidyverse)
library(magrittr)
library(devtools)
install_github("ochyzh/networkdata")
library(networkdata)

#Can you transform `defAlly` into a matrix?

data(defAlly)
head(defAlly)

cntries = unique(defAlly$ccode1[defAlly$year==2012])

defMat = defAlly %>%
  dplyr::filter(year==2012) %>%
  dplyr::select(ccode1, ccode2, defAlly) %>%
  pivot_wider(names_from=ccode2, values_from=defAlly) %>%
  data.matrix()

rownames(defMat) = cntries
defMat = defMat[,-1]
defMat[is.na(defMat)] = 0
diag(defMat) = NA

defMat[1:20,1:20]
