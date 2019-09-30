###############################################################################################################################################
######################## LOGISTIC REGRESSION -- DOWNSAMPLED          ##########################################################################
######################## GETTING FIT OBJECT                          ##########################################################################
######################## 08.21.2019 ARC                              ##########################################################################
###############################################################################################################################################


## INSTALL AND LOAD PACKAGES

packages <- c("randomForest","data.table","doParallel","microbenchmark","SuperLearner","parallel",
              "profvis","lattice","mvtnorm","doRNG","glmnet","e1071","beepr","tidyverse","pROC","caret")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
}

for (package in packages) {
  library(package, character.only=T)
}

## SET WD -- WHERE YOU WANT RESULTS TO GO

setwd("/Users/abigailcartus/Box/Dissertation backup/Dissertation/Simulation - Aim 1/Simulation manuscript/Results predicted probabilities")

## WRITE A FUNCTION TO SIMULATE DATA AND DO LOGISTIC REGRESSION 

simR <- function(index){
  print(index)
  set.seed(index,"L'Ecuyer-CMRG")
  p <- 10; N <- 1e3; yInt<--1
  expit<-function(x){1/(1+exp(-(x)))}
  U<-matrix(0,N,p)
  for(ii in 1:p){U[,ii]<-runif(N,-1,1)}
  X<-matrix(0,N,p)
  int<-log((1/(1-.5))-1)
  coefs<-as.matrix(log(runif(p,min=.5,max=1.5)))
  for(ii in 1:p){X[,ii]<-rbinom(N,1,expit(int+U%*%coefs))}
  colMeansX<-apply(X,2,mean)
  y<-rbinom(N,1,expit(yInt+X%*%coefs+U%*%coefs))
  
  x = data.frame(y,X,U)
  head(x)
  
  mean(y)
  sum(y)
  
  
  ## Logistic regression here
  
  fitY.logit <- glm(y ~ .,data=x,family=binomial("logit"))
  return(fitY.logit)
}
res0 <- lapply(1:1000, function(z) simR(z))

saveRDS(res0, file="LR_Unbalanced_fit.Rds")
