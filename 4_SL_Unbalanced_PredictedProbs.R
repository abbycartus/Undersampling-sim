###############################################################################################################################################
######################## SUPERLEARNER - DATA SPLITTING - UNBALANCED  ##########################################################################
######################## PREDICTED PROBABILITIES                     ##########################################################################
######################## 08.21.2019 ARC                              ##########################################################################
###############################################################################################################################################

## INSTALL AND LOAD PACKAGES


packages <- c("randomForest","data.table","doParallel","microbenchmark","SuperLearner","glmnet","e1071","beepr",
              "tidyverse","pROC","caret","kernlab","KernelKnn","xgboost", "ranger")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
}

for (package in packages) {
  library(package, character.only=T)
}

## SET WD -- WHERE YOU WANT RESULTS TO GO

#setwd("/Users/abigailcartus/Box/Dissertation backup/Dissertation/Simulation - Aim 1/Simulation manuscript/Results predicted probabilities")
setwd("C:\\Users\\abc72\\Box Sync\\Dissertation backup\\Dissertation\Simulation - Aim 1\\Simulation manuscript\\Results predicted probabilities")

## WRITE A FUNCTION TO SIMULATE DATA AND APPLY SUPERLEARNER

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
  
  # Sample splitting
  trainIndex <- createDataPartition(x$y, p = 0.7, list = FALSE, times = 1)
  xTrain <- x[trainIndex,]
  xTest <- x[-trainIndex,]
  
  
  # Do sample splitting -- 70% train, 30% test. Predict from test. 
  # Have x dataset and in x dataset add a column that is just a runif(1000,0,1). THis gives us a colunn of one realization of a uniform variable.
  # For training, just take the bottom 70% and for testing take top 30%. So if new var is >0.7, testing set. If <=0.7 training set 
  # Since I have a binomial outcome, might end up with too many in training and not enough in testing. Just go for it and try it. Do several runs with different realizations of runif variable. If they look wonky, then look at a caret function. In caret(), there's a create partition function. THis has a stratified splitting algorithm in there where you can keep proportions of the outcome roughly the same in both groups. 
  
  head(x)
  
  mean(y)
  sum(y)
  
  
  ## superlearner here -- all defaults to avoid issues with optimization of the non-negative least squares equation 
  sl.lib <- c("SL.ranger","SL.xgboost", "SL.ksvm", "SL.kernelKnn", "SL.glmnet") 
  
  folds = 10
  
  fitY=SuperLearner(Y=xTrain$y,X=xTrain[,-y], family="binomial",
                    method="method.AUC",
                    SL.library=sl.lib,
                    cvControl=list(V=folds,stratifyCV=T))
  
  #Have to just pull out predicted probabilities and y because the fit objects are too huge to save 
  fits <- predict(fitY, onlySL=T, newdata=xTest[,-y])
  pred_probs <- fits$pred
  res <- cbind(pred_probs, xTest$y)
  colnames(res)<- c("Predicted probabilities", "y")
  return(res)
  
}

system.time(res0 <- lapply(1:1000, function(z) simR(z)))

saveRDS(res0, file="SL_Unbalanced_fit.Rds")
