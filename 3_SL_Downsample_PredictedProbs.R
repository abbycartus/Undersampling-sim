###############################################################################################################################################
######################## SUPERLEARNER - DATA SPLITTING - DOWNSAMPLE  ##########################################################################
######################## GETTING FIT OBJECT                          ##########################################################################
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

## SET WORKING DIRECTORY WHERE YOU WANT RESULTS TO GO
# setwd("/Users/abigailcartus/Box/Dissertation backup/Dissertation/Simulation - Aim 1/Simulation manuscript/Results predicted probabilities")
setwd("C:\\Users\\abc72\\Box Sync\\Dissertation backup\\Dissertation\\Simulation - Aim 1\\Simulation manuscript\\Results predicted probabilities")


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
  
  # Doing the downsampling
  #Doing the downsampling
  y1 = subset(x,y==1) #Selecting all with y=1
  x0 = subset(x,y==0) #Selecting all with y=0
  
  nrow(y1)
  
  if(nrow(x0)>=nrow(y1)){
    y0 = x0[sample(1:nrow(y1),nrow(y1),replace=F),] #Sampling from those with y=0
  } else{
    y0 = x0
  }
  
  y_prime <- rbind(y0,y1) # combining the events and sampled non-events
  table(y_prime$y)
  
  missCount <- function(x){sum(is.na(x))}
  apply(y0,2,missCount)
  
  head(x)
  
  mean(y)
  sum(y)
  
  # Sample splitting
  trainIndex <- createDataPartition(y_prime$y, p = 0.7, list = FALSE, times = 1)
  xTrain <- y_prime[trainIndex,]
  xTest <- y_prime[-trainIndex,]
  
  
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

ptm <- proc.time()
system.time(res0 <- lapply(1:1000, function(z) simR(z)))
proc.time() - ptm

saveRDS(res0, file="SL_Downsample_fit.Rds")
