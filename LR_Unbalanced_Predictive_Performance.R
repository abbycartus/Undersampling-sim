###############################################################################################################################################
######################## LOGISTIC REGRESSION - UNBALANCED            ##########################################################################
######################## PREDICTED PROBABILITIES                     ##########################################################################
######################## UPDATED PREDICTIVE PERFORMANCE MEASURES     ##########################################################################
######################## 08.27.2019 ARC                              ##########################################################################
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
setwd("C:\\Users\\abc72\\Box Sync\\Dissertation backup\\Dissertation\\Simulation - Aim 1\\Simulation manuscript\\Results predicted probabilities")


## READ IN THE OBJECT
unblr <- readRDS("LR_Unbalanced_fit.Rds")

## Get the data in the form we want it
processF <- function(z){
  dat <- unblr[[z]]
  preds <- dat$fitted.values
  y <- dat$y
  res <-  cbind(preds,y)
}

unbalanced_lr <- lapply(1:1000, function(z) processF(z))

## Now we have to apply a threshold to every object in the list to give a 0/1 for each predicted probability
predF <- function(z){
  dat <- as_tibble(unbalanced_lr[[z]])
  names(dat)[names(dat) == "preds"] <- "pred_probs"
  dat_preds <- mutate(dat, pred_class = ifelse(pred_probs > 0.2, 1, 0))
  return(dat_preds)
}

lr_unbalanced_class <- lapply(1:1000, function(z) predF(z))

## Now that we have the "true" and "predicted" classifications, we can make a list of 2x2 tables out of all of these

tableF <-  function(z) {
  lr_unbalanced_table <- table(data.frame(lr_unbalanced_class[[z]])[,2], data.frame(lr_unbalanced_class[[z]])[,3])
  return(lr_unbalanced_table)
}

lr_unbalanced_table <- lapply(1:1000, function(z) tableF(z))
# Rows gives us true y. Row 1 = true 0, Row 2 = true 1
# Columns give us predicted class. Col 1 = Predicted 0, Col 2 = Predicted 1

## Finally, let's get all the performance metrics we want -- then we can take their averages
## Remember, there are some tables where we have one cell equal to zero. So we only want tables with dimensions = 4
lr_unbalanced_table <- keep(lr_unbalanced_table, ~sum(dim(.))==4)
# Doesn't make a difference

perfF <- function(z) {
  accuracy <- (lr_unbalanced_table[[z]][2,2] + lr_unbalanced_table[[z]][1,1])/(lr_unbalanced_table[[z]][2,2] + lr_unbalanced_table[[z]][1,1] + lr_unbalanced_table[[z]][1,2] + lr_unbalanced_table[[z]][[2,1]])
  se <- (lr_unbalanced_table[[z]][[2,2]])/(lr_unbalanced_table[[z]][[2,2]] + lr_unbalanced_table[[z]][[2,1]])
  sp <- (lr_unbalanced_table[[z]][1,1])/(lr_unbalanced_table[[z]][1,1] + lr_unbalanced_table[[z]][1,2])
  ppv <- (lr_unbalanced_table[[z]][2,2])/(lr_unbalanced_table[[z]][2,2] + lr_unbalanced_table[[z]][1,2])
  npv <- (lr_unbalanced_table[[z]][1,1])/(lr_unbalanced_table[[z]][1,1] + lr_unbalanced_table[[z]][2,1])
  
  perf_all <- cbind(accuracy, se, sp, ppv, npv)
  colnames(perf_all) <- c("accuracy", "se", "sp", "ppv", "npv")
  return(perf_all)
}

perf_all <- lapply(seq_along(lr_unbalanced_table), function(z) perfF(z))

## Final step: unlist and save this object
perf_all_unlist <- as.data.frame(do.call(rbind, perf_all))
write_csv(perf_all_unlist, "lr_unbalanced_predictive_performance.csv")

## We can also get our mean values here
mean_accuracy <- mean(perf_all_unlist$accuracy)
mean_se <- mean(perf_all_unlist$se)
mean_sp <- mean(perf_all_unlist$sp)
mean_ppv <- mean(perf_all_unlist$ppv)
mean_npv <- mean(perf_all_unlist$npv)
means_lr_unbalanced <- data.frame(cbind(mean_accuracy, mean_se, mean_sp, mean_ppv, mean_npv))
write_csv(means_lr_unbalanced, "lr_unbalanced_means.csv")
# These are all exactly the same as what I reported before (currently in the manuscript) so, good! 
