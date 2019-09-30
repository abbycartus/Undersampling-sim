###############################################################################################################################################
######################## LOGISTIC REGRESSION - Downsampled          ##########################################################################
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
dslr <- readRDS("LR_Downsample_fit.Rds")

## Get the data in the form we want it
processF <- function(z){
  dat <- dslr[[z]]
  preds <- dat$fitted.values
  y <- dat$y
  res <-  cbind(preds,y)
  return(res)
}

downsampled_lr <- lapply(1:1000, function(z) processF(z))

## Now we have to apply a threshold to every object in the list to give a 0/1 for each predicted probability
predF <- function(z){
  dat <- as_tibble(downsampled_lr[[z]])
  names(dat)[names(dat) == "preds"] <- "pred_probs"
  dat_preds <- mutate(dat, pred_class = ifelse(pred_probs > 0.5, 1, 0))
  return(dat_preds)
}

lr_downsampled_class <- lapply(1:1000, function(z) predF(z))

## Now that we have the "true" and "predicted" classifications, we can make a list of 2x2 tables out of all of these

tableF <-  function(z) {
  lr_downsampled_table <- table(data.frame(lr_downsampled_class[[z]])[,2], data.frame(lr_downsampled_class[[z]])[,3])
  return(lr_downsampled_table)
}

lr_downsampled_table <- lapply(1:1000, function(z) tableF(z))
# Rows gives us true y. Row 1 = true 0, Row 2 = true 1
# Columns give us predicted class. Col 1 = Predicted 0, Col 2 = Predicted 1

## Finally, let's get all the performance metrics we want -- then we can take their averages
## Remember, there are some tables where we have one cell equal to zero. So we only want tables with dimensions = 4
lr_downsampled_table <- keep(lr_downsampled_table, ~sum(dim(.))==4)
# Doesn't make a difference

perfF <- function(z) {
  accuracy <- (lr_downsampled_table[[z]][2,2] + lr_downsampled_table[[z]][1,1])/(lr_downsampled_table[[z]][2,2] + lr_downsampled_table[[z]][1,1] + lr_downsampled_table[[z]][1,2] + lr_downsampled_table[[z]][[2,1]])
  se <- (lr_downsampled_table[[z]][[2,2]])/(lr_downsampled_table[[z]][[2,2]] + lr_downsampled_table[[z]][[2,1]])
  sp <- (lr_downsampled_table[[z]][1,1])/(lr_downsampled_table[[z]][1,1] + lr_downsampled_table[[z]][1,2])
  ppv <- (lr_downsampled_table[[z]][2,2])/(lr_downsampled_table[[z]][2,2] + lr_downsampled_table[[z]][1,2])
  npv <- (lr_downsampled_table[[z]][1,1])/(lr_downsampled_table[[z]][1,1] + lr_downsampled_table[[z]][2,1])
  
  perf_all <- cbind(accuracy, se, sp, ppv, npv)
  colnames(perf_all) <- c("accuracy", "se", "sp", "ppv", "npv")
  return(perf_all)
}

perf_all <- lapply(seq_along(lr_downsampled_table), function(z) perfF(z))

## Final step: unlist and save this object
perf_all_unlist <- as.data.frame(do.call(rbind, perf_all))
write_csv(perf_all_unlist, "lr_downsampled_predictive_performance.csv")

## We can also get our mean values here
mean_accuracy <- mean(perf_all_unlist$accuracy)
mean_se <- mean(perf_all_unlist$se)
mean_sp <- mean(perf_all_unlist$sp)
mean_ppv <- mean(perf_all_unlist$ppv)
mean_npv <- mean(perf_all_unlist$npv)
means_lr_downsampled <- data.frame(cbind(mean_accuracy, mean_se, mean_sp, mean_ppv, mean_npv))
write_csv(means_lr_downsampled, "lr_downsampled_means.csv")
# THESE ARE DIFFERENT THAN WHAT I REPORTED -- CHECK ON THEM PLEASE 
