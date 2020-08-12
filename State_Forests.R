closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#install.packages("RApiDatetime", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("grf", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("rattle", repos="http://cran.rstudio.com/", dependencies=TRUE)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis.R")

registerDoParallel(cores=detectCores())

# Obtain the latest data to see how many dates there are

destfile = paste("./data/augmented_us-counties_latest",".csv",sep="")

county_data <- read.csv(file = destfile)

earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)


block.folder = "./data/block"

cutoff.list <- earliest_start:latest_date

first.block.cutoff <- Inf

# Check for the first block file
for (cutoff in cutoff.list){
  # See if block is already in there
  # Block is numbered by last day in it
  fname <- paste("block_",toString(cutoff),".csv",sep="")
  full.path <- file.path(block.folder,fname)
  if (file.exists(full.path)){
    print(paste(fname," exists",sep=""))
    if (first.block.cutoff > cutoff){
      first.block.cutoff <- cutoff
    }
    break
  }
}
num_trees=2000
cutoff.list <- first.block.cutoff:latest_date
cutoff.list <- latest_date:latest_date
# Main loop, parallelize later
counter <- 1
for (cutoff in cutoff.list){
  # See if block is already in there
  # Block is numbered by last day in it
  fname <- paste("block_",toString(cutoff),".csv",sep="")
  full.path <- file.path(block.folder,fname)
  
  # Concatenate every 7 days until no more
  # e.g. 51 is the start
  # Then on 63, we have 63,56
  shift <- (cutoff - first.block.cutoff)%%7 
  data.cutoff.list <- c(seq(first.block.cutoff + shift, cutoff, 7))
  
  print(data.cutoff.list)
  
  block.fullpath.list <- c()
  for (block.number in data.cutoff.list){
    block.fullpath.list <- c(block.fullpath.list, file.path(block.folder, paste("block_",toString(block.number),".csv",sep="")))
  }
  
  df <- lapply(block.fullpath.list,read.csv)
  df <- do.call(rbind,df)
  
  treatment <- df$shifted_time
  outcome <- df$shifted_log_rolled_cases
  
  #exclusion <- c("shifted_log_rolled_cases","fips","State_FIPS_Code","county","state","datetime","log_rolled_cases.x","shifted_time")
  exclusion <- c("shifted_log_rolled_cases","datetime","State_FIPS_Code","county","state","log_rolled_cases.x","shifted_time")
  
  covariates <- (df[,-which(names(df) %in% exclusion)])
  
  covariates.test <- unique(covariates)
  
  state.tau.forest <- grf::causal_forest(X=covariates, Y=outcome, W= treatment, num.trees = num_trees)
  state.tau.hat <- predict(state.tau.forest, covariates.test, estimate.variance = FALSE)
  print(state.tau.hat)
  #state.r.grf <- unlist(state.tau.hat)[1]
  
  if (counter == 10){
    break
  }
  #break
  counter <- counter + 1
  
}

