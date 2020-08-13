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
num_trees=200
cutoff.list <- first.block.cutoff:latest_date
#cutoff.list <- latest_date:latest_date
# Main loop, parallelize later

mainDir = "./data/output"
subDir = "backtest_state_forests"
outputfolder = file.path(mainDir, subDir)

dir.create(outputfolder, showWarnings = TRUE)


counter <- 1
foreach(cutoff = cutoff.list) %dopar%{
#for (cutoff in cutoff.list){
  # See if block is already in there
  # Block is numbered by last day in it
  start_time <- Sys.time()
  
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
  
  df.list <- lapply(block.fullpath.list,read.csv)
  df <- do.call(rbind,df.list)
  
  treatment <- df$shifted_time
  outcome <- df$shifted_log_rolled_cases
  
  #exclusion <- c("shifted_log_rolled_cases","fips","State_FIPS_Code","county","state","datetime","log_rolled_cases.x","shifted_time")
  exclusion <- c("shifted_log_rolled_cases","datetime","State_FIPS_Code","county","state","log_rolled_cases.x","shifted_time")
  
  covariates <- (df[,-which(names(df) %in% exclusion)])
  
  
  state.tau.forest <- grf::causal_forest(X=covariates, Y=outcome, W= treatment, num.trees = num_trees)
  
  current.block <- read.csv(file.path(block.folder, paste("block_",toString(cutoff),".csv",sep="")))
  covariates.test <- current.block[,-which(names(current.block) %in% exclusion)]
  covariates.test.unique <- unique(covariates.test)
  
  
  state.tau.hat <- predict(state.tau.forest, covariates.test.unique, estimate.variance = FALSE)$predictions
  #state.tau.hat <- unlist(state.tau.hat)
  print(state.tau.hat)
  
  identifiers <- unique(covariates.test.unique[c("fips","log_rolled_cases.y")])
  E.log_rolled_cases <- c()
  E.shifted_time <- c()
  nrows <- dim(identifiers)[1]
  for (i in 1:nrows){
    fips <- identifiers[i,1]
    cases <- identifiers[i,2]
    current.county.block <- subset(current.block, fips == fips & log_rolled_cases.y == cases)
    E.log_rolled_cases <- c(E.log_rolled_cases,mean(current.county.block$log_rolled_cases.x))
    # Doesn't matter that order for time is reversed
    E.shifted_time <- c(E.shifted_time,mean(cutoff-current.county.block$shifted_time))
  }
  state.t0.hat <- (E.log_rolled_cases - state.tau.hat*E.shifted_time)/(-state.tau.hat)
  
  print(state.t0.hat)
  
  predicted.grf.future <- state.tau.hat*(cutoff + 7 - state.t0.hat)
  
  # Write down results
  results <- data.frame("fips"=identifiers[1],"log_rolled_cases.y"=identifiers[2],"days_from_start"=cutoff)
  results <- merge(x=results,y=current.block[which(current.block$shifted_time==6),],by="fips")
  results <- results[, c("fips","county","state","days_from_start","datetime","log_rolled_cases.x")]
  results <- unique(results)
  results$t0.hat <- state.t0.hat
  results$tau.hat <- state.tau.hat
  results$predicted.grf.future <- (state.tau.hat*((cutoff + 7) - state.t0.hat ))
  results$Predicted_Double_Days <- log(2,exp(1))/state.tau.hat
  #results$date <- unique(current.block[which(current.block$shifted_time==6),"datetime"])
  #results$log_rolled_cases.x <- (current.block[which(current.block$shifted_time==6),"log_rolled_cases.x"])
  
  output.fname = paste("block_results_",toString(cutoff),".csv",sep="")
  destfolder = file.path(outputfolder,output.fname)
  write.csv(results, destfolder, row.names=FALSE)
  
  end_time <- Sys.time()
  
  time_taken <- end_time - start_time
  
  print(paste("Time taken for cutoff=",toString(cutoff)," is ",toString(time_taken),sep=""))
  
  if (counter >= 10){
    
    #break
  }
  #break
  counter <- counter + 1
  return(results)
}

