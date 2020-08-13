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

registerDoParallel(cores=detectCores())

#########################################################
# POST PROCESSING FOR STATE FOREST BLOCKS
#########################################################

destfile = paste("./data/processed_us-counties_latest",".csv",sep="")
county_data <- read.csv(file = destfile)
county_data$date <- anytime::anydate(county_data$date)

start_date = min(county_data$days_from_start)
end_date = max(county_data$days_from_start)


# Loop through files in ./data/output/backtest_state_forests

backtest.folder <- "data/output/backtest_state_forests"
filelist <- list.files(path=backtest.folder, pattern="*.csv", full.names=FALSE, recursive=FALSE)

fips_all <- sort(unique(county_data$fips))


# Find the first cutoff 

cutoff.start = Inf


for (cutoff.check in start_date:end_date){
  fname <- paste("block_results_",toString(cutoff.check),".csv",sep="")
  full.path <- file.path(backtest.folder,fname)
  # If it doesn't exist, carry on, else, get first cutoff
  if (file.exists(full.path)){
    cutoff.start <- cutoff.check
    break
  }
}

#########################################################################
#  LOOP THROUGH FILES, CHECK 7 DAYS BEHIND
#########################################################################

# We need to see whether the grf from 7 days ago predicted something
# Create a folder for analysis results

confusion.block.folder <- "./data/output/confusion_state_forests"
dir.create(confusion.block.folder, showWarnings=FALSE)



mse.table <- data.frame("cutoff"=cutoff.start:end_date,"block.mse"=NA, "block.mse0"=NA, "block.mse6"=NA)

for (cutoff in cutoff.start:end_date){
  fname <- paste("block_results_",toString(cutoff),".csv",sep="")
  full.path <- file.path(backtest.folder,fname)
  
  
  df <- read.csv(full.path)
  new.df <- df
  
  if(cutoff - 7 < cutoff.start){
    # Data not available
    new.df$predicted.grf.past <- NA
    new.df$block.mse <- NA
  }
  else{
    past.fname <- paste("block_results_",toString(cutoff-7),".csv",sep="")
    past.full.path <- file.path(backtest.folder,past.fname)
    past.df <- read.csv(past.full.path)
    
    past.df$predicted.grf.past <- past.df$predicted.grf.future
    past.df$predicted.grf.past0 <- past.df$predicted.grf.future0
    past.df$predicted.grf.past6 <- past.df$predicted.grf.future6
    past.df <- past.df[c("fips","predicted.grf.past","predicted.grf.past0","predicted.grf.past6")]
    
    new.df <- merge(x=df,y=past.df,by="fips",all=TRUE)
    new.df$block.mse <- NA
    
    mask <- -which(is.na(new.df$predicted.grf.past))
    
    new.df[mask,"block.mse"] <- (new.df[mask,"log_rolled_cases.x"] - new.df[mask,"predicted.grf.past"])**2
    new.df[mask,"block.mse0"] <- (new.df[mask,"log_rolled_cases.x"] - new.df[mask,"predicted.grf.past0"])**2
    new.df[mask,"block.mse6"] <- (new.df[mask,"log_rolled_cases.x"] - new.df[mask,"predicted.grf.past6"])**2
    
    mse.table[which(mse.table$cutoff==cutoff),"block.mse"] <- mean(na.omit(new.df[,"block.mse"]))
    mse.table[which(mse.table$cutoff==cutoff),"block.mse0"] <- mean(na.omit(new.df[,"block.mse0"]))
    mse.table[which(mse.table$cutoff==cutoff),"block.mse6"] <- mean(na.omit(new.df[,"block.mse6"]))
    
  }
  
  # Write the csv
  results.fname <- paste("confusion_block_",toString(cutoff),".csv",sep="")
  results.fullpath <- file.path(confusion.block.folder,results.fname)
  write.csv(new.df,results.fullpath,row.names=FALSE)
  
  if (cutoff==114){
    #break
  }
}

# Write the mse


write.csv(mse.table,"./data/output/block_mse.csv",row.names=FALSE)

