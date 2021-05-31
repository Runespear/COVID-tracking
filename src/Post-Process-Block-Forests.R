closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=detectCores())

#########################################################
# POST PROCESSING FOR STATE FOREST BLOCKS
#########################################################

destfile = paste("../data/processed_us-counties_latest_minus7",".csv",sep="")
county_data <- read.csv(file = destfile)
county_data$date <- anytime::anydate(county_data$date)

# DROP fips = NA 
county_data <- county_data[-which(is.na(county_data$fips)),]
start_date = min(county_data$days_from_start)
end_date = max(county_data$days_from_start)

#fips list

fips.list.dest = "../data/fips-list.csv"
fips.list.df <- read.csv(fips.list.dest)

# Merge the two

all.info <- merge(x=fips.list.df,y=subset(county_data,select=-c(county,state)),by="fips",all=TRUE)

# Check for missing fips

all.info.fips <- sort(unique(all.info$fips))
missing.fips.list <- all.info.fips[which(! all.info.fips %in% unique(fips.list.df$fips)) ]



# Loop through files in .,/data/output/backtest_state_forests
windowsize=2
backtest.folder <- paste("../data/output/backtest_state_forests_windowsize=",toString(windowsize),sep="")
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

#break

#########################################################################
#  LOOP THROUGH FILES, CHECK 7 DAYS BEHIND
#########################################################################

# We need to see whether the grf from 7 days ago predicted something
# Create a folder for analysis results


confusion.block.folder <- paste("../data/output/confusion_state_forests_windowsize=",toString(windowsize),sep="")
dir.create(confusion.block.folder, showWarnings=FALSE)



mse.table <- data.frame("cutoff"=cutoff.start:end_date,"block.mse"=NA, "block.mse.0"=NA, "block.mse.last"=NA)

mape.table <- data.frame("cutoff"=cutoff.start:end_date,"block.mape"=NA, "block.mape.0"=NA, "block.mape.last"=NA)

cutofflist<-cutoff.start:end_date
#cutofflist<-(end_date-1):end_date


for (cutoff in cutofflist){
  fname <- paste("block_results_",toString(cutoff),".csv",sep="")
  full.path <- file.path(backtest.folder,fname)
  
  
  df <- read.csv(full.path)
  new.df <- df
  
  # We need to populate the entire table with fips even if they had no cases / predictions
  
  # Step 1: Generate a table of fips:county:state:date:days_from_start
  cases.data.exclude <- c("county","state","date","days_from_start")
  cases.date.slice <- county_data[which(county_data$days_from_start==cutoff), -which(names(county_data) %in% cases.data.exclude)] 
  
  imputter.df <- fips.list.df
  imputter.df$cutoff <- cutoff
  
  imputter.df <- merge(x=imputter.df,y=cases.date.slice,by="fips",all=TRUE)
  
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
    past.df$predicted.grf.past.0 <- past.df$predicted.grf.future.0
    past.df$predicted.grf.past.last <- past.df$predicted.grf.future.last
    past.df <- past.df[c("fips","predicted.grf.past","predicted.grf.past.0","predicted.grf.past.last","log_rolled_cases.x")]
    
    new.df <- merge(x=df,y=past.df,by="fips",all=TRUE)
    new.df$block.mse <- NA
    
    mask <- -which(is.na(new.df$predicted.grf.past))
    
    new.df[mask,"block new cases"] <- exp(new.df[mask,"log_rolled_cases.x.x"])-exp(new.df[mask,"log_rolled_cases.x.y"])
    
    new.df[mask,"block.mse"] <- (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past"])**2
    new.df[mask,"block.mse.0"] <- (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past.0"])**2
    new.df[mask,"block.mse.last"] <- (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past.last"])**2
    
    new.df[mask,"block.mape"] <- abs( (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past"])/new.df[mask,"log_rolled_cases.x.x"] )
    new.df[mask,"block.mape.0"] <- abs( (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past.0"])/new.df[mask,"log_rolled_cases.x.x"] )
    new.df[mask,"block.mape.last"] <- abs( (new.df[mask,"log_rolled_cases.x.x"] - new.df[mask,"predicted.grf.past.last"])/new.df[mask,"log_rolled_cases.x.x"] )
    
    mse.table[which(mse.table$cutoff==cutoff),"block.mse"] <- mean(na.omit(new.df[,"block.mse"]))
    mse.table[which(mse.table$cutoff==cutoff),"block.mse.0"] <- mean(na.omit(new.df[,"block.mse.0"]))
    mse.table[which(mse.table$cutoff==cutoff),"block.mse.last"] <- mean(na.omit(new.df[,"block.mse.last"]))
    
    mape.table[which(mape.table$cutoff==cutoff),"block.mape"] <- mean(na.omit(new.df[,"block.mape"]))
    mape.table[which(mape.table$cutoff==cutoff),"block.mape.0"] <- mean(na.omit(new.df[,"block.mape.0"]))
    mape.table[which(mape.table$cutoff==cutoff),"block.mape.last"] <- mean(na.omit(new.df[,"block.mape.last"]))
    
  }
  
  test.df <- new.df[,-which(names(new.df) %in% c("county","state","datetime","new_rolled_cases"))]
  test.df <- merge(x=imputter.df,y=test.df,by="fips",all=TRUE)
  test.df <- test.df[,-which(names(test.df) %in% c("deaths","cases","logcases","rolled_cases","cutoff"))]
  # Rename datetime to date.x
  names(test.df)[names(test.df) == "datetime"] <- "date.x"
  # Rename datetime to date.x
  names(test.df)[names(test.df) == "weekly_cases"] <- "weekly new cases"
  
  
  # Write the csv
  results.fname <- paste("confusion_block_",toString(cutoff),".csv",sep="")
  results.fullpath <- file.path(confusion.block.folder,results.fname)
  write.csv(test.df,results.fullpath,row.names=FALSE)
  
  #if (cutoff==114){
    #break
  #}
  # Write the final csv if last
  if (cutoff == end_date){
    
    destfile <- paste("../data/14_Day_Table",".csv",sep="")
    county_14data <- read.csv(file = destfile)
    
    destfile <- paste("../data/30_Day_Table",".csv",sep="")
    county_30check <- read.csv(file = destfile)
    
    #mu<- 6.7
    #sigma<- 5.2
    #test.df$Rt<-with(test.df,exp(tau.hat*mu-0.5*(tau.hat**2)*(sigma**2)))
    
    #test.df[which(test.df$state == "Iowa"),"Predicted_Double_Days"]<- NA
    #test.df[which(test.df$state == "Missouri"),"Predicted_Double_Days"]<- NA
    #test.df[which(test.df$state == "Alabama"),"Predicted_Double_Days"]<- NA
    test.df[which(test.df$fips == 48041),"Predicted_Double_Days"]<- NA
    test.df[which(test.df$fips == 35057),"Predicted_Double_Days"]<- NA
    test.df[which(test.df$fips == 6037),"Predicted_Double_Days"]<- NA
    # test.df[which(test.df$fips == 1009),"Predicted_Double_Days"]<- NA
    # test.df[which(test.df$fips == 1115),"Predicted_Double_Days"]<- NA
    # test.df[which(test.df$fips == 1133),"Predicted_Double_Days"]<- NA
    
   final.df<-merge(x=merge(x=test.df,y=county_30check, by="fips", all.x=TRUE),y=county_14data, by="fips", all.x=TRUE)
    
   final.df[which(final.df$d20 == 1),"Predicted_Double_Days"]<- NA
    
   write.csv(final.df,"../data/output/file_to_plot/confusion_block_latest.csv",row.names=FALSE)
  }
}

# Write the mse


write.csv(mse.table,paste("../data/output/block_mse_windowsize=",toString(windowsize),".csv",sep=""),row.names=FALSE)

write.csv(mape.table,paste("../data/output/block_mape_windowsize=",toString(windowsize),".csv",sep=""),row.names=FALSE)

closeAllConnections()