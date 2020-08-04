list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","hash", "e1071")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("county_analysis.R")
#registerDoParallel(cores=detectCores())

# Location of processed county data

mainDir <- "./data/output"
destfile <- paste("./data/augmented_us-counties-states_latest",".csv",sep="")

county_data <- read.csv(file = destfile)
earliest_start <- min(county_data$days_from_start)
latest_date <- max(county_data$days_from_start)-8
county_list <- sort(unique(county_data$fips))

# Backtest directory
backtestDir <- file.path(mainDir,"backtest")

# read all the allstates_$(CUTOFF)_grf.csv files
cutoff_list <- earliest_start:latest_date


# Place to save resuslts
CountyDir <- file.path(mainDir,"Backtest_by_County")
dir.create(CountyDir)

plotDir <- file.path(mainDir,"Backtest_by_County_plots")
dir.create(plotDir)

for(c in county_list){
  plot.prepare <- data.frame("fips" = NA, "county"=NA, "state"=NA, "predicted.lm"=NA
                             , "predicted.slm"=NA, "predicted.grf"=NA, "predicted.grf.augmented"=NA, "predicted.grf.fonly"=NA
                             , "date.y"=NA, "days_from_start.y" = NA, "log_rolled_cases.y"=NA)
  for(cutoff in cutoff_list){
    filename_raw <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
    filename <- file.path(backtestDir,filename_raw)
    if (file.exists(filename)){
      # Read the file
      cutoff_df <- read.csv(file=filename)
      # Restrict to the county and variables of interest
      cutoff_df <- subset(cutoff_df,fips == c)
      #print(names(cutoff_df))
      cutoff_df<- cutoff_df[,which(names(cutoff_df) %in% c("fips", "date.y", "state", "county", "days_from_start.y", "predicted.lm"
                                                           , "predicted.slm", "predicted.grf", "predicted.grf.augmented", "predicted.grf.fonly"
                                                           ,"log_rolled_cases.y"))]
      # Drop NAs
      cutoff_df <- na.omit(cutoff_df)
      if(nrow(cutoff_df)==0){
        next
      }
      # Concatenate data frames
      plot.prepare<- rbind(plot.prepare,cutoff_df)
    }
  }
  plot.prepare<-na.omit(plot.prepare)
  if(nrow(plot.prepare)==0){
     next
   }
    MaxCase<-max(plot.prepare$log_rolled_cases.y)
    MinCase<-min(plot.prepare$log_rolled_cases.y)
    MaxDay<-max(plot.prepare$days_from_start.y)
    MinDay<-min(plot.prepare$days_from_start.y)
    
    performance_file_path = file.path(CountyDir, paste(toString(c),"_backtest.csv",sep=""))
    write.csv(plot.prepare,performance_file_path,row.names=FALSE)
    
    print(paste("Finished writing backtest for ",toString(cutoff_df$county)," county, ",toString(cutoff_df$state),setp=""))
    
    png(paste("./data/output/Backtest_by_County_plots/",toString(c),"_plot.png",sep=""), width = 1080, height = 720)
    
    title=paste("One Week Prediction","(",toString(cutoff_df$county)," county, ",toString(cutoff_df$state),")",sep="")
    
    plot(plot.prepare$days_from_start.y, plot.prepare$predicted.lm,pch=19, col="gray", type="b", xlab="days", ylab="Log Case Number", xlim=c(MinDay,MaxDay),ylim=c(MinCase,MaxCase),xaxs="i",yaxs="i", main=title)
    lines(plot.prepare$days_from_start.y, plot.prepare$predicted.slm,pch=18, col="blue", type="b", lty=2)
    lines(plot.prepare$days_from_start.y, plot.prepare$predicted.grf, pch=17, col="green", type="b",lty=3)
    lines(plot.prepare$days_from_start.y, plot.prepare$predicted.grf.augmented, pch=16, col="red", type="b",lty=4)
    lines(plot.prepare$days_from_start.y, plot.prepare$log_rolled_cases.y, pch=15, col="black", type="b",lty=5)
    legend(MinDay, MaxCase, legend=c("Predicted by LM", "Predicted by SLM", "Predicted by GRF", "Predicted by GRF with County Features","Actual Case Number"), col=c("gray", "blue", "green", "red","black"), lty=1:5, cex=0.8)
    
    dev.off()
    
    print(paste("Finished ploting backtest for ",toString(cutoff_df$county)," county, ",toString(cutoff_df$state),setp=""))
}

