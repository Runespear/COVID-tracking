closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, require, character.only = TRUE)




# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis_lm.R")
registerDoParallel(cores=detectCores())


# Load Data


destfile = paste("../data/augmented_us-counties-states_latest",".csv",sep="")


county_data <- read.csv(file = destfile)
county_data$log_rolled_cases <- log(county_data$rolled_cases)
county_data <- subset(county_data, log_rolled_cases >= log(20,exp(1)))


# note -1 to the actual windowsize here
windowsize = 1
window_number= windowsize +3
feature_window = 1 
earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)




mainDir = "../data"
subDir = paste("block_windowsize=",toString(windowsize+1),sep="")
block_dir = file.path(mainDir, subDir)
dir.create(block_dir)


cutofflist = (earliest_start+6):(latest_date)
#cutofflist = (latest_date):(latest_date)


for(cutoff in cutofflist){
#foreach(cutoff = cutofflist) %dopar%{
  
  
  #################################
  # Skip file if it exists  
  check.file.name <- paste0("block_",toString(cutoff),".csv") 
  check.file.full.name <- file.path(block_dir, check.file.name) 
  if (file.exists(check.file.full.name)){next}
  #################################
  
  print(paste("Starting computation for cutoff=",toString(cutoff),sep=""))
  
  first<-cutoff-windowsize
  # Get rid of counties where there are less than 2 records so far
  restricted_state_df0 <- subset(county_data, days_from_start <= cutoff)
  tt <- table(restricted_state_df0$fips)
  restricted_state_df0 <- subset(restricted_state_df0,  fips %in% names(tt[tt>=2]) )
  
  # Get rid of counties that have less than 20 log_rolled_cases in the past 2 days
  restricted_state_df0 <- subset(restricted_state_df0, days_from_start >= cutoff-1 & days_from_start <= cutoff)
  tt <- table(restricted_state_df0$fips)
  restricted_state_df0 <- subset(restricted_state_df0,  fips %in% names(tt[tt>=2]) )
  
  if(nrow(restricted_state_df0)==0){
    next
  }
  
  restricted_state_df <- subset(restricted_state_df0, days_from_start >= first & days_from_start <= cutoff)
  
  
  # Taking only the first day data within the block
  Tfirst<-subset(restricted_state_df, days_from_start ==first)
  # Only keep fips and log_rolled_cases
  Tfirst<-Tfirst[, which(names(restricted_state_df) %in% c("fips","log_rolled_cases"))]
  
  Tlast<-subset(restricted_state_df, days_from_start ==cutoff)
  Tlast["cutoff"]<-Tlast$days_from_start
  # Discarding time variant features
  Tlast<-Tlast[,-which(names(Tlast) %in% c("State_FIPS_Code", "date", "datetime", "state", "county","days_from_start","log_rolled_cases","rolled_cases","logcases","deaths", "cases"))]


  Tlm<-county_analysis_lm(restricted_state_df,cutoff, feature_window)
  Tlm<-Tlm[,which (names(Tlm) %in% c("fips","r.lm","t0.lm"))]
    
  Tcase<- restricted_state_df[, which(names(restricted_state_df) %in% c("fips", "State_FIPS_Code", "datetime", "state", "county","log_rolled_cases"))]
  Tcase["shifted_time"]<- restricted_state_df$days_from_start - first
  

  #Tmain<-merge(x=merge(x=merge(x=Tcase,y=Tfirst,by="fips",x.all=TRUE),y=Tlast,by="fips",x.all=TRUE), y=Tlm,by="fips",x.all=TRUE)
  Tmain0<-merge(x=Tcase,y=Tfirst,by="fips",x.all=TRUE)
  Tmain1<-merge(x=Tmain0,y=Tlast,by="fips",x.all=TRUE)
  Tmain<-merge(x=Tmain1, y=Tlm,by="fips",x.all=TRUE)
  Tmain["shifted_log_rolled_cases"]<-Tmain$log_rolled_cases.x-Tmain$log_rolled_cases.y


  
  block_file_path = file.path(block_dir, paste("block_",toString(cutoff),".csv",sep=""))
  write.csv(Tmain,block_file_path,row.names=FALSE)
  print(paste("Finished writing block for cutoff=",toString(cutoff),setp=""))
}


closeAllConnections()