list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis.R")

registerDoParallel(cores=detectCores())



# Load Data

destfile = paste("./data/processed_us-counties_latest",".csv",sep="")

county_data <- read.csv(file = destfile)
county_data$datetime <- anytime::anydate(county_data$date)
county_data$log_rolled_cases <- log(county_data$rolled_cases)

state_list = sort(unique(county_data$state))
# switch to state_list for all states, Idaho, California, Massachusetts, Texas
windowsize = 7
predictionsize = 7
#for (cutoff in (earliest_start+windowsize):(latest_date -predictionsize)){
earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)


cutoff = 171
mainDir = "./data/output"
subDir = "backtest"
backtest_dir = file.path(mainDir, subDir)
dir.create(backtest_dir)

cutofflist = (earliest_start+predictionsize+1):(latest_date - predictionsize)
#cutofflist = (latest_date - predictionsize):(latest_date - predictionsize)
#cutofflist = 150:(latest_date - predictionsize)

for(cutoff in cutofflist){
  print(paste("Starting computation for cutoff=",toString(cutoff),sep=""))
  
  restricted_state_df0 <- NULL
  restricted_state_df1 <- NULL
  
  # Validation set
  restricted_state_df1 <- subset(county_data,days_from_start == cutoff + predictionsize)
  # Training Set
  
  state_df <- subset(county_data,days_from_start >= cutoff-windowsize & days_from_start <= cutoff)
  state_list <- sort(unique(state_df$state))
  try(restricted_state_df0 <- foreach(state = state_list, .combine=rbind) %dopar%{
    k = NULL
    k = try(county_analysis(state, county_data, cutoff-windowsize, cutoff,predictionsize))
    return(k)
  })
  
  if(is.null(restricted_state_df0)){
    next
  }
  
  today<-restricted_state_df0[c("date","days_from_start","county","state","fips","log_rolled_cases","r","t0","lm_predict","r.grf","t0.grf","grf_predict")]
  tomorrow<-restricted_state_df1[c("date","days_from_start","fips","log_rolled_cases")]
  restricted_state_df2<-merge(x=today,y=tomorrow,by="fips",x.all=TRUE)
  restricted_state_df2$lm_mse<-with(restricted_state_df2,(lm_predict-log_rolled_cases.y)**2)
  restricted_state_df2$grf_mse<-with(restricted_state_df2,(grf_predict-log_rolled_cases.y)**2)
  
  print(paste("Finished writing backtest for cutoff=",toString(cutoff),setp=""))
  
  backtest_file_path = file.path(backtest_dir, paste("allstates_",toString(cutoff),"_grf.csv",sep=""))
  
  write.csv(restricted_state_df2,backtest_file_path,row.names=FALSE)
  # break
}



closeAllConnections()

