closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","tidyverse", "lubridate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#install.packages("RApiDatetime", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("grf", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("rattle", repos="http://cran.rstudio.com/", dependencies=TRUE)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis(shifted).R")

#registerDoParallel(cores=detectCores())



# Load Data

destfile = paste("./data/augmented_us-counties_latest",".csv",sep="")
#destfile = paste("./data/county_features",".csv",sep="")

county_data <- read.csv(file = destfile)
county_data$date <- anytime::anydate(county_data$date)
county_data$log_rolled_cases <- log(county_data$rolled_cases)
county_data <- subset(county_data, log_rolled_cases >= log(20,exp(1)))


state_list = sort(unique(county_data$state))
# switch to state_list for all states, Idaho, California, Massachusetts, Texas
windowsize = 6
predictionsize = 7
#for (cutoff in (earliest_start+windowsize):(latest_date -predictionsize)){
earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)

print(latest_date)

#cutoff = 171
mainDir = "./data/output"
subDir = "backtest"
backtest_dir = file.path(mainDir, subDir)
dir.create(backtest_dir)

cutofflist = (earliest_start+windowsize):latest_date
#futurelist= (latest_date - predictionsize+1):latest_date
#cutofflist = (latest_date - predictionsize):(latest_date - predictionsize)
#cutofflist = 150:(latest_date - predictionsize)
#cutofflist = 150:151
#lastcutoff = tail(cutofflist,n=1)
#cutofflist = (latest_date-predictionsize-3):(latest_date)

cutoff.list <- c()
date.x.list <- c()
lm.mse.list <- c()
slm.mse.list <- c()

for(cutoff in cutofflist){
  
  #cutoff_future<-cutoff+predictionsize
  
  print(paste("Starting computation for cutoff=",toString(cutoff),sep=""))
  
  # restricted_state_df0 <- NULL
  # restricted_state_df1 <- NULL
  # Validation set
  # restricted_state_df <- subset(county_data, days_from_start >= cutoff-windowsize & days_from_start <= cutoff+ predictionsize)
  # tt <- table(restricted_state_df$fips)
  # restricted_state_df <- subset(restricted_state_df,  fips %in% names(tt[tt>=7]) )
  
  state_df1 <- subset(county_data, days_from_start == cutoff+ predictionsize)
  #  state_list1 <- sort(unique(state_df1$state))
  #try(restricted_state_df11 <- foreach(state = state_list1, .combine=rbind) %dopar%{
  #  k = NULL
  #  k = try(county_analysis(state, restricted_state_df, cutoff, cutoff+ predictionsize,predictionsize))
  #  return(k)
  #})
  #
  #if(is.null(restricted_state_df11)){
  #  print("no restricted_state_df11")
  #  next
  #}
  
  
  # Training Set
  restricted_state_df <- subset(county_data, days_from_start >= cutoff-windowsize & days_from_start <= cutoff)
  tt <- table(restricted_state_df$fips)
  restricted_state_df <- subset(restricted_state_df,  fips %in% names(tt[tt>=7]) )
  
  
  restricted_state_df0<-county_analysis(restricted_state_df,cutoff-windowsize,cutoff, predictionsize)
  
  #  state_df <- subset(county_data,days_from_start >= cutoff-windowsize & days_from_start <= cutoff)
  #  state_list <- sort(unique(state_df$state))
  #  try(restricted_state_df0 <- foreach(state = state_list, .combine=rbind) %dopar%{
  #    k = NULL
  #    k = try(county_analysis(state, restricted_state_df, cutoff-windowsize, cutoff,predictionsize))
  #    return(k)
  #  })
  
  if(is.null(restricted_state_df0)){
    print("no restricted_state_df0")
    next
  }
  
  
  today<-restricted_state_df0[c("date","days_from_start","county","state","fips","log_rolled_cases","r.lm","t0.lm","predicted.lm","r.slm","t0.slm","predicted.slm")]
  tomorrow<-state_df1[c("date","days_from_start","fips","log_rolled_cases")]
  #tomorrow1<-restricted_state_df11[c("fips","r.lm","r.slm")]
  
  
  restricted_state_df2 <- today
  restricted_state_df2 <- unique(restricted_state_df2)
  # Merge only when there is validation data available
  if (cutoff + predictionsize <= latest_date){
    print(paste("Validation data available for cutoff=",toString(cutoff)),sep="")
    restricted_state_df2<-merge(x=today,y=tomorrow,by="fips",x.all=TRUE)
    #restricted_state_df2<-merge(x=merge(x=today,y=tomorrow,by="fips",x.all=TRUE),y=tomorrow1,by="fips",x.all=TRUE)
    restricted_state_df2$lm.mse<-with(restricted_state_df2,(predicted.lm-log_rolled_cases.y)**2)
    restricted_state_df2$slm.mse<-with(restricted_state_df2,(predicted.slm-log_rolled_cases.y)**2)
    
    
    restricted_state_df2 <- na.omit(unique(restricted_state_df2))
    
    cutoff.list <- c(cutoff.list, cutoff)
    date.x.list <- c(date.x.list, max(anytime::anydate(restricted_state_df2$date.x)))
    lm.mse.list <- c(lm.mse.list, mean(restricted_state_df2$lm.mse))
    slm.mse.list <- c(slm.mse.list, mean(restricted_state_df2$slm.mse))
    
    
    print(paste("cutoff=",toString(cutoff)," slm.mse=", toString(mean(restricted_state_df2$slm.mse))," lm.mse=",toString(mean(restricted_state_df2$lm.mse))," grf.mse=",sep=""))
    
    
  }
  else{
    # Append .x to column names of date, days_from_start, log_rolled_cases
    #restricted_state_df2<-rename(restricted_state_df2, c("date"="date.x","days_from_start"="days_from_start.x","log_rolled_cases"="log_rolled_cases.x"))
    
    #today<-restricted_state_df0[c("date","days_from_start","county","state","fips","log_rolled_cases","r.lm","t0.lm","predicted.lm","r.slm","t0.slm","predicted.slm")]
    restricted_state_df2 <- restricted_state_df2 %>% rename( date.x=date, days_from_start.x=days_from_start, log_rolled_cases.x=log_rolled_cases)
    restricted_state_df2$date.y<- ymd(restricted_state_df2$date.x)+days(7)
    restricted_state_df2$days_from_start.y<- restricted_state_df2$days_from_start.x+7
    restricted_state_df2$log_rolled_cases.y<- NA
  }
  
  
  print(paste("Finished writing backtest for cutoff=",toString(cutoff),setp=""))
  backtest_file_path = file.path(backtest_dir, paste("allstates_",toString(cutoff),"_grf.csv",sep=""))
  #confusion_file_path = file.path(mainDir, "confusion", paste("confusion_allstates_",toString(cutoff),"_grf.csv",sep=""))
  
  write.csv(restricted_state_df2,backtest_file_path,row.names=FALSE)
  # temp measure
  #write.csv(restricted_state_df2,confusion_file_path,row.names=FALSE)
}

performance.list <- list(cutoff=cutoff.list, date.x=date.x.list, lm.mse=lm.mse.list, slm.mse=slm.mse.list)
performance.table <- as.data.frame(performance.list)
# discrepancy = restricted_state_df2[which(restricted_state_df2$lm.mse != restricted_state_df2$slm.mse),]

write.csv(performance.table,file.path(mainDir,"mse_table.csv"),row.names=FALSE)


closeAllConnections()

