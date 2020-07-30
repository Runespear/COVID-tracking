list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table")

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
#cutofflist = 150:151

cutoff.list <- c()
lm.mse.list <- c()
slm.mse.list <- c()
grf.mse.list <- c()

for(cutoff in cutofflist){
  print(paste("Starting computation for cutoff=",toString(cutoff),sep=""))
  
  restricted_state_df0 <- NULL
  restricted_state_df11 <- NULL
  # Validation set
  restricted_state_df1 <- subset(county_data,days_from_start == cutoff + predictionsize)
  
  restricted_state_df <- subset(county_data, days_from_start >= cutoff-windowsize & days_from_start <= cutoff+ predictionsize)
  tt <- table(restricted_state_df$fips)
  restricted_state_df <- subset(restricted_state_df,  fips %in% names(tt[tt>=7]) )
  
  # Validation set 11
  state_df1 <- subset(restricted_state_df,days_from_start >= cutoff-windowsize & days_from_start <= cutoff+ predictionsize)
  state_list1 <- sort(unique(state_df1$state))
  try(restricted_state_df11 <- foreach(state = state_list1, .combine=rbind) %dopar%{
    k = NULL
    k = try(county_analysis(state, restricted_state_df, cutoff, cutoff+ predictionsize,predictionsize))
    return(k)
  })
  
  if(is.null(restricted_state_df11)){
    print("no restricted_state_df11")
    next
  }
  

  # Training Set
  restricted_state_df <- subset(county_data, days_from_start >= cutoff-windowsize & days_from_start <= cutoff)
  tt <- table(restricted_state_df$fips)
  restricted_state_df <- subset(restricted_state_df,  fips %in% names(tt[tt>=7]) )
  
  
  state_df <- subset(county_data,days_from_start >= cutoff-windowsize & days_from_start <= cutoff)
  state_list <- sort(unique(state_df$state))
  try(restricted_state_df0 <- foreach(state = state_list, .combine=rbind) %dopar%{
    k = NULL
    k = try(county_analysis(state, restricted_state_df, cutoff-windowsize, cutoff,predictionsize))
    return(k)
  })
  
  if(is.null(restricted_state_df0)){
    print("no restricted_state_df0")
    next
  }
  
  if (!is.null(restricted_state_df11) && !is.null(restricted_state_df0)) {
    today<-restricted_state_df0[c("date","days_from_start","county","state","fips","log_rolled_cases","r.lm","t0.lm","predicted.lm","r.slm","t0.slm","predicted.slm","r.grf","t0.grf","predicted.grf")]
    tomorrow<-restricted_state_df1[c("date","days_from_start","fips","log_rolled_cases")]
    #tomorrow1<-restricted_state_df11[c("fips","r.lm","r.slm")]
    restricted_state_df2<-merge(x=today,y=tomorrow,by="fips",x.all=TRUE)
    #restricted_state_df2<-merge(x=merge(x=today,y=tomorrow,by="fips",x.all=TRUE),y=tomorrow1,by="fips",x.all=TRUE)
    restricted_state_df2$lm.mse<-with(restricted_state_df2,(predicted.lm-log_rolled_cases.y)**2)
    restricted_state_df2$slm.mse<-with(restricted_state_df2,(predicted.slm-log_rolled_cases.y)**2)
    restricted_state_df2$grf.mse<-with(restricted_state_df2,(predicted.grf-log_rolled_cases.y)**2)
    
    
    restricted_state_df2 <- na.omit(restricted_state_df2)
    
    cutoff.list <- c(cutoff.list, cutoff)
    lm.mse.list <- c(lm.mse.list, mean(restricted_state_df2$lm.mse))
    slm.mse.list <- c(slm.mse.list, mean(restricted_state_df2$slm.mse))
    grf.mse.list <- c(grf.mse.list, mean(restricted_state_df2$grf.mse))
    
    print(paste("cutoff=",toString(cutoff)," slm.mse=", toString(mean(restricted_state_df2$slm.mse))," lm.mse=",toString(mean(restricted_state_df2$lm.mse))," grf.mse=", toString(mean(restricted_state_df2$grf.mse)) ,sep=""))
    print(paste("Finished writing backtest for cutoff=",toString(cutoff),setp=""))
    
    backtest_file_path = file.path(backtest_dir, paste("allstates_",toString(cutoff),"_grf.csv",sep=""))
    
    write.csv(restricted_state_df2,backtest_file_path,row.names=FALSE)
    # break
  }
}

performance.list <- list(cutoff=cutoff.list, lm.mse=lm.mse.list, slm.mse=slm.mse.list, grf.mse=grf.mse.list)
performance.table <- as.data.frame(performance.list)
discrepancy = restricted_state_df2[which(restricted_state_df2$lm.mse != restricted_state_df2$slm.mse),]

write.csv(performance.table,file.path(mainDir,"mse_table.csv"),row.names=FALSE)

closeAllConnections()

