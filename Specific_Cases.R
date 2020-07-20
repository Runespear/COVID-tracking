list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

source("county_analysis.R")

# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=detectCores())



# Load Data

destfile <- paste("./data/processed_us-counties_",Sys.Date(),".csv",sep="")

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

for (cutoff in (latest_date - predictionsize-1):(latest_date - predictionsize)){
  state_df_list <- list()
  lm_mseS<-list()
  grf_mseS<-list()
  cutoff_S <-list()
  state_S <-list()
  total_df <- NULL
  
  restricted_state_df1 <- foreach(state = state_list, .combine=rbind) %dopar%{
    k = county_analysis(state, county_data, earliest_start,cutoff + predictionsize,predictionsize)
    return(k)
  }
  
  restricted_state_df0 <- foreach(state = state_list, .combine=rbind) %dopar%{
    k = county_analysis(state, county_data, cutoff-windowsize, cutoff,predictionsize)
    return(k)
  }
  
    
  today<-restricted_state_df0[c("date","county","state","fips","r","t0","lm_predict","r.grf","t0.grf","grf_predict")]
  tomorrow<-restricted_state_df1[c("date","fips","log_rolled_cases")]
  restricted_state_df2<-merge(x=today,y=tomorrow,by="fips",x.all=TRUE)
  restricted_state_df2$lm_mse<-with(restricted_state_df2,(lm_predict-log_rolled_cases)**2)
  restricted_state_df2$grf_mse<-with(restricted_state_df2,(grf_predict-log_rolled_cases)**2)
    
  #lm_mseS<-list.append(lm_mseS,sum(restricted_state_df2$lm_mse))
  #grf_mseS<-list.append(grf_mseS,sum(restricted_state_df2$grf_mse))
  #cutoff_S<-list.append(cutoff_S,cutoff)
  #state_S<-list.append(state_S,state)
    
  total_df <- rbind(total_df,restricted_state_df2)
  
  mainDir = "./data/output/"
  subDir = state
  file_sub = paste(mainDir,subDir,sep="")
  dir.create(file.path(mainDir, subDir))
    
  # write.csv(restricted_state_df2,paste(file_sub,"/",state,"_",toString(cutoff),"_grf.csv",sep=""),row.names=FALSE)
  
  #print(paste("Done writing csv for day ", toString(cutoff), " of " ,state,sep=""))
  # break

  #print(cbind(state_list,lm_mseS, grf_mseS))
  write.csv(total_df,paste(mainDir,"allstates_",toString(cutoff),"_grf.csv",sep=""),row.names=FALSE)
}



closeAllConnections()

