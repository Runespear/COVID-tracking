list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)



# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis.R")

registerDoParallel(cores=detectCores())


# Define Classes

OutbreakClass <- function(Double_Days){
  # Less than or equal to 3 days is outbreak
  if (0 <= Double_Days & Double_Days <= 7){
    return(4)
  }
  else if (7 < Double_Days & Double_Days <=14){
    return(3)
  }
  else if (14 < Double_Days & Double_Days <=21){
    return(2)
  }
  else if (21 < Double_Days & Double_Days <=28){
    return(1)
  }
  else{
    return(0)
  }
}


# Load Data
mainDir = "./data/output/"
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

# Predict Given latest date

allstates_latest_df <- foreach(state = state_list, .combine=rbind) %dopar%{
  k = county_analysis(state, county_data, latest_date-windowsize,latest_date,predictionsize)
  return(k)
}

write.csv(allstates_latest_df,paste(mainDir,"allstates_","latest","_grf.csv",sep=""),row.names=FALSE)


# Combine with counties that are not included in allstates_latest_df
prediction_df <- allstates_latest_df

prediction_df["Double_Days"] <- log(2,exp(1))/prediction_df["r.grf"]

class_list <- lapply(prediction_df[["Double_Days"]], OutbreakClass)
prediction_df["Outbreak_Class"] <- unlist(class_list)

inspect_1 <- prediction_df[which(prediction_df["Outbreak_Class"] == 1),]
inspect_2 <- prediction_df[which(prediction_df["Outbreak_Class"] == 2),]

# Give to jag

outbreak_df <- prediction_df[c("date","fips","county","state","Outbreak_Class")]
write.csv(outbreak_df,paste(mainDir,"allstates_","outbreak","_prediction.csv",sep=""),row.names=FALSE)
