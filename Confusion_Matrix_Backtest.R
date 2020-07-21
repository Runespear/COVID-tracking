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

PredictedOutbreakClass <- function(Double_Days){
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

ActualOutbreakClass <- function(){
  
}

# Location of processed county data

mainDir <- "./data/output"
destfile <- paste("./data/processed_us-counties_latest",".csv",sep="")

county_data <- read.csv(file = destfile)
earliest_start <- min(county_data$days_from_start)
latest_date <- max(county_data$days_from_start)


# Backtest directory
backtestDir <- file.path(mainDir,"backtest")

# read all the allstates_$(CUTOFF)_grf.csv files
cutoff_list <- earliest_start:latest_date

# List of dataframes
df_list <- c()
# List of existing cutoffs
actual_cutoff_list <- c()

for(cutoff in cutoff_list){
  filename <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
  filename <- file.path(backtestDir,filename)
  # Check if file exists
  if (file.exists(filename)){
    # Read the file
    cutoff_df <- read.csv(file=filename)
    # Drop NAs
    cutoff_df <- na.omit(cutoff_df)
    if(is.null(cutoff_df)){
      next
    }
    # Analyze the backtest dataset
    prediction_df <- cutoff_df
    # Calculate the doubling days from r.grf
    prediction_df["Double_Days"] <- log(2,exp(1))/cutoff_df["r.grf"]
    class_list <- lapply(prediction_df[["Double_Days"]], PredictedOutbreakClass)
    # Generate the predicted labels for each county
    prediction_df["Predicted_Outbreak_Class"] <- unlist(class_list)
    
    # Calculate the actual labels of each county 7 days later
    
    # Add into actual_cutoff_list
    actual_cutoff_list = c(actual_cutoff_list,cutoff)
  }
  
}


# Compute the predicted class and actual class

closeAllConnections()


