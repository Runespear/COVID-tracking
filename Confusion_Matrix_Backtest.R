list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","hash")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)



# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis.R")

registerDoParallel(cores=detectCores())


# Define Classes

PredictedOutbreakClass <- function(Double_Days.col,DD.list){
  # DD.list contains the list of benchmark doubling days e.g. c(7,14,21,28)
  # Double_Days.col is the column of Double_Days from the dataframe
  Helper <- function(Double_Days){
    Dlist <- c(DD.list, Inf)
    
    numClasses <- length(DD.list) + 1
    # Nonsense prediction
    if (Double_Days <= 0){
      return(0)
    }
    for (i in 1:numClasses){
      if(Double_Days <= Dlist[i]){
        return(numClasses - i)
      }
    }
  }
  return(lapply(Double_Days.col,Helper))
}

#qq <- mapply(minimum_distance, df$a, df$b, df$c)

ActualOutbreakClass <- function(log_rolled_cases.x.col,log_rolled_cases.y.col, DD.list, predictionsize){
  #DD.list <- c(7,14,21,28)
  Helper <- function(log_rolled_cases.x,log_rolled_cases.y){
    
    rolled_cases.x <- exp(log_rolled_cases.x)
    rolled_cases.y <- exp(log_rolled_cases.y)
      
    #print(rolled_cases.x)
    
    rate.list <- log(2,exp(1))/DD.list
    rate.list <- c(rate.list,0)
    # Upper and lower bounds per class from now
    # Inf > r7 > r14 > r21 > r28 > 0
    # rate.list := c(r7,r14,r21,r28,0)
    cases.bound.list <- rolled_cases.x*exp(rate.list*predictionsize)
    
    numClasses <- length(DD.list) + 1
    for (i in 1:numClasses){
      if (rolled_cases.y >= cases.bound.list[i]){
        return(numClasses - i)
      }
    }
    return(0)
  }
  return(mapply(Helper,log_rolled_cases.x.col,log_rolled_cases.y.col))
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
#cutoff_list <- c(174)

# List of dataframes
df_list <- c()
# List of existing cutoffs
actual_cutoff_list <- c()
# List of doubling days
DD.list <- c(7,14,21,28)
NumClasses <- length(DD.list) + 1
predictionsize <- 7

cutoff_df <- NULL
prediction_df <- NULL
CM <- NULL
CM.hash <- hash()

mse <- NULL
mse.hash <- hash()

for(cutoff in cutoff_list){
  filename <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
  filename <- file.path(backtestDir,filename)
  # Check if file exists
  print(filename)
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
    prediction_df["Predicted_Double_Days"] <- log(2,exp(1))/cutoff_df["r.grf"]
    class_list <- PredictedOutbreakClass(prediction_df$Predicted_Double_Days, DD.list)
    # Generate the predicted labels for each county
    prediction_df["Predicted_Outbreak_Class"] <- unlist(class_list)
    
    # Calculate the actual labels of each county 7 days later
    actual_class_list <- (ActualOutbreakClass(prediction_df$log_rolled_cases.x,prediction_df$log_rolled_cases.y,DD.list,predictionsize))
    prediction_df["Actual_Outbreak_Class"] <- actual_class_list
    
    # Calculate MSE of grf
    mse <- sum(prediction_df$grf_mse)/length(prediction_df$grf_mse)
    mse.hash[[toString(cutoff)]] <- mse
    
    level.list <- 0:(NumClasses-1)
    X <- factor(prediction_df$Predicted_Outbreak_Class,levels = level.list)
    Y <- factor(prediction_df$Actual_Outbreak_Class, levels = level.list)
    
    CM <- confusionMatrix(X,Y)
    CM.hash[[toString(cutoff)]] <- (CM)
    
    # Add into actual_cutoff_list
    actual_cutoff_list = c(actual_cutoff_list,cutoff)
    #break
  }
  
}


# Compute the predicted class and actual class

closeAllConnections()


