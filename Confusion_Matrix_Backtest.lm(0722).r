list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","hash", "e1071")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)



# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("county_analysis.R")

registerDoParallel(cores=detectCores())


# Define Classes

#PredictedOutbreakClass <- function(Double_Days.col,DD.list){
  # DD.list contains the list of benchmark doubling days e.g. c(7,14,21,28)
  # Double_Days.col is the column of Double_Days from the dataframe
#  Helper <- function(Double_Days){
#    Dlist <- c(DD.list, Inf)
    
#    numClasses <- length(DD.list) + 1
    # Nonsense prediction
#    if (Double_Days <= 0){
#      return(0)
#    }
#    for (i in 1:numClasses){
#      if(Double_Days <= Dlist[i]){
#        return(numClasses - i)
#      }
#    }
#  }
#  return(lapply(Double_Days.col,Helper))
#}


PredictedOutbreakClass <- function(r.x.col, DD.list){
  #DD.list <- c(7,14,21,28)
  Helper <- function(r.x){
    
    #rolled_cases.x <- exp(log_rolled_cases.x)
    #rolled_cases.y <- exp(log_rolled_cases.y)
    
    #print(rolled_cases.x)
    
    rate.list <- log(2,exp(1))/DD.list
    rate.list <- c(rate.list,0)
    # Upper and lower bounds per class from now
    # Inf > r7 > r14 > r21 > r28 > 0
    # rate.list := c(r7,r14,r21,r28,0)
    #cases.bound.list <- rolled_cases.x*exp(rate.list*predictionsize)
    
#    cases.bound.list <- log_rolled_cases.x + predictionsize*rate.list
    
    numClasses <- length(DD.list) + 1
    for (i in 1:numClasses){
      if (r.x >= rate.list[i]){
        return(numClasses - i)
      }
    }
    return(0)
  }
  return(mapply(Helper,r.x.col))
}






#qq <- mapply(minimum_distance, df$a, df$b, df$c)

ActualOutbreakClass <- function(log_rolled_cases.x.col, DD.list, predictionsize, r.y.col){
  #DD.list <- c(7,14,21,28)
  Helper <- function(log_rolled_cases.x,r.y){
    
    #rolled_cases.x <- exp(log_rolled_cases.x)
    #rolled_cases.y <- exp(log_rolled_cases.y)
      
    #print(rolled_cases.x)
    
    rate.list <- log(2,exp(1))/DD.list
    rate.list <- c(rate.list,0)
    # Upper and lower bounds per class from now
    # Inf > r7 > r14 > r21 > r28 > 0
    # rate.list := c(r7,r14,r21,r28,0)
    #cases.bound.list <- rolled_cases.x*exp(rate.list*predictionsize)
    
    cases.bound.list <- log_rolled_cases.x + predictionsize*rate.list
    
    numClasses <- length(DD.list) + 1
    for (i in 1:numClasses){
      if (r.y >= rate.list[i]){
        return(numClasses - i)
      }
    }
    return(0)
  }
  return(mapply(Helper,log_rolled_cases.x.col,r.y.col))
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
DD.list <- c(14,28)
NumClasses <- length(DD.list) + 1
predictionsize <- 7

cutoff_df <- NULL
prediction_df <- NULL
CM <- NULL
CM.hash <- hash()

mse <- NULL
mse.hash <- hash()

# Place to save resuslts
confusionDir <- file.path(mainDir,"confusion")
dir.create(confusionDir)


days<-list()

acc<-list()

nr<-114

preci<-matrix(0,nrow=nr,ncol=NumClasses)

recall<-matrix(0,nrow=nr,ncol=NumClasses)

f1<-matrix(0,nrow=nr,ncol=NumClasses)

d<-1


for(cutoff in cutoff_list){
  filename_raw <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
  filename <- file.path(backtestDir,filename_raw)
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
    class_list <- PredictedOutbreakClass(prediction_df$r.x, DD.list)
    # Generate the predicted labels for each county
    prediction_df["Predicted_Outbreak_Class"] <- unlist(class_list)
    
    # Calculate the actual labels of each county 7 days later
    actual_class_list <- (ActualOutbreakClass(prediction_df$log_rolled_cases.x,DD.list,predictionsize,prediction_df$r.y))
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
    # Write the actual and predicted classes to separate folder
    
    confusion_filename_raw <- paste("confusion_",filename_raw,sep="")
    confusion.file.path <- file.path(confusionDir, confusion_filename_raw)
    write.csv(prediction_df,confusion.file.path,row.names=FALSE)
    days<-list.append(days,cutoff)
    acc<-list.append(acc,CM$overall['Accuracy'])
    
    for(i in (1:NumClasses)){preci[d,i]<-CM[["byClass"]][i,"Precision"]}
    
    for(i in (1:NumClasses)){recall[d,i]<-CM[["byClass"]][i,"Recall"]}
    
    for(i in (1:NumClasses)){f1[d,i]<-CM[["byClass"]][i,"F1"]}
    
    d<-d+1
    
  }
  
}

CM.hash[["100"]]

CM.hash[["100"]][["byClass"]][,"F1"]

CM.hash[["100"]][["byClass"]][1,"F1"]

CM.hash[["100"]]$byClass 



plot(days,recall[,1], type="b", pch=19, col="gray", xlab="days", ylab="Recall",xlim=c(0,180), ylim=c(0,1), xaxs="i", yaxs="i")

plot(days,f1[,1], type="b", pch=19, col="gray", xlab="days", ylab="F1",xlim=c(0,180), ylim=c(0,1), xaxs="i", yaxs="i")

#for(i in (2:5)){lines(days,preci[,i])}

#par(xaxs="i", yaxs="i")

lines(days,f1[,2], pch=18, col="orange", type="b", lty=2)

lines(days,f1[,3], pch=17, col="red", type="b", lty=3)

#lines(days,f1[,4], pch=16, col="pink", type="b", lty=4)

#lines(days,f1[,5], pch=15, col="red", type="b", lty=5)

legend(0, 0.8, legend=c("Class 0", "Class 1", "Class 2"),
       col=c("gray", "orange", "red"), lty=1:3, cex=0.8)
# Compute the predicted class and actual class

closeAllConnections()


