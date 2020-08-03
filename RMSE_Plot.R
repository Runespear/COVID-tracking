closeAllConnections()
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



# Location of MSE data

mainDir <- "./data/output"
#destfile <- paste("./data/processed_us-counties_latest",".csv",sep="")

#county_data <- read.csv(file = destfile)



# Backtest directory
#backtestDir <- file.path(mainDir,"backtest")


#cutoff = 171
#mainDir = "./data/output"
#subDir = "backtest"
#backtest_dir = file.path(mainDir, subDir)
#dir.create(backtest_dir)


filename_raw <- paste("mse_table",".csv",sep="")
filename <- file.path(mainDir,filename_raw)

cutoff_df <- read.csv(file=filename)
cutoff_df <- na.omit(cutoff_df)
restricted_state_df2 <- cutoff_df


    
lm.mse.list <- sqrt(restricted_state_df2$lm.mse)
slm.mse.list <- sqrt(restricted_state_df2$slm.mse)
grf.mse.list <- sqrt(restricted_state_df2$grf.mse)
augmented.grf.mse.list <- sqrt(restricted_state_df2$augmented.grf.mse)
fonly.grf.mse.list <- sqrt(restricted_state_df2$fonly.grf.mse)
days<-restricted_state_df2$cutoff


title="One Week Prediction"

plot(days, lm.mse.list, pch=19, col="gray", type="l", xlab="days", ylab="RMSE", xlim=c(60,185),ylim=c(0,0.6),xaxs="i",yaxs="i", main=title)
lines(days, grf.mse.list,pch=18, col="green", type="l", lty=2)
lines(days, augmented.grf.mse.list,pch=18, col="blue", type="l", lty=3)
lines(days, fonly.grf.mse.list,pch=18, col="white", type="l", lty=4)
lines(days, slm.mse.list,pch=18, col="red", type="l", lty=5)
legend(60, 0.5, legend=c("LM","GRF","GRF.AllFeatures","GRF.CountyFeatures","SLM"), col=c("gray", "green", "blue","white","red"), lty=1:5, cex=0.8)



closeAllConnections()

