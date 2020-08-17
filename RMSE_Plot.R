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
#source("county_analysis.R")

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
windowsize=7

filename_raw <- paste("mse_table",".csv",sep="")
filename <- file.path(mainDir,filename_raw)

block.filename <- paste("block_mse_windowsize=",toString(windowsize),".csv",sep="")
block.filename <- file.path(mainDir,block.filename)

block_df <- read.csv(block.filename)
block_df <- na.omit(block_df)

cutoff_df <- read.csv(file=filename)
cutoff_df <- na.omit(cutoff_df)
cutoff_df <- cutoff_df[,!(names(cutoff_df) %in% c("date.x"))]



restricted_state_df2 <- merge(x=cutoff_df,y=block_df,by="cutoff")



    
lm.mse.list <- sqrt(restricted_state_df2$lm.mse)
slm.mse.list <- sqrt(restricted_state_df2$slm.mse)
#grf.mse.list <- sqrt(restricted_state_df2$grf.mse)
#augmented.grf.mse.list <- sqrt(restricted_state_df2$augmented.grf.mse)
#fonly.grf.mse.list <- sqrt(restricted_state_df2$fonly.grf.mse)
block.grf.mse.list <- sqrt(restricted_state_df2$block.mse)
block.grf.mse.0.list <- sqrt(restricted_state_df2$block.mse.0)
block.grf.mse.last.list <- sqrt(restricted_state_df2$block.mse.last)

days<-restricted_state_df2$cutoff


title="One Week Prediction"

plot(days, lm.mse.list, pch=19, col="gray", type="l", xlab="days", ylab="RMSE", xlim=c(10,185),ylim=c(0,0.7),xaxs="i",yaxs="i", main=title)
#lines(days, grf.mse.list,pch=18, col="green", type="l", lty=2)
#lines(days, augmented.grf.mse.list,pch=18, col="blue", type="l", lty=3)
#lines(days, fonly.grf.mse.list,pch=18, col="orange", type="l", lty=4)
lines(days, slm.mse.list,pch=18, col="red", type="l", lty=5)
lines(days, block.grf.mse.list,pch=18, col="purple", type="l", lty=6)
lines(days, block.grf.mse.0.list,pch=18, col="magenta", type="l", lty=7)
lines(days, block.grf.mse.last.list,pch=18, col="cyan", type="l", lty=8)

legend(10, 0.7, legend=c("LM","SLM","GRF.block","GRF.block.0","GRF.block.last"), col=c("gray", "red","purple","magenta","cyan"), lty=1:6, cex=0.8)



closeAllConnections()


