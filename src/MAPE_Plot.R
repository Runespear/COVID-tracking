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



# Location of mape data

mainDir <- "../data/output"
#destfile <- paste("../data/processed_us-counties_latest",".csv",sep="")

#county_data <- read.csv(file = destfile)



# Backtest directory
#backtestDir <- file.path(mainDir,"backtest")


#cutoff = 171
#mainDir = "../data/output"
#subDir = "backtest"
#backtest_dir = file.path(mainDir, subDir)
#dir.create(backtest_dir)
windowsize=2

filename_raw <- paste("mape_table",".csv",sep="")
filename <- file.path(mainDir,filename_raw)

block.filename <- paste("block_mape_windowsize=",toString(windowsize),".csv",sep="")
block.filename <- file.path(mainDir,block.filename)

block_df <- read.csv(block.filename)
block_df <- na.omit(block_df)

cutoff_df <- read.csv(file=filename)
cutoff_df <- na.omit(cutoff_df)
cutoff_df <- cutoff_df[,!(names(cutoff_df) %in% c("date.x"))]



restricted_state_df2 <- merge(x=cutoff_df,y=block_df,by="cutoff", all.y=TRUE)




lm.mape.list <- restricted_state_df2$lm.mape
slm.mape.list <- restricted_state_df2$slm.mape

block.grf.mape.list <- restricted_state_df2$block.mape
block.grf.mape.0.list <- restricted_state_df2$block.mape.0
block.grf.mape.last.list <- restricted_state_df2$block.mape.last

days<-restricted_state_df2$cutoff


MaxDay<-max(days)
MinDay<-min(days)

png(paste("../data/output/","MAPE_windowsize=",toString(windowsize),"_plot.png",sep=""), width = 1080, height = 720)

title="One Week Prediction"

plot(days, lm.mape.list, pch=19, col="blue", type="l", xlab="days", ylab="MAPE", xlim=c(MinDay,MaxDay),ylim=c(0,0.3),xaxs="i",yaxs="i", main=title)
lines(days, slm.mape.list,pch=18, col="green", type="l", lty=2)
#lines(days, block.grf.mape.list,pch=18, col="purple", type="l", lty=6)
#lines(days, block.grf.mape.0.list,pch=18, col="magenta", type="l", lty=7)
lines(days, block.grf.mape.last.list,pch=18, col="red", type="l", lty=3)

legend(MinDay, 0.3, legend=c("LM","SLM","GRF.block.last"), col=c("blue", "green", "red"), lty=1:3, cex=0.8)

dev.off()


closeAllConnections()


