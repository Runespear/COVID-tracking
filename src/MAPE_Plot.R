closeAllConnections()
require("pacman")
p_load("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
p_load("zoo", "dtw", "foreach", "evaluate","rlist","data.table","here")


setwd(file.path(here(),"src"))
#source("county_analysis.R")

registerDoParallel(cores=detectCores())

options(bitmapType='cairo')

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
numtrees=2000

filename_raw <- paste("mape_table",".csv",sep="")
filename <- file.path(mainDir,filename_raw)

block.filename <- paste("block_mape_windowsize=",toString(windowsize),"_numtrees=",toString(numtrees),".csv",sep="")
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

png(paste("../data/output/","MAPE_windowsize=",toString(windowsize),"_numtrees=",toString(numtrees),"_plot.png",sep=""), width = 1080, height = 720)

title="One Week Prediction"

plot(days, lm.mape.list, pch=19, col="blue", type="l", xlab="days", ylab="MAPE", xlim=c(MinDay,MaxDay),ylim=c(0,0.1),xaxs="i",yaxs="i", main=title)
lines(days, slm.mape.list,pch=18, col="green", type="l", lty=2)
#lines(days, block.grf.mape.list,pch=18, col="purple", type="l", lty=6)
#lines(days, block.grf.mape.0.list,pch=18, col="magenta", type="l", lty=7)
lines(days, block.grf.mape.last.list,pch=18, col="red", type="l", lty=3)

legend(MinDay, 0.1, legend=c("LM","SLM","GRF.block.last"), col=c("blue", "green", "red"), lty=1:3, cex=0.8)

dev.off()


closeAllConnections()


