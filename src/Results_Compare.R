closeAllConnections()
library(pacman)
p_load("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
p_load("zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr","here")


lapply(list.of.packages, require, character.only = TRUE)
options(bitmapType='cairo')


# Set Working Directory to File source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(file.path(here(),"/src/"))

#registerDoParallel(cores=detectCores())

#################################################################
# Compare results of Block v.s. lm and slm wrt MAPE and MSE
#################################################################

output.folder <- "../data/output"

block.mse.fname <- "block_mse_windowsize=2.csv"
block.mape.fname <- "block_mape_windowsize=2.csv"

block.mse.df <- read.csv(file.path(output.folder,block.mse.fname))
block.mape.df <- read.csv(file.path(output.folder,block.mape.fname))
block.mse.df <- na.omit(block.mse.df)
block.mape.df <- na.omit(block.mape.df)

max.mape.improvement <- c()
max.rmse.improvement <- c()

for(wsize in c(2,4,8,16)){
  lm.mse.fname <- paste("mse_table_windowsize=",toString(wsize),".csv",sep="")
  lm.mape.fname <- paste("mape_table_windowsize=",toString(wsize),".csv",sep="")
  
  
  lm.mse.df <- read.csv(file.path(output.folder,lm.mse.fname))
  lm.mape.df <- read.csv(file.path(output.folder,lm.mape.fname))
  
  # Drop the NA
  
  
  lm.mse.df <- na.omit(lm.mse.df)
  lm.mape.df <- na.omit(lm.mape.df)
  
  mse.compare.df <- merge(x=block.mse.df,y=lm.mse.df,by="cutoff")
  mape.compare.df <- merge(x=block.mape.df,y=lm.mape.df,by="cutoff")
  
  # Add comparison column for rmse and mape (how much percentage decrease from lm to block.last)
  
  # Convert to RMSE  
  mse.compare.df$compare.rmse <- 1 - sqrt(mse.compare.df$block.mse.last)/sqrt(mse.compare.df$lm.mse)
  mape.compare.df$compare.mape <- 1 - mape.compare.df$block.mape.last/mape.compare.df$lm.mape
  
  max.mape.improvement <- c(max.mape.improvement,max(mape.compare.df$compare.mape))
  max.rmse.improvement <- c(max.rmse.improvement,max(mse.compare.df$compare.rmse))
  
  
  write.csv(mse.compare.df,file.path(output.folder,paste("mse_compare_windowsize=",toString(wsize),".csv",sep="")),row.names=FALSE)
  write.csv(mape.compare.df,file.path(output.folder,paste("mape_compare_windowsize=",toString(wsize),".csv",sep="")),row.names=FALSE)
  
}






