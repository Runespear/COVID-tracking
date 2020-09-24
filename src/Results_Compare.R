closeAllConnections()
library(pacman)
p_load("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
p_load("zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr","here")

options(bitmapType='cairo')


# Set Working Directory to File source directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(file.path(here(),"/src/"))

#registerDoParallel(cores=detectCores())

#################################################################
# Compare results of Block v.s. lm and slm wrt MAPE and MSE
#################################################################
numtrees=2000

output.folder <- "../data/output"

block.mse.fname <- paste("block_mse_windowsize=2_numtrees=",toString(numtrees),".csv",sep="")
block.mape.fname <- paste("block_mape_windowsize=2_numtrees=",toString(numtrees),".csv",sep="")

block.mse.df <- read.csv(file.path(output.folder,block.mse.fname))
block.mape.df <- read.csv(file.path(output.folder,block.mape.fname))

block.mse.df <- subset(block.mse.df, select=-c(block.mse,block.mse.0))
block.mape.df <- subset(block.mape.df, select=-c(block.mape,block.mape.0))


block.mse.df <- na.omit(block.mse.df)
block.mape.df <- na.omit(block.mape.df)

mse.compare.df<-block.mse.df
mape.compare.df<-block.mape.df

max.mape.improvement <- c()
max.rmse.improvement <- c()

wsize.list <- c(2,4,8,16)

for(wsize in wsize.list){
  lm.mse.fname <- paste("mse_table_windowsize=",toString(wsize),".csv",sep="")
  lm.mape.fname <- paste("mape_table_windowsize=",toString(wsize),".csv",sep="")
  
  
  lm.mse.df <- read.csv(file.path(output.folder,lm.mse.fname))
  lm.mape.df <- read.csv(file.path(output.folder,lm.mape.fname))
  
  lm.mse.df <- subset(lm.mse.df, select=-c(slm.mse))
  names(lm.mse.df)[names(lm.mse.df) == "lm.mse"] <- paste("lm.mse.windowsize=",toString(wsize),sep="")
  lm.mape.df <- subset(lm.mape.df, select=-c(slm.mape))
  names(lm.mape.df)[names(lm.mape.df) == "lm.mape"] <- paste("lm.mape.windowsize=",toString(wsize),sep="")
  
  #lm.mse.df
  # Drop the NA
  
  
  lm.mse.df <- na.omit(lm.mse.df)
  lm.mape.df <- na.omit(lm.mape.df)
  
  mse.compare.df <- merge(x=mse.compare.df,y=lm.mse.df,by="cutoff")
  mape.compare.df <- merge(x=mape.compare.df,y=lm.mape.df,by="cutoff")
  
  # Add comparison column for rmse and mape (how much percentage decrease from lm to block.last)
  
  # Convert to RMSE  
  #mse.compare.df$compare.rmse <- 1 - sqrt(mse.compare.df$block.mse.last)/sqrt(mse.compare.df$lm.mse)
  #mape.compare.df$compare.mape <- 1 - mape.compare.df$block.mape.last/mape.compare.df$lm.mape
  
  #max.mape.improvement <- c(max.mape.improvement,max(mape.compare.df$compare.mape))
  #max.rmse.improvement <- c(max.rmse.improvement,max(mse.compare.df$compare.rmse))
  
  
  
}

#write.csv(mse.compare.df,file.path(output.folder,paste("mse_compare_windowsize=",toString(wsize),".csv",sep="")),row.names=FALSE)
#write.csv(mape.compare.df,file.path(output.folder,paste("mape_compare_windowsize=",toString(wsize),".csv",sep="")),row.names=FALSE)
write.csv(mse.compare.df,file.path(output.folder,paste("mse_block_lm_compare_numtrees=",toString(numtrees),".csv",sep="")),row.names=FALSE)
write.csv(mse.compare.df,file.path(output.folder,paste("mape_block_lm_compare_numtrees=",toString(numtrees),".csv",sep="")),row.names=FALSE)

#######################################################################
 # plot the lm.mse and lm.mape (2,4,8,16) v.s. block.last.mse and block.last.mape
#######################################################################

table.list <- list(mse.compare.df,mape.compare.df)
metric.list <- c("mse","mape")

for (i in c(1,2)){
  df.to.plot <- table.list[[i]]
  metric.to.plot <- metric.list[i]
  
  title <- paste("One week prediction ",metric.to.plot,sep="")
  
  days <- df.to.plot$cutoff
  MinDay <- min(days)
  MaxDay <- max(days)
  
  block.last.metric <- df.to.plot[,c(paste("block.",metric.to.plot,".last",sep=""))]
  legend.list <- c(paste("block.",metric.to.plot,".last",sep=""))
  
  cl <- rainbow(length(wsize.list + 1))
  png(paste("../data/output/",metric.to.plot,"_compare_lm_numtrees=",toString(numtrees),".png" ,sep="") )
  plot(days, block.last.metric, pch=19, col=rainbow[1], type="l", xlab="days", ylab=metric.to.plot, xlim=c(MinDay,MaxDay),ylim=c(0,0.1),xaxs="i",yaxs="i",lty=1, main=title)
  # Plot lm metrics
  for (j in 1:length(wsize.list)){
    
    lm.metric <- df.to.plot[,c(paste("lm.",metric.to.plot,".windowsize=",wsize.list[j],sep=""))]
    lines(days, lm.metric, col=cl[j+1], type=j+1)
    legend.list <- c(legend.list,paste("lm.",metric.to.plot,".windowsize=",wsize.list[j],sep=""))
  }
  legend(MinDay, 0.1, legend=legend.list, col=cl, lty=1:(wsize.list+1), cex=0.8)
  
  dev.off()
}

