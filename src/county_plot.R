require("pacman")
p_load("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
p_load("zoo", "dtw", "foreach", "evaluate","rlist","hash", "e1071","here")

# Set Working Directory to File source directory
setwd(file.path(here(),"src"))
#source("county_analysis.R")
registerDoParallel(cores=detectCores())

# Location of processed county data

mainDir <- "../data/output"
destfile <- paste("../data/augmented_us-counties-states_latest",".csv",sep="")

county_data <- read.csv(file = destfile)
#earliest_start <- min(county_data$days_from_start)
#earliest_start<-60
#latest_date <- max(county_data$days_from_start)
county_list <- sort(unique(county_data$fips))

windowsize=2

# Backtest directory
backtestDir <- file.path(mainDir,"backtest")

forest_backtestDir<-file.path(mainDir,paste0("backtest_state_forests_windowsize=",toString(windowsize)))

# read all the allstates_$(CUTOFF)_grf.csv files
#cutoff_list <- earliest_start:latest_date


# Place to save resuslts

CountyPlot<-file.path(mainDir,paste0("Backtest_by_County_Windowsize=",toString(windowsize)))
dir.create(CountyPlot)


CountyDir <- file.path(CountyPlot,"Backtest_by_County")
dir.create(CountyDir)

plotDir <- file.path(CountyPlot,"Backtest_by_County_plots")
dir.create(plotDir)

#DplotDir <- file.path(CountyPlot,"Backtest_by_County_Dplots")
#dir.create(DplotDir)

  for(c in county_list){
#foreach(c=county_list)%dopar%{
    
    check.file.name <- paste0(toString(c),"_backtest.csv")
    check.file.full.name <- file.path(CountyDir, check.file.name) 
    if (file.exists(check.file.full.name)){
      df <- read.csv(file = check.file.full.name)
      if (nrow(df)>7) {
      df<-head(df,-7)
      plot.prepare <- df[,-which(names(df) %in% c("D.r.lm",	"D.r.slm",	"D.tau.hat",	"B.D.r.lm",	"B.D.r.slm",	"B.D.tau.hat"))]
      # days_from_start.y in the backtest represents the future 7 days
      earliest_start<-max(plot.prepare$days_from_start.y)-7+1
      } else {plot.prepare <- data.frame("fips" = NA, "county"=NA, "state"=NA, "r.lm"=NA, "predicted.lm"=NA
                                         , "r.slm"=NA, "predicted.slm"=NA, "date.y"=NA, "days_from_start.y" = NA, "log_rolled_cases.y"=NA, "tau.hat"=NA, "predicted.grf.future.0"=NA, "predicted.grf.future.last"=NA)
      earliest_start<-60
      }
    }else{
      plot.prepare <- data.frame("fips" = NA, "county"=NA, "state"=NA, "r.lm"=NA, "predicted.lm"=NA
                               , "r.slm"=NA, "predicted.slm"=NA, "date.y"=NA, "days_from_start.y" = NA, "log_rolled_cases.y"=NA, "tau.hat"=NA, "predicted.grf.future.0"=NA, "predicted.grf.future.last"=NA)
      earliest_start<-60
      }
    
    latest_date <- max(county_data$days_from_start)
    cutoff_list <- earliest_start:latest_date
    
    for(cutoff in cutoff_list){
      filename_raw <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
      filename <- file.path(backtestDir,filename_raw)
      
      forest_filename_raw <- paste("block_results_",toString(cutoff),".csv",sep="")
      forest_filename <- file.path(forest_backtestDir,forest_filename_raw)
      
      if (file.exists(filename) ){
        # Read the file
        cutoff_df0 <- read.csv(file=filename)
        # Restrict to the county and variables of interest
        cutoff_df0 <- subset(cutoff_df0,fips == c)
        #print(names(cutoff_df))
        cutoff_df0<- cutoff_df0[,which(names(cutoff_df0) %in% c("fips", "date.y", "state", "county", "days_from_start.y", "predicted.lm"
                                                             , "predicted.slm", "predicted.grf", "predicted.grf.augmented", "predicted.grf.fonly"
                                                             ,"log_rolled_cases.y", "r.lm", "r.slm"))]
        if(file.exists(forest_filename)){
        # Read the file
        forest_cutoff_df <- read.csv(file=forest_filename)
        # Restrict to the county and variables of interest
        forest_cutoff_df <- subset(forest_cutoff_df,fips == c)
        forest_cutoff_df<- forest_cutoff_df[,which(names(forest_cutoff_df) %in% c("fips", "predicted.grf.future.0", "predicted.grf.future.last","tau.hat"))]
        }else{
        forest_cutoff_df<- cutoff_df0[,which(names(cutoff_df0) %in% c("fips", "date.y"))]
        #}
        forest_cutoff_df[,"tau.hat"]<-NA
        forest_cutoff_df[,"predicted.grf.future.0"]<-NA
        forest_cutoff_df[,"predicted.grf.future.last"]<-NA
        forest_cutoff_df<-select(forest_cutoff_df,-c(date.y))}
        # Merge files
        
        cutoff_df<-merge(x=cutoff_df0,y=forest_cutoff_df, by="fips",x.all=TRUE)
        
        # Drop NAs
       # cutoff_df <- na.omit(cutoff_df)
        #if(nrow(cutoff_df)==0){
        #  next
        #}
        # Concatenate data frames
        plot.prepare<- rbind(plot.prepare,cutoff_df)
      }
    }
    plot.prepare<-plot.prepare %>% filter_all(any_vars(!is.na(.)))
  #  plot.prepare<-na.omit(plot.prepare)
    if(nrow(plot.prepare)==0){
       next
    }
    
    
    plot.prepare<- plot.prepare %>% mutate(D.r.lm=r.lm-lag(r.lm),D.r.slm=r.slm-lag(r.slm), D.tau.hat=tau.hat-lag(tau.hat))
    
    plot.prepare$B.D.r.lm[plot.prepare$D.r.lm>0]<-1
    plot.prepare$B.D.r.lm[plot.prepare$D.r.lm<0]<- -1
    plot.prepare$B.D.r.lm[plot.prepare$D.r.lm==0]<- 0
    
    plot.prepare$B.D.r.slm[plot.prepare$D.r.slm>0]<-1
    plot.prepare$B.D.r.slm[plot.prepare$D.r.slm<0]<- -1
    plot.prepare$B.D.r.slm[plot.prepare$D.r.slm==0]<- 0
    
    plot.prepare$B.D.tau.hat[plot.prepare$D.tau.hat>0]<-1
    plot.prepare$B.D.tau.hat[plot.prepare$D.tau.hat<0]<- -1
    plot.prepare$B.D.tau.hat[plot.prepare$D.tau.hat==0]<- 0
    
    #plot.prepare1<- plot.prepare0[,which(names(plot.prepare0) %in% c("fips","tau.hat","r.lm","r.slm"))]
    #plot.prepare1 <- plot.prepare1[-1,] - plot.prepare1[-nrow(plot.prepare1),]
    #plot.prepare1 <-plot.prepare1 %>% rename( D.r.lm=r.lm, D.r.slm=r.slm, D.tau.hat=tau.hat)
    
    #plot.prepare<-merge(x=plot.prepare0,y=plot.prepare1, by="fips",x.all=TRUE)
    
    MaxCase<-max(plot.prepare$predicted.grf.future.last)+1
    MinCase<-min(plot.prepare$predicted.grf.future.last)-2
    MaxDay<-max(plot.prepare$days_from_start.y)
    MinDay<-min(plot.prepare$days_from_start.y)
    
    performance_file_path = file.path(CountyDir, paste(toString(c),"_backtest.csv",sep=""))
    write.csv(plot.prepare,performance_file_path,row.names=FALSE)
    
    print(paste("Finished writing backtest for ",toString(plot.prepare$county[1])," county, ",toString(plot.prepare$state[1]),setp=""))
    
    #plot_file_path= file.path(plotDir, paste(toString(c),"_plot.png",sep=""))
    #png(plot_file_path, width = 1080, height = 720))
    
    png(paste("../data/output/",paste0("Backtest_by_County_Windowsize=",toString(windowsize)),"/Backtest_by_County_plots/",toString(c),"_plot.png",sep=""), width = 1080, height = 720)
    
    title=paste("One Week Prediction","(",toString(plot.prepare$county[1])," county, ",toString(plot.prepare$state[1]),")",sep="")
    
    plot(plot.prepare$days_from_start.y, plot.prepare$predicted.lm,pch=19, col="blue", type="b", xlab="days", ylab="Log Case Number", xlim=c(MinDay,MaxDay),ylim=c(MinCase,MaxCase),xaxs="i",yaxs="i", main=title)
    lines(plot.prepare$days_from_start.y, plot.prepare$predicted.slm,pch=18, col="green", type="b", lty=2)
    lines(plot.prepare$days_from_start.y, plot.prepare$predicted.grf.future.last, pch=17, col="red", type="b",lty=3)
    #lines(plot.prepare$days_from_start.y, plot.prepare$predicted.grf.future.0, pch=16, col="red", type="b",lty=4)
    lines(plot.prepare$days_from_start.y, plot.prepare$log_rolled_cases.y, pch=15, col="black", type="b",lty=4)
    legend(MinDay, MaxCase, legend=c("Predicted by LM", "Predicted by SLM", "Predicted by Block GRF Last","Actual Case Number"), col=c("blue", "green", "red","black"), lty=1:4, cex=0.8)
    
    dev.off()
  
    # r Difference plot
    
    #plot.prepare<-na.omit(plot.prepare)
    
    #MaxDr<-max(plot.prepare$D.r.lm)*1.1
    #MinDr<-min(plot.prepare$D.r.lm)*1.1
    #MaxDay<-max(plot.prepare$days_from_start.y)
    #MinDay<-min(plot.prepare$days_from_start.y)
    
    #png(paste("../data/output/",paste0("Backtest_by_County_Windowsize=",toString(windowsize)),"/Backtest_by_County_Dplots/",toString(c),"_plot.png",sep=""), width = 1080, height = 720)
    
    #title=paste("One Week Prediction","(",toString(cutoff_df$county)," county, ",toString(cutoff_df$state),")",sep="")
    
    #plot(plot.prepare$days_from_start.y, plot.prepare$D.r.lm,pch=19, col="gray", type="b", xlab="days", ylab="Log Case Number", xlim=c(MinDr,MaxDr),ylim=c(MinCase,MaxCase),xaxs="i",yaxs="i", main=title)
    #lines(plot.prepare$days_from_start.y, plot.prepare$D.r.slm,pch=18, col="blue", type="b", lty=2)
    #lines(plot.prepare$days_from_start.y, plot.prepare$D.tau.hat, pch=17, col="red", type="b",lty=3)
    #legend(MinDay, MaxCase, legend=c("Predicted by LM", "Predicted by SLM", "Predicted by Block GRF"), col=c("gray", "blue", "red"), lty=1:3, cex=0.8)
    
    #dev.off()
    
    
    print(paste("Finished ploting backtest for ",toString(plot.prepare$county[1])," county, ",toString(plot.prepare$state[1]),setp=""))
}

