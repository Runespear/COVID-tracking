closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#install.packages("RApiDatetime", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("grf", repos="http://cran.rstudio.com/", dependencies=TRUE)

#install.packages("rattle", repos="http://cran.rstudio.com/", dependencies=TRUE)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=detectCores())

#########################################################
# PREPEND FUNCTION FOR FIPS
#########################################################

prepend <- function(fips){
  FIPS.STRING <- toString(fips)
  if (nchar(FIPS.STRING)<5){
    FIPS.STRING <- paste("0",FIPS.STRING,sep="")
  }
  FIPS.STRING <- paste("'",FIPS.STRING,sep="")
  return(FIPS.STRING)
}

#########################################################
# IMPUTE PREDICTED CASES AND DOUBLING DAYS WITH NA     ##
#########################################################

# Load the non augmented county data

destfile = paste("./data/processed_us-counties_latest",".csv",sep="")
county_data <- read.csv(file = destfile)
county_data$date <- anytime::anydate(county_data$date)

start_date = min(county_data$date)
end_date = max(county_data$date)

county_data$days_from_start <- as.numeric(county_data$date- start_date , units="days")
county_data$logcases <- log(county_data$cases)

county_data$log_rolled_cases <- log(county_data$rolled_cases)

earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)


# Load county_fips_master.csv

fips.master <- read.csv("./data/county_fips_master.csv")



fips.master$county <- fips.master$county_name
fips.master$state <- fips.master$state_name
fips.master <-  fips.master[c("fips","county","state")]
fips.master$predicted.grf <- NA
fips.master$Predicted_Double_Days <- NA
fips.master$FIPS.STRING <- mapply(prepend, fips.master$fips)



# Loop through files in ./data/output/backtest

backtest.folder <- "data/output/backtest"
filelist <- list.files(path=backtest.folder, pattern="*.csv", full.names=FALSE, recursive=FALSE)

fips_all <- sort(unique(county_data$fips))

t <- NULL
cdata <- NULL
mdata <- NULL
t.fips.list <- NULL

cutoff <- NA
cutoff.list <- earliest_start:latest_date



for (cutoff in cutoff.list){
  
  fname <- paste("allstates_",toString(cutoff),"_grf.csv",sep="")
  
  
  
  full.path <-file.path(backtest.folder,fname)
  
  if (file.exists(full.path)){
    t <- read.csv(full.path)
  }
  else{
    next
  }
  
  print(fname)
  
  m <- NULL
  
  cdata <- subset(county_data, days_from_start == max(county_data$days_from_start))
  cdata <- na.omit(cdata[c("fips","county","state","log_rolled_cases")])
  cdata$FIPS.STRING <- mapply(prepend, cdata$fips)
  
  try(m <- t[c("fips","days_from_start.x","date.x","county","state","log_rolled_cases.x")])
  
  if (is.null(m)){
    m <- t[c("fips","days_from_start","date","county","state","log_rolled_cases")]
    cdata$days_from_start <- unique(m$days_from_start)
    cdata$date <- unique(m$date)
    
    fips.master$days_from_start <- unique(m$days_from_start)
    fips.master$date <- unique(m$date)
  }
  else{
    cdata$days_from_start.x <- unique(m$days_from_start.x)
    cdata$date.x <- unique(m$date.x)
    
    fips.master$days_from_start.x <- unique(m$days_from_start.x)
    fips.master$date.x <- unique(m$date.x)
  }
  
  
  m$FIPS.STRING <- mapply(prepend, m$fips)
    
  
  
  cdata$predicted.grf <- NA
  cdata$Predicted_Double_Days <- NA
  
  m.fips.list <- sort(unique(m$fips))
  
  for (fips in fips_all){
    if (! fips %in% m.fips.list){
      m <- dplyr::bind_rows(m, cdata[which(cdata$fips==fips),])
      m.fips.list <- append(m.fips.list, fips)
    }
  }
  for (fips in unique(fips.master$fips)){
    if (! fips %in% m.fips.list){
      m <- dplyr::bind_rows(m, fips.master[which(fips.master$fips==fips),])
      m.fips.list <- append(m.fips.list, fips)
    }
  }
  
  # Merge missing
  missing.fips <- read.csv("./data/missing-fips.csv")
  m.test <- merge(x=m,y=missing.fips,all=TRUE)
  m.test[which(is.na(m.test$days_from_start.x)),"days_from_start.x"] <- unique(m$days_from_start.x)
  m.test[which(is.na(m.test$date.x)),"date.x"] <- unique(m$date.x)
  m.test <- subset(m.test, select=-c(STATEFP,FIPS.STRING))
  m <- m.test
  
  
  m<-m[order(m$fips),]
  # Add actual number of cases currently
  m$log_rolled_cases.x[is.na(m$log_rolled_cases.x)] <- m$log_rolled_cases[is.na(m$log_rolled_cases.x)]
  m <- subset(m, select = -c(log_rolled_cases))
  
  # Check to see if prediction of today's cases 7 days ago is present, if yes, get them
  past.fname <- paste("allstates_",toString(cutoff-7),"_grf.csv",sep="")
  
  
  
  past.full.path <-file.path(backtest.folder,past.fname)
  m.new <- m
  
  
  if (file.exists(past.full.path)){
    past.t <- read.csv(past.full.path)
    print(paste("Past data exists as ",toString(cutoff-7),sep=""))
    # Add in prediction of today from past  
    
    past.t <- past.t[c("fips","predicted.grf")]
    
    m.new <- merge(x=m,y=past.t,by="fips",all=TRUE)
    
    m.new <-rename(m.new, c("predicted.grf.x"="predicted.grf.future"))
    m.new <-rename(m.new, c("predicted.grf.y"="predicted.grf.past"))
    
  }
  else{
    print(paste("No past data exists as ",toString(cutoff-7),sep=""))
    
    m.new <-rename(m, c("predicted.grf"="predicted.grf.future"))
    m.new$predicted.grf.past <- NA
  }
  

  # Write file in ./data/output/confusion/ folder
  destfolder <- "./data/output/confusion/"
  fname <- paste("confusion_",fname,sep="")
  write.csv(m.new, file.path(destfolder,fname),row.names=FALSE)
  
  #break
  
}
# Latest
fname <- "confusion_allstates_latest_grf.csv"

write.csv(m.new, file.path(destfolder,fname),row.names=FALSE)