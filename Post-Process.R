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

# Load county_fips_master.csv

fips.master <- read.csv("./data/county_fips_master.csv")



fips.master$county <- fips.master$county_name
fips.master$state <- fips.master$state_name
fips.master <-  fips.master[c("fips","county","state")]
fips.master$predicted.grf.augmented <- NA
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
for (x in filelist){
  print(x)
  
  t <- read.csv(file.path(backtest.folder,x))
  
  m <- NULL
  
  cdata <- subset(county_data, days_from_start == max(county_data$days_from_start))
  cdata <- na.omit(cdata[c("fips","county","state")])
  cdata$FIPS.STRING <- mapply(prepend, cdata$fips)
  
  try(m <- t[c("fips","days_from_start.x","date.x","county","state","predicted.grf.augmented","Predicted_Double_Days")])
  
  if (is.null(m)){
    m <- t[c("fips","days_from_start","date","county","state","predicted.grf.augmented","Predicted_Double_Days")]
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
    
  
  
  cdata$predicted.grf.augmented <- NA
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
  
  m<-m[order(m$fips),]
  
  # Write file in ./data/output/confusion/ folder
  destfolder <- "./data/output/confusion/"
  fname <- paste("confusion_",x,sep="")
  write.csv(m, file.path(destfolder,fname),row.names=FALSE)
  # break
}

