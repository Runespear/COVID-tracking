list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
list.of.packages <- c(list.of.packages, "zoo","usmap","readxl","lubridate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# URL of NYTimes Data
#nyt_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

destfile <- paste("../data/us-counties_latest",".csv",sep="")
#county_data <- read.csv(nyt_url)
#write.csv(county_data, destfile, row.names=FALSE)
# Pre-processing the data

county_data <- read.csv(file = destfile)
county_data$datetime <- as.Date(county_data$date)
county_data$date <- as.Date(county_data$date)

# CONVERY NYC fips from NA -> 99999

county_data[which(county_data$county=="New York City"),"fips"] <- 99999

# Find the earliest date and latest dates

start_date = min(county_data$datetime)
end_date = max(county_data$datetime)

# Add Columns of days from start

county_data$days_from_start <- as.numeric(county_data$datetime- start_date , units="days")


# Obtain list of fips

fips_list = sort(unique(county_data$fips))


# get 22-day incident cases

for (fips in fips_list){
  
  fips.df <- county_data[which(county_data$fips==fips),]
  if (dim(fips.df)[1] == 0){
    print(paste("fips ",toString(fips)," has no entry ",sep=""))
    next
  }
  first.fips.date <- min(fips.df$days_from_start)
  last.fips.date <- max(fips.df$days_from_start)
  
  if(first.fips.date == last.fips.date){
    print(paste("fips ",toString(fips)," only has one entry ",sep=""))
    next
  }
  for (day in (first.fips.date+1):last.fips.date){
    print(day)
    county.day.slice <- fips.df[which(fips.df$days_from_start == day),]
    if (dim(county.day.slice)[1] == 0){
      # Missing days inbetween e.g. fips 31057 day 184 jumps to 189
      print(paste("imputing for day ",toString(day)," of fips ",toString(fips),sep=""))
      imputter <- fips.df[which(fips.df$days_from_start == day-1),]
      # Change the date
      imputter$days_from_start <- day
      imputter$datetime <- as.Date(imputter$datetime)+1
      imputter$date <- as.Date(imputter$date)+1
      # Change the deaths
      imputter$deaths<-0
      # Append the data
      fips.df<-rbind(fips.df,imputter)
      county_data <- rbind(county_data,imputter)
      #county_data[which(county_data$fips==fips & !is.na(county_data$rolled_cases) & county_data$days_from_start == day),] <- fips.df[which(fips.df$days_from_start == day),]
    }
    #fips.df[which(fips.df$days_from_start == day),"new_rolled_cases"] <- fips.df[which(fips.df$days_from_start == day),"rolled_cases"] - fips.df[which(fips.df$days_from_start == day-1),"rolled_cases"]
  }
  
  if((first.fips.date+22) > last.fips.date){
    for(day in first.fips.date:last.fips.date){
      #total.deaths<- sum(fips.df[which(fips.df$days_from_start<= day),"deaths"])
      fips.df[which(fips.df$days_from_start == day),"incident_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"]
    }
    next
  }else{
    
    for(day in first.fips.date:(first.fips.date+21)){
      #total.deaths<- sum(fips.df[which(fips.df$days_from_start<= day),"deaths"])
      fips.df[which(fips.df$days_from_start == day),"incident_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"]
    }
    
    for (day in (first.fips.date+22):last.fips.date){
      #total.deaths0<- sum(fips.df[which(fips.df$days_from_start<= day),"deaths"])
      #total.deaths22<- sum(fips.df[which(fips.df$days_from_start<= day-22),"deaths"])
      fips.df[which(fips.df$days_from_start == day),"incident_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"] - fips.df[which(fips.df$days_from_start == day-22),"cases"]
      
    }
    
  }
  county_data[which(county_data$fips==fips),"cases"] <- fips.df[,"incident_cases"]
  
  print(fips)
}


# Take 7 day rolling average per county

foreach(fips = fips_list)%do%{
  county_slice = county_data[which(county_data$fips==fips), ]
  county_slice$rolled_cases =  zoo::rollmean(county_slice$cases, 7, fill=NA, align="right")
  county_data[which(county_data$fips==fips), "rolled_cases"] <- county_slice$rolled_cases
}

# construct the 14-day table

latest_date = max(county_data$days_from_start)
county_14data<-subset(county_data, days_from_start > latest_date-14 & days_from_start <= latest_date)
county_14data <- county_14data[, which(names(county_14data) %in% c("date","fips", "rolled_cases"))]

county_14data <- reshape(county_14data, 
             timevar = "date",
             idvar = c("fips"),
             direction = "wide")



write.csv(county_14data,"../data/14_Day_Table.csv",row.names=FALSE)


closeAllConnections()
