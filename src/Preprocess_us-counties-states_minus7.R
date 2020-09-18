require("pacman")
p_load("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
p_load("zoo","usmap","readxl","lubridate","here")


#setwd(file.path(here(),"src"))
setwd("~/COVID-tracking/src")

destfile <- paste("../data/us-counties_latest",".csv",sep="")


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

# Add logcases

#county_data$logcases <- log(county_data$cases)


# Obtain list of fips

fips_list = sort(unique(county_data$fips))


# Take 7 day rolling average per county

#foreach(fips = fips_list)%do%{
#  county_slice = county_data[which(county_data$fips==fips), ]
#  county_slice$rolled_cases =  zoo::rollmean(county_slice$cases, 7, fill=NA, align="right")
#  county_data[which(county_data$fips==fips), "rolled_cases"] <- county_slice$rolled_cases
#}

# Obtain the daily new cases
present.fips.list <- sort(unique(county_data$fips))

county_data$weekly_cases <- NA
# First loop through counties
# present.fips.list
county_data_backup <- county_data
for (fips in present.fips.list){
  
  fips.df <- county_data[which(county_data$fips==fips & !is.na(county_data$cases)),]
  if (dim(fips.df)[1] == 0){
    print(paste("fips ",toString(fips)," has no entry ",sep=""))
    next
  }
  first.fips.date <- min(fips.df$days_from_start)
  last.fips.date <- max(fips.df$days_from_start)
  fips.df[which(fips.df$days_from_start == first.fips.date),"weekly_cases"] <- fips.df[which(fips.df$days_from_start == first.fips.date),"cases"]
  print(fips)
  if(first.fips.date == last.fips.date){
    print(paste("fips ",toString(fips)," only has one entry ",sep=""))
    next
  }
  for (day in (first.fips.date+1):last.fips.date){
    #print(day)
    county.day.slice <- fips.df[which(fips.df$days_from_start == day),]
    if (dim(county.day.slice)[1] == 0){
      # Impute Missing days inbetween e.g. fips 31057 day 184 jumps to 189
      print(paste("imputing for day ",toString(day)," of fips ",toString(fips),sep=""))
      imputter <- fips.df[which(fips.df$days_from_start == day-1),]
      # Change the date
      imputter$days_from_start <- day
      
      imputter$datetime <- as.Date(imputter$datetime)+1
      imputter$date <- as.Date(imputter$date)+1
      # Append the data
      fips.df<-rbind(fips.df,imputter)
      county_data <- rbind(county_data,imputter)
      #county_data[which(county_data$fips==fips & !is.na(county_data$rolled_cases) & county_data$days_from_start == day),] <- fips.df[which(fips.df$days_from_start == day),]
    }
  }
  # Below is to calculate new weekly cases
  if (first.fips.date+7 > last.fips.date){
    next
  }
  for (day in (first.fips.date+7):last.fips.date){
    
    fips.df[which(fips.df$days_from_start == day),"weekly_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"] - fips.df[which(fips.df$days_from_start == day-7),"cases"]
  }
  county_data[which(county_data$fips==fips& !is.na(county_data$cases)),"weekly_cases"] <- fips.df[,"weekly_cases"]
}
#break
# Write intermediate result as processed_us-counties_latest.csv

write.csv(county_data,"../data/processed_us-counties_latest_minus7.csv",row.names=FALSE)


closeAllConnections()
