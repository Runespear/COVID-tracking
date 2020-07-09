list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
list.of.packages <- c(list.of.packages, "zoo")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=6)

# Blaine County, Idaho Ski Bar
# fips = 16013
# Cases = 126
# Date ~= 2020-06-03

# Pre-processing the data

county_data <- read.csv(file = './data/us-counties_2020-06-16.csv')

county_data$datetime <- anytime::anydate(county_data$date)

# Find the earliest date and latest dates

start_date = min(county_data$datetime)
end_date = max(county_data$datetime)

# Add Columns of days from start

county_data$days_from_start <- as.numeric(county_data$datetime- start_date , units="days")

# Add logcases

county_data$logcases <- log(county_data$cases)


# Obtain list of fips

fips_list = sort(unique(county_data$fips))


# Take 7 day rolling average per county

for (fips in fips_list){
  county_slice = county_data[which(county_data$fips==fips), ]
  county_slice$rolled_cases =  zoo::rollmean(county_slice$cases, 7, fill=NA, align="right")
  county_data[which(county_data$fips==fips), "rolled_cases"] <- county_slice$rolled_cases
}

# Slice away first 6 days

county_data = county_data[complete.cases(county_data),]

start_date = min(county_data$datetime)
end_date = max(county_data$datetime)

write.csv(county_data, "./data/processed_us-counties.csv", row.names=FALSE)

closeAllConnections()
