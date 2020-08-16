list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
list.of.packages <- c(list.of.packages, "zoo","usmap","readxl","lubridate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#registerDoParallel(cores=6)

# Blaine County, Idaho Ski Bar
# fips = 16013
# Cases = 126
# Date ~= 2020-06-03

# URL of NYTimes Data
nyt_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

destfile <- paste("./data/us-counties_latest",".csv",sep="")
county_data <- read.csv(nyt_url)
write.csv(county_data, destfile, row.names=FALSE)
# Pre-processing the data

county_data <- read.csv(file = destfile)
county_data$datetime <- anytime::anydate(county_data$date)

# CONVERY NYC fips from NA -> 99999

county_data[which(county_data$county=="New York City"),"fips"] <- 99999

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

foreach(fips = fips_list)%do%{
  county_slice = county_data[which(county_data$fips==fips), ]
  county_slice$rolled_cases =  zoo::rollmean(county_slice$cases, 7, fill=NA, align="right")
  county_data[which(county_data$fips==fips), "rolled_cases"] <- county_slice$rolled_cases
}

# Write intermediate result as processed_us-counties_latest.csv

write.csv(county_data,"./data/processed_us-counties_latest.csv",row.names=FALSE)

# Slice away first 6 days

county_data = county_data[complete.cases(county_data),]

start_date = min(county_data$datetime)
end_date = max(county_data$datetime)

# Process county features

county_features <- read.csv(file=file.path("./data/county_features.csv"))

# Drop all "E_..." prefix

county_features <- county_features[,which(!grepl("M_",names(county_features)))]

# Convert -999 to NA

county_features[county_features==-999] <-NA

# DROP STm, STATE, ST_ABBR, COUNTY, LOCATION since we already have fips

county_features <- county_features[, -which(names(county_features) %in% c("ST","STATE","ST_ABBR","COUNTY","LOCATION"))]

names(county_features)[names(county_features)=="FIPS"] <- "fips"

county_data_augmented <- merge(x=county_data, y=county_features, by="fips", all.x = TRUE)

end_file = paste("./data/augmented_us-counties_latest",".csv",sep="")

write.csv(county_data_augmented, end_file, row.names=FALSE)


# Load CUSP Data

CUSP = paste("./data/COVID-19 US state policy database (CUSP)",".xlsx",sep="")

# Pre-processing CUSP data

df <- read_excel(CUSP, sheet=2, n_max=51)

foreach(i = 3:13)%do%{ df <- merge(df, read_excel(CUSP, i, n_max=51)) }

foreach(i = 15:18)%do%{ df <- merge(df, read_excel(CUSP, i, n_max=51)) }

names(df) <- gsub(" ","_",names(df))

for(i in 4:length(names(df))){
  
  if(!(all(df[,i]  %in% c(0,1), na.rm = TRUE))){
    
    if(all(format(as.Date(df[,i], origin="1899-12-30"),"%Y")  %in% c(1899,2019,2020), na.rm = TRUE)){
      df[,i]<-as.Date(df[,i], origin="1899-12-30")
      for (j in 1:51) {if (year(df[j,i])==1899) {year(df[j,i])<-2030}}
    } else {next}
  } 
}

# DROP State, State_Abbreviation 

df <- df[, -which(names(df) %in% c("State", "State_Abbreviation"))]


#merge CUSP Data with county_data_augmented Data

county_data_augmented["State_FIPS_Code"]<- as.numeric(fips(county_data_augmented$state, county = c()))

data<-merge(x=county_data_augmented, y=df, by = "State_FIPS_Code", all.x = TRUE)

data$datetime<-as.Date(data$datetime, "%Y-%m-%d")


for (i in length(names(county_data_augmented)):length(names(data))) {
  
  if (inherits(data[,i], 'Date')){
    data[,i]<-data$datetime-data[,i]+1
    data[,i][data[,i]<0]<-0
  }
}



end_file = paste("./data/augmented_us-counties-states_latest",".csv",sep="")
#end_file = paste("./data/processed_us-counties_latest",".csv",sep="")

write.csv(data, end_file, row.names=FALSE)

closeAllConnections()
