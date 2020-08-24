list.of.packages <- c("tidyverse","matrixStats")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)



# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# County Location ---------------------------------------------------------------


# Source: https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html

# Load County Location

county<- read.delim("./data/2019_Gaz_counties_national.txt")

county<- county %>% select(GEOID, INTPTLAT, INTPTLONG)

#county$FIPS<-county$GEOID

county$LAT<-county$INTPTLAT

county$LON<-county$INTPTLONG

#features<-select(county,-c(GEOID,INTPTLAT,INTPTLONG))

#write_csv(features, "./data/county_features.csv")

# Centroids + SVI ---------------------------------------------------------

svi <- read_csv("./data/SVI2018_US_COUNTY.csv") # source: https://svi.cdc.gov/data-and-tools-download.html

svi$GEOID<-as.numeric(svi$FIPS)

features <- left_join(county,svi, by = "GEOID" )

features<-select(features,-c(GEOID,INTPTLAT,INTPTLONG))

# Generate a new row for NYC, by taking the median of New York, Kings, Queens, Bronx and Richmond counties
# {36005: Bronx, 36047: Kings, 36061: New York, 36081: Queens, Richmond: 36085 }

NYC.counties <- c(36005,36047,36061,36081,36085)

NYC.counties.features <- features[which(features$FIPS %in% NYC.counties),-which(names(features)%in% c("STATE","ST_ABBR","COUNTY","LOCATION","FIPS"))]
NYC.counties.features <- mutate_all(NYC.counties.features, function(x) as.numeric(as.character(x)))

NYC.features <- colMedians(as.matrix(NYC.counties.features))
NYC.features <- rbind(NYC.counties.features,NYC.features)
NYC.features <- NYC.features[c(dim(NYC.features)[1]),]
#NYC.features <- apply(NYC.counties.features,2,median)
#NYC.features <- as.data.frame(NYC.features)
#break
#"STATE","ST_ABBR","COUNTY","LOCATION", "FIPS"

NYC.features$STATE <- "NEW YORK"
NYC.features$ST_ABBR <- "NY"
NYC.features$FIPS <- 99999
NYC.features$COUNTY <- "New York City"
NYC.features$LOCATION <- "New York City, New York"

features <- rbind(features, NYC.features)

write_csv(features, "./data/county_features.csv")
