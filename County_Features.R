list.of.packages <- c("tidyverse")

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

county$FIPS<-county$GEOID

county$LAT<-county$INTPTLAT

county$LON<-county$INTPTLONG

features<-select(county,-c(GEOID,INTPTLAT,INTPTLONG))

write_csv(features, "./data/county_features.csv")

# Centroids + SVI ---------------------------------------------------------

svi <- read_csv("./data/SVI2018_US_COUNTY.csv") # source: https://svi.cdc.gov/data-and-tools-download.html

svi$GEOID<-as.numeric(svi$FIPS)

#features <- left_join(county,svi, by = "GEOID" )

#features<-select(features,-c(GEOID,INTPTLAT,INTPTLONG))

#write_csv(features, "./data/county_features.csv")