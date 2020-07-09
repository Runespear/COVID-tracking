list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "choroplethr", "choroplethrMaps")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=6)


# Load Data

county_data <- read.csv(file = './data/processed_us-counties.csv')
county_data$datetime <- anytime::anydate(county_data$date)
county_data$log_rolled_cases <- log(county_data$rolled_cases)

idaho_df = county_data[which(county_data$state=="Idaho"),]

idaho_fips_list = sort(unique(idaho_df$fips))

# Blaine County, Idaho Ski Bar
# fips = 16013
# Cases = 126
# Date ~= 2020-03-06

blaine_df <- county_data[which(county_data$fips==16013),]

log_exp <-function(t,r,t0){r*t-r*t0}
double_exp <- function(t,r,t0){exp(r*t-r*t0)}




earliest_start = min(idaho_df$days_from_start)
latest_start = earliest_start
for (fips in idaho_fips_list){
  county_df = idaho_df[which(idaho_df$fips == fips),]
  county_start = min(county_df$days_from_start)
  #print(county_start)
  if (county_start > latest_start ){
    latest_start = county_start
  }
}

# Restrict analyses < 80 days from start
cutoff = 73
restricted_idaho_df = idaho_df[which(idaho_df$days_from_start < cutoff),]
restricted_idaho_fips_list = sort(unique(restricted_idaho_df$fips))
rlist = c()
t0list = c()

for (fips in restricted_idaho_fips_list){
  #print(fips)
  county_df = restricted_idaho_df[which(restricted_idaho_df$fips == fips),]
  
  logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
  rguess = coef(logmodel)[[2]]
  t0guess = coef(logmodel)[[1]]/(-rguess)
  #guess = c(r = rguess, t0 = t0guess)
  
  while(TRUE){
    expmodel<-NULL
    # Note if nls fails, then carry on with r and t0 set to NA
    try(expmodel<-nls(formula = rolled_cases ~ double_exp(days_from_start, r, t0), data=county_df, start = c(r = rguess, t0 = t0guess) )); # does not stop in the case of error
    
    if(!is.null(expmodel)){
      r = coef(expmodel)[[1]]
      t0 = coef(expmodel)[[2]]
      
      rlist = c(rlist, r)
      t0list = c(t0list, t0)
      
      break; # if nls works, then quit from the loop
    }
    r = NA
    t0 = NA
    rlist = c(rlist,NA)
    t0list = c(t0list,NA)
    break;
  }
  restricted_idaho_df[which(restricted_idaho_df$fips == fips),"t0"] = t0
  restricted_idaho_df[which(restricted_idaho_df$fips == fips),"r"] = r
}

# Method 1: Calculate ln(I) = r*t - intercept and feed into GRF
# Method 2: Cluster time series by DTW -> then refit model with exponential model
restricted_idaho_df = na.omit(restricted_idaho_df)

tau.forest <-causal_forest(X=restricted_idaho_df[,c("r","t0")], Y=restricted_idaho_df[,"log_rolled_cases"], W= restricted_idaho_df[,"days_from_start"])

# Re-estimated r
restricted_idaho_fips_list = sort(unique(restricted_idaho_df$fips))
r.grflist = c()
for (fips in restricted_idaho_fips_list){
  # print(fips)
  county_df = restricted_idaho_df[which(restricted_idaho_df$fips == fips),]
  X.test <- unique(restricted_idaho_df[which(restricted_idaho_df$fips == fips), c("r","t0")])

  tau.hat <- predict(tau.forest,X.test)
  
  restricted_idaho_df[which(restricted_idaho_df$fips == fips), "r.grf"] <- tau.hat
  r.grflist = c(r.grflist,tau.hat)
  
  county_df$X <- county_df$days_from_start * tau.hat[[1]]
  # Re-estimate t0
  t0.hat <- (mean(county_df$log_rolled_cases) - mean(county_df$X))/(-tau.hat)
  
  restricted_idaho_df[which(restricted_idaho_df$fips == fips), "t0.grf"] <- t0.hat
}

write.csv(restricted_idaho_df,"./data/idaho_grf.csv",row.names=FALSE)


# Sacramento County, California
# fips = 06067
# Start Date ~= 2020-03-15
# start day of sacramento is 37
# 44 days from start day of sacramento

california_df = county_data[which(county_data$state=="California"),]

# 10
earliest_california_start = min(california_df$days_from_start) 
earliest_california_start_datetime = min(california_df$datetime)

sacramento_df = california_df[which(california_df$fips == 6067),]

cutoff = 65
restricted_california_df = california_df[which(california_df$days_from_start < cutoff),]
restricted_california_fips_list = sort(unique(restricted_california_df$fips))
rlist = c()
t0list = c()
explist = c()

for (fips in restricted_california_fips_list){
  print(fips)
  county_df = restricted_california_df[which(restricted_california_df$fips == fips),]
  
  logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
  rguess = coef(logmodel)[[2]]
  t0guess = coef(logmodel)[[1]]/(-rguess)
  #guess = c(r = rguess, t0 = t0guess)
  
  while(TRUE){
    expmodel<-NULL
    # Note if nls fails, then carry on with r and t0 set to NA
    try(expmodel<-nls(formula = rolled_cases ~ double_exp(days_from_start, r, t0), data=county_df, start = c(r = rguess, t0 = t0guess) )); # does not stop in the case of error
    
    if(!is.null(expmodel)){
      r = coef(expmodel)[[1]]
      t0 = coef(expmodel)[[2]]
      
      rlist = c(rlist, r)
      t0list = c(t0list, t0)
      explist = c(explist, expmodel)
      
      break; # if nls works, then quit from the loop
    }
    r = NA
    t0 = NA
    explist = NA
    rlist = c(rlist,NA)
    t0list = c(t0list,NA)
    explist = c(explist, expmodel)
    break;
  }
  restricted_california_df[which(restricted_california_df$fips == fips),"t0"] = t0
  restricted_california_df[which(restricted_california_df$fips == fips),"r"] = r
}
restricted_california_df = na.omit(restricted_california_df)

tau.forest <-causal_forest(X=restricted_california_df[,c("r","t0")], Y=restricted_california_df[,"log_rolled_cases"], W= restricted_california_df[,"days_from_start"])
restricted_california_fips_list = sort(unique(restricted_california_df$fips))
r.grflist = c()
for (fips in restricted_california_fips_list){
  # print(fips)
  county_df = restricted_california_df[which(restricted_california_df$fips == fips),]
  X.test <- unique(restricted_california_df[which(restricted_california_df$fips == fips), c("r","t0")])
  
  tau.hat <- predict(tau.forest,X.test)
  
  restricted_california_df[which(restricted_california_df$fips == fips), "r.grf"] <- tau.hat
  r.grflist = c(r.grflist,tau.hat)
  
  county_df$X <- county_df$days_from_start * tau.hat[[1]]
  # Re-estimate t0
  county_df$X <- county_df$days_from_start * tau.hat[[1]]
  # Re-estimate t0
  t0.hat <- (mean(county_df$log_rolled_cases) - mean(county_df$X))/(-tau.hat)
  
  restricted_california_df[which(restricted_california_df$fips == fips), "t0.grf"] <- t0.hat
}
write.csv(restricted_california_df,"./data/california_grf.csv",row.names=FALSE)


closeAllConnections()

