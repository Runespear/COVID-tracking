list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "choroplethr", "choroplethrMaps", "foreach", "evaluate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(new.packages)

library(rlist)

# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# registerDoParallel(cores=6)
county_analysis <- function(state, county_data, cutoffstart,cutoffend, predictionsize){
  
  state_df = county_data[which(county_data$state==state),]
  
  state_fips_list = sort(unique(state_df$fips))
  
  # Define log linear and double parameter exponent models
  log_exp <-function(t,r,t0){r*t-r*t0}
  double_exp <- function(t,r,t0){exp(r*t-r*t0)}
  
  
  earliest_start = min(state_df$days_from_start)
  latest_start = earliest_start
  for (fips in state_fips_list){
    county_df = state_df[which(state_df$fips == fips),]
    county_start = min(county_df$days_from_start)
    #print(county_start)
    if (county_start > latest_start ){
      latest_start = county_start
    }
  }
  
  restricted_state_df = subset(state_df, days_from_start <= cutoffend & days_from_start >= cutoffstart)
  # print(restricted_state_df)
  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  rlist = c()
  t0list = c()
  
  for (fips in restricted_state_fips_list){
    #print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    
    logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
    #print(fips)
    #print(coef(summary(logmodel)))
    rguess <- NULL
    try(rguess <- coef(summary(logmodel))["days_from_start","Estimate"])
    if (is.null(rguess)){
      rguess <- NA
      t0guess <- NA
#      SE_rguess <-NA
      predict_guess<-NA
      next
    }
    if (rguess == 0){
      t0guess = min(county_df$days_from_start)
      sigma_rguess = 0
      predict_guess=0
    }
    else{
      t0guess = coef(summary(logmodel))["(Intercept)","Estimate"]/(-rguess)
      #guess = c(r = rguess, t0 = t0guess)
#      SE_rguess = coef(summary(logmodel))["days_from_start", "Std. Error"]
      predict_guess = log_exp(cutoffend-1,rguess,t0guess)
    }
    #print(coef(logmodel))
    #print(confint(logmodel))
    #return(logmodel)
    restricted_state_df[which(restricted_state_df$fips == fips),"t0"] = t0guess
    restricted_state_df[which(restricted_state_df$fips == fips),"r"] = rguess
 #   restricted_state_df[which(restricted_state_df$fips == fips),"r.SE"] = SE_rguess
    restricted_state_df[which(restricted_state_df$fips == fips),"lm_predict"] = predict_guess
  }
  # print(restricted_state_df)
  # Method 1: Calculate ln(I) = r*t - intercept and feed into GRF
  # Method 2: Cluster time series by DTW -> then refit model with exponential model
  restricted_state_df = na.omit(restricted_state_df)
  tau.forest <- NULL
  
  # num_trees_list = c(2000)
  num_trees =2000
  tau.forest <-grf::causal_forest(X=restricted_state_df[,c("r","t0")], Y=restricted_state_df[,"log_rolled_cases"], W= restricted_state_df[,"days_from_start"], num.trees = num_trees)
  # Re-estimated r
  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  r.grflist = c()
  for (fips in restricted_state_fips_list){
    # print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    X.test <- unique(restricted_state_df[which(restricted_state_df$fips == fips), c("r","t0")])
    
    tau.hat <- predict(tau.forest,X.test, estimate.variance = TRUE)
    sigma.hat <- sqrt(tau.hat$variance.estimates)
    #print(tau.hat)
    
    r.grf.string = paste("r.grf","",sep="")
    r.SE.grf.string = paste("r.SE.grf","",sep="")
    grf.predict.string = paste("grf_predict","",sep="")
    t0.grf.string = paste("t0.grf","",sep="")
    
    restricted_state_df[which(restricted_state_df$fips == fips), r.grf.string] <- tau.hat
    restricted_state_df[which(restricted_state_df$fips == fips), r.SE.grf.string] <- sigma.hat
    r.grflist = c(r.grflist,tau.hat)
    
    county_df$X <- county_df$days_from_start * tau.hat[[1]]
    # Re-estimate t0
    t0.hat <- (mean(county_df$log_rolled_cases) - mean(county_df$X))/(-tau.hat)
    
    restricted_state_df[which(restricted_state_df$fips == fips), grf.predict.string] <-log_exp(cutoffend+ predictionsize,tau.hat,t0.hat)
    
    restricted_state_df[which(restricted_state_df$fips == fips), t0.grf.string] <- t0.hat
    #print(restricted_state_df)
}
  restricted_state_df = subset(restricted_state_df, days_from_start == cutoffend)
  #print(restricted_state_df)
  return(restricted_state_df)
  #write.csv(restricted_idaho_df,"./data/idaho_grf.csv",row.names=FALSE)
  
}

# Load Data

destfile <- paste("./data/processed_us-counties_",Sys.Date(),".csv",sep="")

county_data <- read.csv(file = destfile)
county_data$datetime <- anytime::anydate(county_data$date)
county_data$log_rolled_cases <- log(county_data$rolled_cases)

state_list = sort(unique(county_data$state))
# switch to state_list for all states, Idaho, California, Massachusetts, Texas
windowsize = 7
predictionsize = 7
#for (cutoff in (earliest_start+windowsize):(latest_date -predictionsize)){
earliest_start = min(county_data$days_from_start)
latest_date = max(county_data$days_from_start)

for (cutoff in (latest_date - predictionsize):(latest_date - predictionsize)){
  state_df_list <- list()
  lm_mseS<-list()
  grf_mseS<-list()
  cutoff_S <-list()
  state_S <-list()
  total_df <- NULL
  for (state in state_list){
    #state_df = county_data[which(county_data$state==state),]
    
    restricted_state_df1 <- NULL
    restricted_state_df2 <- NULL
    restricted_state_df0 <- NULL
    
    try(restricted_state_df1 <- county_analysis(state, county_data, earliest_start,cutoff + predictionsize,predictionsize))
    if(is.null(restricted_state_df1)){
      next
    }
    try(restricted_state_df0 <- county_analysis(state, county_data, cutoff-windowsize, cutoff,predictionsize))
    if(is.null(restricted_state_df0)){
      next
    }
    
    
    today<-restricted_state_df0[c("date","county","state","fips","r","t0","lm_predict","r.grf","t0.grf","grf_predict")]
    tomorrow<-restricted_state_df1[c("date","fips","log_rolled_cases")]
    restricted_state_df2<-merge(x=today,y=tomorrow,by="fips",x.all=TRUE)
    restricted_state_df2$lm_mse<-with(restricted_state_df2,(lm_predict-log_rolled_cases)**2)
    restricted_state_df2$grf_mse<-with(restricted_state_df2,(grf_predict-log_rolled_cases)**2)
    
    lm_mseS<-list.append(lm_mseS,sum(restricted_state_df2$lm_mse))
    grf_mseS<-list.append(grf_mseS,sum(restricted_state_df2$grf_mse))
    cutoff_S<-list.append(cutoff_S,cutoff)
    state_S<-list.append(state_S,state)
    
    total_df <- rbind(total_df,restricted_state_df2)
    
    mainDir = "./data/output/"
    subDir = state
    file_sub = paste(mainDir,subDir,sep="")
    dir.create(file.path(mainDir, subDir))
    
    # write.csv(restricted_state_df2,paste(file_sub,"/",state,"_",toString(cutoff),"_grf.csv",sep=""),row.names=FALSE)
    
    print(paste("Done writing csv for day ", toString(cutoff), " of " ,state,sep=""))
    # break
  }
  print(cbind(state_S,lm_mseS, grf_mseS))
  write.csv(total_df,paste(mainDir,"allstates_",toString(cutoff),"_grf.csv",sep=""),row.names=FALSE)
}



closeAllConnections()

