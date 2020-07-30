require("data.table")

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
  
  restricted_state_df = subset(state_df, days_from_start <= cutoffend & days_from_start > cutoffstart)
  # print(restricted_state_df)
  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  rlist = c()
  t0list = c()
  
  for (fips in restricted_state_fips_list){
    #print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    
    # 2 types of linear models
    # ln(I_t) = rt - rt_0
    # ln(I_t) - ln(I_tw) = r(t-t_w)
    county_df$logdiff <- county_df$log_rolled_cases - min(county_df$log_rolled_cases)
    county_df$shifted_time <- county_df$days_from_start - cutoffstart
    
    # SHIFTED LOGMODEL
    shifted.logmodel = lm(formula = logdiff ~ shifted_time, data=county_df)
    print(shifted.logmodel)
    shifted.rguess <- NULL
    shifted.t0 <- NULL
    shifted.predict_guess <- NULL
    
    # NORMAL LOGMODEL
    logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
    print(logmodel)
    lm.rguess <- NULL
    lm.t0 <- NULL
    lm.predict_guess <- NULL
    
    
    # CALCULATE INTERCEPT AND PREDICTION FOR SHIFTED LOGMODEL
    try(shifted.rguess <- coef(summary(shifted.logmodel))["shifted_time","Estimate"])
    print(shifted.rguess)
    if (is.null(shifted.rguess)){
      shifted.rguess <- NA
      shifted.t0 <- NA
      shifted.predict_guess<-NA
      next
    }
    else if (shifted.rguess == 0){
      shifted.t0 = min(county_df$days_from_start)
      shifted.predict_guess=min(county_df$log_rolled_cases)
    }
    else{
      shifted.t0 =( mean(county_df$log_rolled_cases) - shifted.rguess*mean(county_df$days_from_start))/(-shifted.rguess)
      shifted.predict_guess = log_exp(cutoffend+predictionsize,shifted.rguess,shifted.t0)
    }
    restricted_state_df[which(restricted_state_df$fips == fips),"t0.slm"] = shifted.t0
    restricted_state_df[which(restricted_state_df$fips == fips),"r.slm"] = shifted.rguess
    restricted_state_df[which(restricted_state_df$fips == fips),"predicted.slm"] = shifted.predict_guess
  
    
    # CALCULATE INTERCEPT AND PREDICTION FOR LOGMODEL
    try(lm.rguess <- coef(summary(logmodel))["days_from_start","Estimate"])
    if (is.null(lm.rguess)){
      lm.rguess <- NA
      lm.t0 <- NA
      lm.predict_guess<-NA
      next
    }
    else if (lm.rguess == 0){
      lm.t0 = min(county_df$days_from_start)
      lm.predict_guess=min(county_df$log_rolled_cases)
    }
    else{
      lm.t0 = coef(summary(logmodel))["(Intercept)","Estimate"]/(-lm.rguess)
      lm.predict_guess = log_exp(cutoffend+predictionsize,lm.rguess,lm.t0)
    }
    restricted_state_df[which(restricted_state_df$fips == fips),"t0.lm"] = lm.t0
    restricted_state_df[which(restricted_state_df$fips == fips),"r.lm"] = lm.rguess
    restricted_state_df[which(restricted_state_df$fips == fips),"predicted.lm"] = lm.predict_guess
  }
  
  
  
  # print(restricted_state_df)
  # Method 1: Calculate ln(I) = r*t - intercept and feed into GRF
  # Method 2: Cluster time series by DTW -> then refit model with exponential model
  restricted_state_df = na.omit(restricted_state_df)
  tau.forest <- NULL
  
  # num_trees_list = c(2000)
  num_trees =2000
  tau.forest <-grf::causal_forest(X=restricted_state_df[,c("r.slm","t0.slm")], Y=restricted_state_df[,"log_rolled_cases"], W= restricted_state_df[,"days_from_start"], num.trees = num_trees)
  # Re-estimated r
  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  r.grflist = c()
  for (fips in restricted_state_fips_list){
    # print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    X.test <- unique(restricted_state_df[which(restricted_state_df$fips == fips), c("r.slm","r.slm")])
    
    tau.hat <- predict(tau.forest,X.test, estimate.variance = TRUE)
    sigma.hat <- sqrt(tau.hat$variance.estimates)
    #print(tau.hat)
    
    r.grf.string = paste("r.grf","",sep="")
    # r.SE.grf.string = paste("r.SE.grf","",sep="")
    grf.predict.string = paste("predicted.grf","",sep="")
    t0.grf.string = paste("t0.grf","",sep="")
    
    restricted_state_df[which(restricted_state_df$fips == fips), r.grf.string] <- tau.hat
    # restricted_state_df[which(restricted_state_df$fips == fips), r.SE.grf.string] <- sigma.hat
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