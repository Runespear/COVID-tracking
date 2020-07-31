require("data.table")

county_analysis <- function(state, county_data, cutoffstart,cutoffend, predictionsize){
  #print(state)
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
    print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    
    # 2 types of linear models
    # ln(I_t) = rt - rt_0
    # ln(I_t) - ln(I_tw) = r(t-t_w)
    window.start.log_rolled_cases <- min(county_df$log_rolled_cases)
    restricted_state_df[which(restricted_state_df$fips == fips),"window.start.log_rolled_cases"] <- window.start.log_rolled_cases
    
    county_df$logdiff <- county_df$log_rolled_cases - window.start.log_rolled_cases
    county_df$shifted_time <- county_df$days_from_start - cutoffstart
    
    
    # SHIFTED LOGMODEL
    shifted.logmodel = lm(formula = logdiff ~ shifted_time, data=county_df)
    #print(shifted.logmodel)
    shifted.rguess <- NULL
    shifted.t0 <- NULL
    shifted.predict_guess <- NULL
    
    # NORMAL LOGMODEL
    logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
    #print(logmodel)
    lm.rguess <- NULL
    lm.t0 <- NULL
    lm.predict_guess <- NULL
    
    
    # CALCULATE INTERCEPT AND PREDICTION FOR SHIFTED LOGMODEL
    try(shifted.rguess <- coef(summary(shifted.logmodel))["shifted_time","Estimate"])
    #print(shifted.rguess)
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
  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  
  # print(restricted_state_df)
  
  num_trees =2000
  # Default GRF with only r.slm, t0.slm, window.start.log_rolled_cases as features
  tau.forest <- NULL
  tau.forest <-grf::causal_forest(X=restricted_state_df[,c("r.slm","t0.slm","window.start.log_rolled_cases")], Y=restricted_state_df[,"log_rolled_cases"], W= restricted_state_df[,"days_from_start"], num.trees = num_trees)

  augmented.tau.forest <- NULL
  feature.exclusion <- c("fips","r.lm","t0.lm","predicted.lm","predicted.slm","date","county","state","cases","datetime","deaths","days_from_start","logcases","rolled_cases","log_rolled_cases", "r.grf","t0.grf","predicted.grf","r.grf.augmented","t0.grf.augmented","predicted.grf.augmented")
  augmented.tau.forest <-grf::causal_forest(X=restricted_state_df[,-which(names(restricted_state_df) %in% feature.exclusion )], Y=restricted_state_df[,"log_rolled_cases"], W= restricted_state_df[,"days_from_start"], num.trees = num_trees)
  
  
  
  #r.grflist = c()
  for (fips in restricted_state_fips_list){
    print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    X.test <- unique(county_df[, c("r.slm","t0.slm","window.start.log_rolled_cases")])
    augmented.X.test <- unique(county_df[, -which(names(restricted_state_df) %in% feature.exclusion)])
    
    #print(X.test)
    #print(augmented.X.test)
    
    tau.hat <- predict(tau.forest,X.test, estimate.variance = FALSE)
    r.grf <- unlist(tau.hat)[1]
    #sigma.hat <- sqrt(tau.hat$variance.estimates)
    #print(tau.hat[[1]])
    augmented.tau.hat <- predict(augmented.tau.forest, augmented.X.test, estimate.variance = FALSE)
    augmented.r.grf <- unlist(augmented.tau.hat)[1]
    print(augmented.r.grf)
    print(augmented.X.test)
    
    r.grf.string = paste("r.grf","",sep="")
    # r.SE.grf.string = paste("r.SE.grf","",sep="")
    grf.predict.string = paste("predicted.grf","",sep="")
    t0.grf.string = paste("t0.grf","",sep="")
    
    restricted_state_df[which(restricted_state_df$fips == fips), r.grf.string] <- r.grf
    
    restricted_state_df[which(restricted_state_df$fips == fips), "r.grf.augmented"] <- augmented.r.grf
    
    # restricted_state_df[which(restricted_state_df$fips == fips), r.SE.grf.string] <- sigma.hat
    #r.grflist = c(r.grflist,tau.hat)
    
    county_df$y.hat <- county_df$days_from_start * r.grf
    county_df$augmented.y.hat <- county_df$days_from_start * augmented.r.grf
    # Re-estimate t0
    t0.hat <- (mean(county_df$log_rolled_cases) - mean(county_df$y.hat))/(-r.grf)
    augmented.t0.hat <- (mean(county_df$log_rolled_cases) - mean(county_df$augmented.y.hat))/(-augmented.r.grf)
    
    # Put in the predicted cases
    restricted_state_df[which(restricted_state_df$fips == fips), grf.predict.string] <-log_exp(cutoffend+ predictionsize,r.grf,t0.hat)
    restricted_state_df[which(restricted_state_df$fips == fips), "predicted.grf.augmented"] <-log_exp(cutoffend+ predictionsize,augmented.r.grf,augmented.t0.hat)
    
    # Put in thee predicted intercepts
    restricted_state_df[which(restricted_state_df$fips == fips), t0.grf.string] <- t0.hat
    restricted_state_df[which(restricted_state_df$fips == fips), "t0.grf.augmented"] <- augmented.t0.hat
    
    
    
  }
  restricted_state_df = subset(restricted_state_df, days_from_start == cutoffend)
  
  #print(restricted_state_df)
  return(restricted_state_df)
  #write.csv(restricted_idaho_df,"./data/idaho_grf.csv",row.names=FALSE)
  
}