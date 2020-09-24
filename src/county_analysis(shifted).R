library("data.table")

county_analysis <- function(restricted_state_df0, cutoffstart,cutoffend, predictionsize){
  
  # Define log linear and double parameter exponent models
  log_exp <-function(t,r,t0){r*t-r*t0}
  double_exp <- function(t,r,t0){exp(r*t-r*t0)}
  
  
  #  earliest_start = min(state_df$days_from_start)
  #  latest_start = earliest_start
  #  for (fips in state_fips_list){
  #    county_df = state_df[which(state_df$fips == fips),]
  #    county_start = min(county_df$days_from_start)
  #print(county_start)
  #    if (county_start > latest_start ){
  #      latest_start = county_start
  #    }
  #  }
  
  #  restricted_state_df0 <- subset(state_df, days_from_start <= cutoffend & days_from_start >= cutoffstart)
     restricted_state_df_end <- subset(restricted_state_df0, days_from_start == cutoffend)
     names(restricted_state_df_end)[names(restricted_state_df_end)=="log_rolled_cases"] <- "log_rolled_cases_last"
     restricted_state_df_end <- restricted_state_df_end[c("fips","log_rolled_cases_last")]
     restricted_state_df<-merge(x=restricted_state_df0,y=restricted_state_df_end, by="fips", all.x=TRUE)
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
    window.start.log_rolled_cases <- min(county_df$log_rolled_cases)
    restricted_state_df[which(restricted_state_df$fips == fips),"window.start.log_rolled_cases"] <- window.start.log_rolled_cases
    latest_cases<-min(county_df$log_rolled_cases_last)
    
    county_df$logdiff <- county_df$log_rolled_cases - window.start.log_rolled_cases
    county_df$shifted_time <- county_df$days_from_start - cutoffstart
    
    
    # SHIFTED LOGMODEL FIXED INTERCEPT REGRESSION
    shifted.logmodel = lm(formula = logdiff ~ shifted_time + 0, data=county_df)
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
      shifted.predict_guess = predictionsize*shifted.rguess + latest_cases
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
      lm.predict_guess = predictionsize*lm.rguess + latest_cases
    }
    restricted_state_df[which(restricted_state_df$fips == fips),"t0.lm"] = lm.t0
    restricted_state_df[which(restricted_state_df$fips == fips),"r.lm"] = lm.rguess
    restricted_state_df[which(restricted_state_df$fips == fips),"predicted.lm"] = lm.predict_guess
  }
  
  
  restricted_state_df = subset(restricted_state_df, days_from_start == cutoffend)
  
  #print(restricted_state_df)
  return(restricted_state_df)
  #write.csv(restricted_idaho_df,"./data/idaho_grf.csv",row.names=FALSE)
  
}