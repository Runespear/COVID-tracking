county_analysis_lm <- function(restricted_state_df,cutoff){

  restricted_state_fips_list = sort(unique(restricted_state_df$fips))
  rlist = c()
  t0list = c()
  
  for (fips in restricted_state_fips_list){
#    print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
  
    
    # NORMAL LOGMODEL
    logmodel = lm(formula = log_rolled_cases ~ days_from_start, data=county_df)
    #print(logmodel)
    lm.rguess <- NULL
    lm.t0 <- NULL
    
    
    # CALCULATE INTERCEPT AND PREDICTION FOR LOGMODEL
    try(lm.rguess <- coef(summary(logmodel))["days_from_start","Estimate"])
    if (is.null(lm.rguess)){
      lm.rguess <- NA
      lm.t0 <- NA
      next
    }
    else if (lm.rguess == 0){
      lm.t0 = min(county_df$days_from_start)
    }
    else{
      lm.t0 = coef(summary(logmodel))["(Intercept)","Estimate"]/(-lm.rguess)
    }
    restricted_state_df[which(restricted_state_df$fips == fips),"t0.lm"] = lm.t0
    restricted_state_df[which(restricted_state_df$fips == fips),"r.lm"] = lm.rguess
  }
  
  
  restricted_state_df = subset(restricted_state_df, days_from_start == cutoff)
  
  #print(restricted_state_df)
  return(restricted_state_df)
  #write.csv(restricted_idaho_df,"./data/idaho_grf.csv",row.names=FALSE)
  
}




