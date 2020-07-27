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
  
  # Check these 2 lines
  #tt <- table(restricted_state_df$fips)
  #restricted_state_df <- subset(restricted_state_df,  fips %in% names(tt[tt>=7]) )
  #restricted_state_df <- restricted_state_df[restricted_state_df$fips %in% names(tt[tt >= 7]), ]
  #print(restricted_state_df)
  
  for (fips in restricted_state_fips_list){
    #print(fips)
    county_df = restricted_state_df[which(restricted_state_df$fips == fips),]
    
    county_df$logdiff <- county_df$log_rolled_cases - min(county_df$log_rolled_cases)
    county_df$shifted_time <- county_df$days_from_start - cutoffstart
    
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
      predict_guess = log_exp(cutoffend+predictionsize,rguess,t0guess)
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