closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

registerDoParallel(cores=detectCores())

#########################################################
# POST PROCESSING FOR STATE FOREST BLOCKS
#########################################################

results.file.name = "../data/output/file_to_plot/confusion_block_latest.csv"
county.data <- read.csv(file = results.file.name)

parameters.file.name = "../data/epidemic_parameters.csv"
parameters.data <- read.csv(parameters.file.name)

names(parameters.data)[names(parameters.data)=="X"] <- "state"

new.output.data <- merge(x=county.data,y=parameters.data,by="state",all=TRUE)

# SEIR Assumption
# R=(1+r/b1)*(1+r/b2) # units supposed to be in rates 1/day
# b1, b2 provided are duration in days 

new.output.data$R0 <- NA
# Indices where r > min{-b1,-b2}
mask.Ozden <- which(new.output.data$tau.hat > pmin(-1/new.output.data$b1,-1/new.output.data$b1))
#break
#https://www.medrxiv.org/content/10.1101/2020.03.21.20040329v1.full.pdf
serial.interval = 6.7
upper.limit.latent = 2.52

latent.period <- upper.limit.latent
infectious.period <- serial.interval - upper.limit.latent

new.output.data[mask.Ozden,"R0.Ozden"] <- (1 + new.output.data[mask.Ozden,"tau.hat"]*new.output.data[mask.Ozden,"b1"])*(1 + new.output.data[mask.Ozden,"tau.hat"]*new.output.data[mask.Ozden,"b2"])

new.output.data$days_from_start <- max(na.omit(unique(new.output.data$days_from_start)))
new.output.data<-new.output.data[!(new.output.data$state=="US"),]



write.csv(new.output.data,"../data/output/file_to_plot/confusion_block_latest_R0.csv",row.names=FALSE)



# state	fips	county	date.x	weekly.new.cases	days_from_start	log_rolled_cases.x.x	t0.hat	tau.hat	Predicted_Double_Days	b1	b2	R0
simplified.data <- new.output.data[, names(new.output.data) %in% c("state", "fips", "county", "date.x", "weekly.new.cases", "days_from_start", "log_rolled_cases.x.x", "tau.hat", "Predicted_Double_Days","b1","b2","R0.Ozden")]
write.csv(simplified.data,"../data/output/file_to_plot/confusion_block_latest_R0_simplified.csv",row.names=FALSE)


closeAllConnections()

