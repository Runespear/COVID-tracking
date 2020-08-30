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
# R=(1+r/b1)/(1+r/b2)
new.output.data$R0 <- (1 + new.output.data$tau.hat/new.output.data$b1)*(1 + new.output.data$tau.hat/new.output.data$b2)

write.csv(new.output.data,"../data/output/file_to_plot/confusion_block_latest_R0.csv",row.names=FALSE)

closeAllConnections()