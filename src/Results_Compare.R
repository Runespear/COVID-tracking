closeAllConnections()
list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime","rlist")
list.of.packages <- c(list.of.packages, "zoo", "dtw", "foreach", "evaluate","rlist","data.table","plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Set Working Directory to File source directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#registerDoParallel(cores=detectCores())

#################################################################
# Compare results of Block v.s. lm and slm wrt MAPE and MSE
#################################################################

output.folder <- "../data/output/"

block.mse.fname <- "block_mse_windowsize=2.csv"
block.mape.fname <- "block_mape_windowsize=2.csv"

lm.mse.fname <- "mse_table.csv"
lm.mape.fname <- "mape_table.csv"

block.mse.df <- read.csv(file.path(output.folder,block.mse.fname))
block.mape.df <- read.csv(file.path(output.folder,block.mape.fname))
lm.mse.df <- read.csv(file.path(output.folder,lm.mse.fname))
lm.mape.df <- read.csv(file.path(output.folder,lm.mape.fname))

# Drop the NA

block.mse.df <- na.omit(block.mse.df)
block.mape.df <- na.omit(block.mape.df)
lm.mse.df <- na.omit(lm.mse.df)
lm.mape.df <- na.omit(lm.mape.df)

mse.compare.df <- merge(x=block.mse.df,y=lm.mse.df,by="cutoff")
mape.compare.df <- merge(x=block.mape.df,y=lm.mape.df,by="cutoff")

# Add comparison column lm.mape.last/block.mape.last - 1

#mse.compare.df$max.compare.mse <- pmax(mse.compare.df$lm.mse/mse.compare.df$block.mse.last - 1,mse.compare.df$slm.mse/mse.compare.df$block.mse.last - 1)
mse.compare.df$compare.mse <- pmax(-mse.compare.df$block.mse.last/mse.compare.df$lm.mse +1,-mse.compare.df$block.mse.last/mse.compare.df$slm.mse + 1)

#mape.compare.df$max.compare.mape <- pmax(mape.compare.df$lm.mape/mape.compare.df$block.mape.last - 1,mape.compare.df$slm.mape/mape.compare.df$block.mape.last - 1)
mape.compare.df$compare.mape <- pmax(-mape.compare.df$block.mape.last/mape.compare.df$lm.mape + 1,-mape.compare.df$block.mape.last/mape.compare.df$slm.mape + 1)

write.csv(mse.compare.df,file.path(output.folder,"mse_compare.csv"),row.names=FALSE)
write.csv(mape.compare.df,file.path(output.folder,"mape_compare.csv"),row.names=FALSE)




