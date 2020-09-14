# Set Working Directory to File source directory
require("pacman")
p_load("here")
setwd(file.path(here(),"src"))

mainDir = "../data"
block_dir = file.path(mainDir, "block_windowsize=2")
unlink(block_dir, recursive = TRUE)

mainDir = "../data/output"
backtestfolder = file.path(mainDir, "backtest_state_forests_windowsize=2")
backtest0folder = file.path(mainDir, "backtest")
confusionfolder = file.path(mainDir, "confusion_state_forests_windowsize=2")
countyfolder = file.path(mainDir, "Backtest_by_County_Windowsize=2")

unlink(backtestfolder, recursive = TRUE)
unlink(backtest0folder, recursive = TRUE)
unlink(confusionfolder, recursive = TRUE)
unlink(countyfolder, recursive = TRUE)


#2
source("Preprocess_us-counties-states.R")

#3
source("Specific_Cases(from Day 0).R")

#4
source("Block_Prepare.R")

#5
source("State_Forests.R")

#6
source("Preprocess_us-counties-states_minus7.R")

#7
source("Post-Process-Block-Forests.R")

#8
source("RMSE_Plot.R")

#9
source("MAPE_Plot.R")

#10
source("county_plot.R")