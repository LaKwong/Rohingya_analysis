
# PATS+ visualizer from Ajay Pillarisetti https://github.com/ajaypillarisetti/patsplus

# load packages
list.of.packages <- c("shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
#####


# Easiest way is to use runGitHub
runGitHub("patsplus_visualizer", "ajaypillarisetti")

# Run a tar or zip file directly
runUrl("https://github.com/ajaypillarisetti/patsplus_visualizer/archive/master.tar.gz")
runUrl("https://github.com/ajaypillarisetti/patsplus_visualizer/archive/master.zip")