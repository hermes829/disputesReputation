# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829"
){pathMain="~/Dropbox/Research/RemmerProjects/disputesReputation";
  pathGraphics="~/Dropbox/Research/RemmerProjects/disputesReputation/Graphics";
  pathData="~/Dropbox/Research/RemmerProjects/disputesReputation/Data";
  pathCode="~/Desktop/Research/RemmerProjects/disputesReputation/RCode";
  pathFunctions="~/Desktop/Prog Notes/R Functions"}

# Loading libraries and functions
require(countrycode)
setwd(pathFunctions)
require(xlsx)
require(gdata)

# Setting seed
set.seed(6886)
setwd(pathMain)