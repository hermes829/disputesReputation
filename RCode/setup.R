# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"
){pathMain="~/Dropbox/Research/RemmerProjects/disputesReputation";
  pathGraphics='~/Research/RemmerProjects/disputesReputation/Latex/graphics'
  pathData="~/Dropbox/Research/RemmerProjects/disputesReputation/Data";
  pathResults="~/Dropbox/Research/RemmerProjects/disputesReputation/Results";
  pathCode="~/Research/RemmerProjects/disputesReputation/RCode";
  pathLatex="~/Research/RemmerProjects/disputesReputation/Latex"
  pathFunctions="~/Prog Notes/R Functions"
  load('~/Research/BuildingPanelData/panel.rda')
}

# Loading libraries and functions
loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) )
  }
}

toLoad=c('foreign', 'countrycode', 'xlsx', 'gdata', 'ggplot2',
  'reshape', 'scales', 'plyr', 'doBy', 'WDI', 'zoo', 'panelAR',
  'lme4', 'plm', 'lmtest', 'xtable', 'apsrtable', 'tikzDevice', 'MASS',
  'RColorBrewer\')

loadPkg(toLoad)

# ggplot theme
theme_set(theme_bw())

# sourcing helpers
setwd(pathCode)
source('textFunctions.R')
source('tscsHelpers.R')
source('vizResults.R')

# Setting seed
set.seed(6886)
setwd(pathMain)

# minor Helper functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

pchLab=function(x){ paste('pch_',x,sep='') }
lagLab=function(x){ paste('lag_',x,sep='') }
pchLabName=function(x){ paste('\\%$\\Delta$ Change',x,sep=' ') }
lagLabName=function(x,mvs2=FALSE){ 
  if(!mvs2){ return(paste(x, '$_{t-1}$', sep='')) }
  if(mvs2){ return(paste(x, '$_{(t-1) + (t-2)}$', sep='')) }
}

# Log transformations for vars with negative values
logNeg <- function(z){
	x <- z[!is.na(z)]; y <- x
	y[x>0] <- log(x[x>0]); y[x<0] <- -log(abs(x[x<0])); y[x==0] <- 0
	z[!is.na(z)] <- y; z
}

# Rescaling variables
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

# turn variables into numeric
char = function(x){ as.character(x) }
numSM <- function(x){ as.numeric(char(x)) }

mapVar=function(var, old, new){
  var=char(var)
  for(ii in 1:length(old)){ var[var==old[ii]]=new[ii] }
  return ( factor(var, levels=new) ) }