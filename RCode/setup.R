# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"
){pathMain="~/Dropbox/Research/disputesReputation";
  pathGraphics='~/Research/disputesReputation/Latex/graphics'
  pathData="~/Dropbox/Research/disputesReputation/Data";
  pathRaw="~/Dropbox/Research/disputesReputation/Data/components/";
  pathBin="~/Dropbox/Research/disputesReputation/Data/binaries/";
  pathResults="~/Dropbox/Research/disputesReputation/Results";
  pathCode="~/Research/disputesReputation/RCode";
  pathLatex="~/Research/disputesReputation/Latex"
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

toLoad=c('foreign', 'countrycode', 'openxlsx', 'gdata', 'ggplot2',
  'reshape', 'scales', 'plyr', 'doBy', 'WDI', 'zoo', 'panelAR', 'magrittr',
  'lme4', 'plm', 'lmtest', 'xtable', 'apsrtable', 'MASS',
  'RColorBrewer', 'tikzDevice')
loadPkg(toLoad)

# ggplot theme
theme_set(theme_bw())

# sourcing helpers
setwd(pathCode)
source('textFunctions.R')
source('tscsHelpers.R')
source('relDataHelpers.R')
source('vizResults.R')

# Setting seed
set.seed(6886)
setwd(pathMain)

# minor Helper functions
allCombos = function(x,y){ as.vector(outer(x,y,paste0)) }

cname = function(x){ countrycode(x, 'country.name', 'country.name') }

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

chLab=function(s='diff',x){ paste0(s,'_',x) }
lagLab=function(x,n){ paste0('lag',n,'_',x) }
chLabName=function(x){ paste0('\\%$\\Delta$ ',x) }
lagLabName=function(x,mvs=FALSE, mvsNum=NULL){ 
  if(!mvs){ return(paste0(x, '$_{t-1}$')) }
  if(mvs){ return(paste0(x, '$_{(t-1) + (t-',mvsNum,')}$')) } }
dName = function(x, t=NULL){
  if(is.null(t)){ paste0('Cumulative ', x, '$_{t-1}$') } else { paste(x, ' (past', t, 'years)') } }

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
num <- function(x){ as.numeric(char(x)) }

mapVar=function(var, old, new){
  var=char(var)
  for(ii in 1:length(old)){ var[var==old[ii]]=new[ii] }
  return ( factor(var, levels=new) ) }