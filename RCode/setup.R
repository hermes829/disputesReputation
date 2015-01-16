# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"
){pathMain="~/Dropbox/Research/RemmerProjects/disputesReputation";
  pathGraphics="~/Dropbox/Research/RemmerProjects/disputesReputation/Graphics";
  pathData="~/Dropbox/Research/RemmerProjects/disputesReputation/Data";
  pathResults="~/Dropbox/Research/RemmerProjects/disputesReputation/Results";
  pathCode="~/Research/RemmerProjects/disputesReputation/RCode";
  pathLatex="~/Research/RemmerProjects/disputesReputation/Latex"
  pathFunctions="~/Prog Notes/R Functions"
  pathPaper="~/Dropbox/Research/RemmerProjects/disputesReputation/IO/graphics"
  load('~/Research/BuildingPanelData/panel.rda')
}



# Loading libraries and functions
library(foreign)
library(countrycode)
library(xlsx)
library(gdata)
library(ggplot2)
theme_set(theme_bw())
library(reshape)
library(scales)
library(plyr)
library(doBy)
library(WDI)
library(zoo)
library(panelAR)
library(lme4)
library(plm)
library(lmtest)
library(xtable)
library(apsrtable)
library(tikzDevice)

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
lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }

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
numSM <- function(x){ as.numeric(as.character(x)) }