# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829"
){pathMain="~/Dropbox/Research/RemmerProjects/disputesReputation";
  pathGraphics="~/Dropbox/Research/RemmerProjects/disputesReputation/Graphics";
  pathData="~/Dropbox/Research/RemmerProjects/disputesReputation/Data";
  pathCode="~/Desktop/Research/RemmerProjects/disputesReputation/RCode";
  pathLatex="~/Desktop/Research/RemmerProjects/disputesReputation/Latex"
  pathFunctions="~/Desktop/Prog Notes/R Functions"}

load('~/Desktop/Research/BuildingPanelData/panel.rda')

# Loading libraries and functions
require(foreign)
require(countrycode)
require(xlsx)
require(gdata)
require(ggplot2)
theme_set(theme_bw())
require(reshape)
require(plyr)
require(doBy)
require(doBy)
require(WDI)
require(lme4)
require(cshapes)
require(xtable)
require(apsrtable)
require(tikzDevice)
setwd(pathFunctions)
source('clus_errors.R')
source('multiplot.R')

# Setting seed
set.seed(6886)
setwd(pathMain)

# Helper functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
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
numSM <- function(x){ as.numeric(as.character(x)) }

# The function to use is lagDataSM
# This takes a dataset, a variable country year which is 
# a concatenation of the country identifier and year
# Then a country variable
# The varsTOlag should be inputted as a vector
# And lag is just a numeric specifying how many years to lag the variable
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagDataSM <- function(data, country_year, country, varsTOlag, lag)
{
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2, 
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) ) 
    } )
  colnames(lagData) <- paste('lag', lag, '_', varsTOlag, sep='')
  cbind(data, lagData)
}

# calculate moving averages for panel datasets
moveAvgPanel <- function(data, ccode, timepd, vars, wdow){
	vars2 <- paste('mva',wdow,'_',vars,sep='')
	rollmeanSM <- function(x){ as.vector(filter(x, rep(1/wdow, wdow), sides=1)) }
	temp <- ddply(data[,c(ccode, vars)], .(ccode), numcolwise(rollmeanSM))
	temp <- matrix(temp[,vars],ncol=length(vars)); colnames(temp) <- vars2
	cbind(data, temp)
}

# Calculate cumulative sum of var
cumulTS <- function(
	data=combData, cntry_var='cname', time_var='year', key='cyear', 
		start=1960, end=2013, variable){
	print(paste('Progress in calculating cumulative sum for ', variable))
	cum_var <- paste('c',variable,sep='')
	temp <- data[data[,time_var]>=start & data[,time_var]<end,c(key,cntry_var,time_var,variable)]
	temp <- cbind(temp, 0)
	names(temp) <- c('cyear', 'cntry', 'year', variable, cum_var)
	
	countries <- unique(data[,cntry_var]); years <- start:end; fullData <- NULL

	for(ii in 1:length(countries)){
		slice <- temp[temp$cntry==countries[ii],]
		years <- min(slice$year):max(slice$year)
			for(jj in 2:length(years)){
				slice[slice$year==years[jj],cum_var] <- 
					slice[slice$year==years[jj],variable] + 
						slice[slice$year==(years[jj]-1),cum_var] }
			fullData <- rbind(fullData, slice) 
			if(ii==1 | ii%%20==0 | ii==length(countries)){
				cat(paste(round(100*ii/length(countries),0),'% ',sep=''))}
		}
	print(' Completed '); fullData[,c(key, variable, cum_var)]
}

# Create spatially weighted variables
# Requires access to SM created panel dataset
# In the dataset with the variables to be weighted it is 
# necessary to have country identifier given by ccode and
# time identifier given by year
spatialBuild <- function(spatList, varData, years, variable, sp_suffix, 
	invert=FALSE, rowST=TRUE){
	spatData <- NULL

	for(i in 1:length(years)){
		spatMat <- spatList[[i]]
		# rownames for matrices
		distNames <- as.numeric(rownames(spatMat))
		ndistNames <- panel$ccode[match(distNames, panel$GWCODE)]
		rownames(spatMat) <- ndistNames; colnames(spatMat) <- ndistNames

		# Invert
		if(invert){spatMat<-1/spatMat; spatMat[spatMat==Inf]<-1; diag(spatMat)<-0}

		# Applying row standardized weights
		if(rowST){dmatDenom <- apply(spatMat,1,sum)
				dmatDenom[dmatDenom==0] <- 1
				spatMat2 <- spatMat/dmatDenom
				} else {
					spatMat2 <- spatMat
				}
		
		# Bringing in data
		spat_vars <- c('ccode', variable)
		dataYear <- varData[varData$year==years[i], spat_vars]
		dataYear <- dataYear[which(dataYear$ccode %in% ndistNames),]
		o <- as.character(dataYear$ccode)
		
		spatMat2 <- spatMat2[o,o]
		# data rows with NAs that are in distance matrix
		# this is equivalent to just dropping them from teh 
		# spatial variable calculation
		dataYear[is.na(dataYear)] <- 0
		
		for(j in 1:nrow(spatMat2)){
			row_weights <- t(t(dataYear[,c(2:ncol(dataYear))]) %*%  spatMat2[j,])
			row_weights2 <- cbind(row_weights, years[i], dataYear$ccode[j])
			spatData <- rbind(spatData, row_weights2)
		}
		if(i==1 | i%%5==0 | i==length(years)){
			cat(paste(years[i],' ',sep=''))} }
	spatData <- data.frame(spatData, row.names=NULL)

	names(spatData) <- c(
		paste(sp_suffix,names(spatData)[1:(length(spat_vars)-1)],sep=''),
		'year','ccode')
	spatData$cyear <- paste(spatData$ccode, spatData$year, sep='') 
	spatData
}