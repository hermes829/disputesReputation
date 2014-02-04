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
  if(l>0){
	cuts <- (length(x)-(l-1)):length(x)
	c(rep(NA,l), x[ -cuts ] )
    	} else {
		cuts <- (1:abs(l))
	    c( x[ -cuts ], rep(NA,abs(l)) )
    	}  
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

# Order panel dataset
orderTS=function(x, unit, time){ x=x[order(x[,time]),]; x=x[order(x[,unit]),]; x }

# calculate moving averages for panel datasets
movePanel <- function(data, ccode, time, vars, wdow, sum=FALSE){
	data=orderTS(data, ccode, time)
	vars2 <- paste('mva',wdow,'_',vars,sep='')
	vars3 <- paste('mvs',wdow,'_',vars,sep='')
	rollmeanSM <- function(x){ as.vector(filter(x, rep(1/wdow, wdow), sides=1)) }
	temp <- ddply(data[,c(ccode, vars)], .(ccode), numcolwise(rollmeanSM))
	if(sum==FALSE){
		temp <- temp[,vars]; colnames(temp) <- vars2 }else{
		temp <- temp[,vars]*wdow; colnames(temp) <- vars3 }
	cbind(data, temp)
}

# Calculate cumulative sum of var
cumulTS=function(data, ccode, time, vars){
	data=orderTS(data, ccode, time)	
	vars2=paste('C',vars,sep='')
	cumsumSM=function(x){x[is.na(x)]=0; cumsum(x)}
	temp=ddply(data[,c(ccode,vars)], .(ccode), cumsumSM)
	temp=temp[,vars]; colnames(temp)=vars2
	temp[which(is.na(data[,vars]),arr.ind=TRUE)]=NA
	cbind(data, temp) 
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

# Table summaries for lm objects
lmtableSM = function(coefs, vnames, modelResults, modelSumm, modelNames, digs=3){
	noModels=length(modelSumm)
	tableResults = matrix('', nrow=2*length(coefs), ncol=1+noModels)
	tableResults[,1] = rep(coefs,2)
	colnames(tableResults) = c('Variable',paste('Model',1:noModels))
	for(ii in 2:ncol(tableResults)){
		temp = modelSumm[[ii-1]]
		df = summary(modelResults[[ii-1]])[['df']][2]
		temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
		estims = temp[1:length(coefs),'Estimate']
		estims = round(as.numeric(as.character(estims)),digs)
		tvals = abs(temp[1:length(coefs),'t value'])
		tvals = round(as.numeric(as.character(tvals)),digs)
		estims = ifelse(tvals>=qt(0.95,df) & !is.na(tvals) & tvals<qt(0.975,df), 
			paste('$', estims,'^{\\ast}$',sep=''), estims)
		estims = ifelse(tvals>=qt(0.975,df) & !is.na(tvals), 
			paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
		tableResults[1:length(coefs),ii] = estims
		serrors = temp[(length(coefs)+1):nrow(tableResults),'Std. Error']
		serrors = round(as.numeric(as.character(serrors)),digs)
		serrors = paste('(',serrors,')',sep='')
		serrors = ifelse(serrors=='(NA)','',serrors)
		tableResults[(length(coefs)+1):nrow(tableResults),ii] = serrors
	}

	# Reorganizing rows and variable labels
	tableFinal = NULL
	for(ii in 1:length(coefs)){
		temp = cbind('', t(tableResults[ii+length(coefs),2:ncol(tableResults)]))
		tableFinal = rbind(tableFinal, tableResults[ii,], temp) }
	tableFinal[,'Variable'] = vnames[match(tableFinal[,'Variable'],coefs)]
	tableFinal[,'Variable'][is.na(tableFinal[,'Variable'])] = ''

	# Adding other info
	sSize = cbind('n', t(as.vector(mapply(x=modelResults,
			function(x) FUN=summary(x)[['df']][2] + summary(x)[['df']][1] ))))
	gSize = cbind('N', t(as.vector(mapply(x=modelResults, function(x)
		FUN=summary(x)[['df']][1]-length(attr(summary(x)[['terms']],'term.labels'))+1 ))))
	rSQ = cbind('$R^{2}$', t(as.vector(mapply(x=modelResults,
			function(x) FUN=round(summary(x)[['r.squared']],2) ))))
	arSQ = cbind('Adj. $R^{2}$', t(as.vector(mapply(x=modelResults,
			function(x) FUN=round(summary(x)[['adj.r.squared']],2) ))))
	rmse = round(mapply(x=modelResults, function(x) 
		FUN=sqrt(mean(summary(x)[['residuals']]^2))),2)
	frmse = cbind('RMSE', t(rmse))

	tableFinal = rbind(tableFinal, sSize, gSize, rSQ, arSQ, frmse)
	colnames(tableFinal)[2:(noModels+1)] = modelNames
	tableFinal
}