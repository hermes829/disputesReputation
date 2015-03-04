# Creating tables

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
load('invProfAR1_All.rda'); allYrSumm=modSumm; allYrResults=modResults
load('invProfAR1_pre07.rda'); pre07Summ=modSumm; pre07Results=modResults

# Keep first two dispute measures
modSumm = list( allYrSumm[[1]], allYrSumm[[2]], pre07Summ[[1]], pre07Summ[[2]])
modResults = list( allYrResults[[1]], allYrResults[[2]], pre07Results[[1]], pre07Results[[2]])

fileTable='ecmResultsInvProfile.tex'; captionTable='ECM regression on investment profile with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
##########################################################################################

##########################################################################################
# Mods to match table to Karen's style
modNames = apply(expand.grid(ivsName[1:2], c('(All Years)', '(Pre-2007)')), 1, 
	function(x){ paste(x[1], x[2]) } )

varDef=rbind(
	cbind('pch_disputes', pchLabName('Registered Disputes')),
	cbind('lag_disputes', lagLabName('Registered Disputes')),
	cbind(ivAll[[1]][c(11,3,12,4,13,5,14,6,15,7,16,8,17,8)], 
		ivAllNames[[1]][c(11,3,12,4,13,5,14,6,15,7,16,8,17,8)]),
	cbind(ivAll[[1]][1], ivAllNames[[1]][1])
	)

# Changing name of each dispute measure in results to disputes
modSumm=lapply(modSumm, function(x){
	rownames(x)[2]='lag_disputes'
	rownames(x)[10]='pch_disputes'
	x
	})
##########################################################################################

##########################################################################################
# Creating APSR like tables
digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varDef[,1]), ncol=1+noModels)

tableResults[,1] = rep(varDef[,1],2)
colnames(tableResults) = c('Variable',modNames)
for(ii in 2:ncol(tableResults)){
	temp = modSumm[[ii-1]]
	n = modResults[[ii-1]]$df.residual
	temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
	estims = temp[1:nrow(varDef),'Estimate']
	estims = round(as.numeric(as.character(estims)),digs)
	tvals = abs(temp[1:nrow(varDef),'t value'])
	tvals = round(as.numeric(as.character(tvals)),digs)
	estims = ifelse(tvals>=qt(0.975,n) & !is.na(tvals) & tvals<qt(0.995,n), 
		paste('$', estims,'^{\\ast}$',sep=''), estims)
	estims = ifelse(tvals>=qt(0.995,n) & !is.na(tvals), 
		paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)	
	# estims = ifelse(tvals>=qt(0.975,n) & !is.na(tvals) & tvals<qt(0.995,n), 
	# 	paste('$', estims,'^{\\ast}$',sep=''), estims)
	# estims = ifelse(tvals>=qt(0.995,n) & !is.na(tvals) & tvals<qt(0.9995,n), 
	# 	paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
	# estims = ifelse(tvals>=qt(0.9995,n) & !is.na(tvals), 
	# 	paste('$', estims,'^{\\ast\\ast\\ast}$',sep=''), estims)			
	estims = ifelse(is.na(estims),'',estims)
	tableResults[1:nrow(varDef),ii] = estims
	serrors = temp[(nrow(varDef)+1):nrow(tableResults),'Std. Error']
	serrors = round(as.numeric(as.character(serrors)),digs)
	serrors = paste('(',serrors,')',sep='')
	serrors = ifelse(serrors=='(NA)','',serrors)
	tableResults[(nrow(varDef)+1):nrow(tableResults),ii] = serrors
}

# Reorganizing rows and variable labels
tableFinal = NULL
for(ii in 1:nrow(varDef)){
	temp = cbind('', t(tableResults[ii+nrow(varDef),2:ncol(tableResults)]))
	tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

# Adding other info
sSize = cbind('n', t(as.vector(mapply(x=modResults, 
	function(x) FUN=length(x$residuals)))))
gSize = cbind('N', t(as.vector(mapply(x=modResults, 
	function(x) FUN=sum(grepl('ccode', names(x$coefficients)))))))
rSQ = cbind('$R^{2}$', t(as.vector(mapply(x=modResults,
		function(x) FUN=round(x$r2,2) ))))
rmse = round(mapply(x=modResults, function(x) FUN=sqrt(mean(x$residuals^2))),2)
fRmse = cbind('RMSE', t(rmse))
tableFinal = rbind(tableFinal, sSize, gSize, rSQ, fRmse)
nStats=4
temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
tableFinal[,'Variable']=temp

# Add & before every period
tableFinal[,2:ncol(tableFinal)]=apply(tableFinal[,2:ncol(tableFinal)], c(1,2), 
	function(x){ 
		if( grepl('\\$', x) ){ gsub('\\$*\\.', '$&$.', x)
		} else { gsub('\\.', '&.', x) } })

setwd(pathGraphics)
print.xtable(xtable(tableFinal, align='llcccc', caption=captionTable),
	include.rownames=FALSE,
	# sanitize.text.function = function(x) x,
	# sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
	sanitize.text.function = identity,
	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats),
	size="footnotesize",	
	file=fileTable )