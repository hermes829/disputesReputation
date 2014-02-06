# Creating tables

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
load('invProfRE.rda')
# load('propRightsRE.rda')
##########################################################################################

##########################################################################################
# Extracting coef estims and serrors from lmer object
modelSumm = lapply(modResults, function(x) FUN=attributes(summary(x))[['coefs']])
varsTable=unique(unlist(ivAll))
varsTable=varsTable[c(2,10,18:25,1,3,11,4,12,5,13,6,14,7,15,8,16,9,17)]
varsTableNames=unique(unlist(ivAllNames))
varsTableNames=varsTableNames[c(2,10,18:25,1,3,11,4,12,5,13,6,14,7,15,8,16,9,17)]
varDef=cbind(varsTable, varsTableNames)
##########################################################################################

##########################################################################################
# Creating APSR like tables
digs=3; noModels=length(modelSumm)
tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)

tableResults[,1] = rep(varsTable,2)
colnames(tableResults) = c('Variable',paste('Model',1:noModels))
for(ii in 2:ncol(tableResults)){
	temp = modelSumm[[ii-1]]
	n = attributes(summary(modResults[[ii-1]]))[['dims']][2]
	temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
	estims = temp[1:length(varsTable),'Estimate']
	estims = round(as.numeric(as.character(estims)),digs)
	tvals = abs(temp[1:length(varsTable),'t value'])
	tvals = round(as.numeric(as.character(tvals)),digs)
	estims = ifelse(tvals>=qt(0.95,n) & !is.na(tvals) & tvals<qt(0.975,n), 
		paste('$', estims,'^{\\ast}$',sep=''), estims)
	estims = ifelse(tvals>=qt(0.975,n) & !is.na(tvals), 
		paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
	estims = ifelse(is.na(estims),'',estims)
	tableResults[1:length(varsTable),ii] = estims
	serrors = temp[(length(varsTable)+1):nrow(tableResults),'Std. Error']
	serrors = round(as.numeric(as.character(serrors)),digs)
	serrors = paste('(',serrors,')',sep='')
	serrors = ifelse(serrors=='(NA)','',serrors)
	tableResults[(length(varsTable)+1):nrow(tableResults),ii] = serrors
}

# Reorganizing rows and variable labels
tableFinal = NULL
for(ii in 1:length(varsTable)){
	temp = cbind('', t(tableResults[ii+length(varsTable),2:ncol(tableResults)]))
	tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

# Adding other info
sSize = cbind('n', t(as.vector(mapply(x=modResults,
		function(x) FUN=attributes(summary(x))[['dims']][2]))))
gSize = cbind('N', t(as.vector(mapply(x=modResults,
			function(x) FUN=attributes(summary(x))[['dims']][4]))))
fitStats = mapply(x=modResults, function(x) FUN=attributes(summary(x))[['AICtab']])
fitStats = cbind(rownames(fitStats), fitStats)[1:2,]
fit = matrix(c(as.character(fitStats[,1]),
	round(as.numeric(fitStats[,2:(noModels+1)]),2)),nrow=2)
rmse = round(mapply(x=modResults, function(x) 
	FUN=sqrt(mean(attributes(summary(x))[['resid']]^2))),2)
fRmse = cbind('RMSE', t(rmse))

tableFinal = rbind(tableFinal, sSize, gSize,
 # fit, 
 fRmse)

temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
tableFinal[,'Variable']=temp


setwd(pathResults)
print.xtable(xtable(tableFinal, align='llccccc',
	caption='Random effects ECM regression on investment profile with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	# caption='Random effects ECM regression on the protection of property rights with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	), include.rownames=FALSE,
	# sanitize.text.function = function(x) x,
	sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
	hline.after=c(0,0,50,53,53), 
	file='modResultsInvProfileV1.tex'
	# file='modResultsPropRightsV1.tex'
	# file='modResultsInvProfileV2.tex'
	# file='modResultsPropRightsV2.tex'
	)