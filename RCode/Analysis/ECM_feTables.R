# Creating tables

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
# load('invProfFE.rda'); fileTable='feResultsInvProfile.tex'; captionTable='Fixed effects ECM regression on investment profile with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
load('propRightsFE.rda'); fileTable='feResultsPropRights.tex'; captionTable='Fixed effects ECM regression on the protection of property rights with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
##########################################################################################

##########################################################################################
# matrix of vars and their names
varsTable=unlist(lapply(ivs, function(x) FUN=paste( c('lag_','pch_'), x, sep='' )))
varsTable=c(varsTable[1:10], gsub('pch','lag',dv), varsTable[11:length(varsTable)])

pchLabName=function(x){ paste('\\%$\\Delta$',x,sep=' ') }
lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
varsTableNames=unlist( lapply(ivsName, function(x) FUN= c(lagLabName(x), pchLabName(x))) )
varsTableNames=c(varsTableNames[1:10], lagLabName(dvName), 
	varsTableNames[11:length(varsTableNames)])

varDef=cbind(varsTable, varsTableNames)
##########################################################################################

##########################################################################################
# Creating APSR like tables
digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)

tableResults[,1] = rep(varsTable,2)
colnames(tableResults) = c('Variable',paste('Model',1:noModels))
for(ii in 2:ncol(tableResults)){
	temp = modSumm[[ii-1]]
	n = modResults[[ii-1]]$df.residual
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
	function(x) FUN=length(x$residuals)))))
gSize = cbind('N', t(as.vector(mapply(x=modResults, 
	function(x) FUN=length(x$residuals)-x$df.residual-length(x$coefficient)))))
rSQ = cbind('$R^{2}$', t(as.vector(mapply(x=modResults,
		function(x) FUN=round(summary(x)$r.squared[1],2) ))))
arSQ = cbind('Adj. $R^{2}$', t(as.vector(mapply(x=modResults,
		function(x) FUN=round(summary(x)$r.squared[2],2) ))))
rmse = round(mapply(x=modResults, function(x) FUN=sqrt(mean(x$residuals^2))),2)
fRmse = cbind('RMSE', t(rmse))
tableFinal = rbind(tableFinal, sSize, gSize, rSQ, arSQ, fRmse)

temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
tableFinal[,'Variable']=temp

setwd(pathResults)
print.xtable(xtable(tableFinal, align='llccccc',
	caption=captionTable
	), include.rownames=FALSE,
	# sanitize.text.function = function(x) x,
	# sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
	sanitize.text.function = identity,		
	# hline.after=c(0,0,46,51,51), 
	hline.after=c(0,0,50,55,55), 	
	size="footnotesize",	
	file=fileTable
	)