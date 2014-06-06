# Creating tables

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
# load('LinvProfFE.rda'); fileTable='LfeResultsInvProfile.tex'; captionTable='Fixed effects regression on investment profile with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
# load('BLinvProfFE.rda'); fileTable='BLfeResultsInvProfile.tex'; captionTable='Fixed effects regression on investment profile using balanced panel with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
load('detLinvProfFE.rda'); fileTable='detLfeResultsInvProfile.tex'; captionTable='Fixed effects regression on detrended investment profile using balanced panel with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
##########################################################################################

##########################################################################################
# matrix of vars and their names
varsTable=unlist(lapply(ivs, function(x) FUN=paste( c('lag_'), x, sep='' )))

lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
varsTableNames=unlist( lapply(ivsName, function(x) FUN= c(lagLabName(x))) )

# Mods to match table to Karen's style
modNames=ivsName[1:5]

varsTable[1:5]='lag_disputes'; varsTable=unique(varsTable)
varsTableNames[1:5]=lagLabName('Registered Disputes'); varsTableNames=unique(varsTableNames)

varDef=cbind(varsTable, varsTableNames)

# Changing name of each dispute measure in results to disputes
modSumm=lapply(modSumm, function(x) FUN={rownames(x)[1]='lag_disputes'; x})
##########################################################################################

##########################################################################################
# Creating APSR like tables
digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)

tableResults[,1] = rep(varsTable,2)
colnames(tableResults) = c('Variable',modNames)
for(ii in 2:ncol(tableResults)){
	temp = modSumm[[ii-1]]
	n = modResults[[ii-1]]$df.residual
	temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
	estims = temp[1:length(varsTable),'Estimate']
	estims = round(as.numeric(as.character(estims)),digs)
	tvals = abs(temp[1:length(varsTable),'t value'])
	tvals = round(as.numeric(as.character(tvals)),digs)
	estims = ifelse(tvals>=qt(0.975,n) & !is.na(tvals) & tvals<qt(0.995,n), 
		paste('$', estims,'^{\\ast}$',sep=''), estims)
	estims = ifelse(tvals>=qt(0.995,n) & !is.na(tvals) & tvals<qt(0.9995,n), 
		paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
	estims = ifelse(tvals>=qt(0.9995,n) & !is.na(tvals), 
		paste('$', estims,'^{\\ast\\ast\\ast}$',sep=''), estims)			
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
nStats=5

temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
tableFinal[,'Variable']=temp
tableFinal

setwd(pathPaper)
print.xtable(xtable(tableFinal, align='llccccc',
	# caption=captionTable
	), include.rownames=FALSE,
	# sanitize.text.function = function(x) x,
	# sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
	sanitize.text.function = identity,		
	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats), 	
	size="footnotesize",	
	file=fileTable
	)# Creating tables