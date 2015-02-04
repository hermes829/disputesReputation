# Creating tables

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
load('LinvProfFE.rda'); fileTable='LfeResultsInvProfile.tex'; captionTable='Fixed effects regression on investment profile with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
# load('BLinvProfFE.rda'); fileTable='BLfeResultsInvProfile.tex'; captionTable='Fixed effects regression on investment profile using balanced panel with robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
##########################################################################################

##########################################################################################
# Mods to match table to Karen's style
modNames=unlist(lapply(ivsName, function(x) x[1]))
varDef=rbind(
	cbind('lag_disputes', lagLabName('Registered Disputes')),
	cbind(ivAll[[1]][2:8], ivsName[[1]][2:8] ) )

# Changing name of each dispute measure in results to disputes
modSumm=lapply(modSumm, function(x) FUN={rownames(x)[1]='lag_disputes'; x})
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