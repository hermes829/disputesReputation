####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData_upperincIncluded.rda'))
load(paste0(pathBin, 'textYr.rda'))
aData$storyCnt =  textYr$count[match(aData$year, textYr$year)]
aData$storyCntLog = log( aData$storyCnt + 1 )
#######################################################################################
# Setting up models
dv='invProf'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# disputes
dispVars =  c( 'iDispB', 'niDispB')
dispLabs = c('ICSID', 'Not ICSID' )
ivDisp=c( paste0('mvs2_',dispVars), paste0('mvs5_',dispVars), paste0(dispVars, 'C') )
lagLabName = function(x,mvs=TRUE,y=NULL){
	if(mvs){ return( paste0(x, ' (past ', y, ' years)') ) }
	if(!mvs){ return( paste0(x, '$_{t-1}$') ) }
}
ivDispName = c( lagLabName(dispLabs,T,2), lagLabName(dispLabs,T,5), paste0('Cumulative ', lagLabName(dispLabs,F) ))

# Other covariates
ivOther=c(
	'gdpGr'
	,'gdpCapLog'
	,'popLog'
	,'inflLog'
	, 'intConf'	
	,'extConf'
	,'rbitNoDuplC'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1) ) )

# Setting up variables names for display
ivOtherName=c(
	'\\%$\\Delta$ GDP'
	,'Ln(GDP per capita)'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	, 'External Stability'
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
ivsName=lapply(ivDispName, function(x) FUN= c(x, lagLabName(ivOtherName)))
#######################################################################################

#######################################################################################
# Running fixed effect models with plm
aData$year2 = aData$year
plmData=pdata.frame( aData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year', 'year2', 'storyCnt', 'storyCntLog') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Peak at dispute var results
do.call('rbind',lapply(modSumm, function(x) x[1,,drop=FALSE]))

sub = res = data.frame( do.call('rbind',lapply(modSumm, function(x) x[1,,drop=FALSE])) )

# Calculate sd effects
sdEffect = function(var, coef, data){
	beta = coef[var,1]
	sdX = sd(data[,var],na.rm=T)
	sdY = sd(data[,'invProf'],na.rm=T)
	eff = beta*(sdX/sdY)
	return(eff)
}

sub$eff = lapply(rownames(sub), function(x){ sdEffect(x,sub,aData) }) %>% unlist()
sub[order(sub$eff),c(1,3,5)]
#######################################################################################

#######################################################################################
# Creating APSR like tables
fileTable='LupperIncludedResultsInvProfile.tex'
captionTable='Regression on investment profile using country fixed effects, robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
varDef = cbind( unique(unlist(ivAll)),  unique(unlist(ivsName)) )
varDef = varDef[c(1,nrow(varDef)-1,nrow(varDef),2:(nrow(varDef)-2)),]

digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varDef[,1]), ncol=1+noModels)
tableResults[,1] = rep(varDef[,1],2)
colnames(tableResults) = c('Variable',paste0('Model ',1:noModels))
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

setwd(pathLatex)
print.xtable(xtable(tableFinal, align='llcccccc', caption=captionTable),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats),
	size="footnotesize",	
	file=fileTable )
#######################################################################################