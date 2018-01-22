#######################################################################################
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/disputesReputation/RCode/setup.R') }

### Load data
load(paste0(pathBin, 'analysisData.rda'))
#######################################################################################

#######################################################################################
# Setting up models
aData$fdiLog2 = logNeg(aData$fdi) ; dv='fdiLog2'

# disputes
dispVars =  c( 'iDispB')
dispLabs = c('ICSID' )
ivDisp=c( paste0('mvs2_',dispVars), paste0('mvs5_',dispVars) )
ivDispName = c( dName(dispLabs,2), dName(dispLabs,5) )

# Other covariates
ivOther=c(
	'iDispBC',
	'gdpGr', 'gdpCapLog', 'popLog','inflLog',
	'intConf', 'extConf',
	'rbitNoDuplC', 'kaopen', 'polity', 'propRights'
	)
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1), 'globSumRFDI' ) )

# Setting up variables names for display
ivOtherName=c(
	'Cumulative ICSID',
	'\\%$\\Delta$ GDP', 'Ln(GDP per capita)', 'Ln(Pop.)', 'Ln(Inflation)', 
	'Internal Stability','External Stability',
	'Ratified BITs','Capital Openness','Polity', 'Property Rights'
	)
ivsName=lapply(1:length(ivDispName), function(x){ c(ivDispName[x], lagLabName(ivOtherName), 'World FDI') })
#######################################################################################

#######################################################################################
# Running fixed effect models with plm
plmData=pdata.frame( aData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], index=c('ccode','year') )
modForm=lapply(ivAll, function(x){
	as.formula( paste(dv,  paste(x, collapse=' + '), sep=' ~ ')) })
modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x){
	coeftest(x)[,c('Estimate','Std. Error', 't value', 'Pr(>|t|)')] })
#######################################################################################

#######################################################################################
# Creating APSR like tables
fileTable='LcumulativeIncludedResultsFDI.tex'
captionTable='Fixed effects regression on Ln(FDI flows) with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
varDef = cbind( unique(unlist(ivAll)),  unique(unlist(ivsName)) )
varDef = varDef[c(1,nrow(varDef)-1,nrow(varDef),2:(nrow(varDef)-2)),]

digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varDef[,1]), ncol=1+noModels)
tableResults[,1] = rep(varDef[,1],2)
colnames(tableResults) = c('Variable','Model 1', 'Model 2')
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

print.xtable(xtable(tableFinal, align='llcc', caption=captionTable),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats),
	size="footnotesize",	
	file=paste0(pathLatex, '/', fileTable) )
#######################################################################################