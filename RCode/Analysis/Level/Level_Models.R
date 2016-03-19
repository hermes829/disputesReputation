####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData.rda'))
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

# # Saving results for further analysis
# setwd(pathResults)
# fileFE2 = strsplit(fileFE, '\\.') %>% unlist() %>% paste(.,collapse='v2.')
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE2)
#######################################################################################

#######################################################################################
# Creating APSR like tables
fileTable='LfeResultsInvProfile.tex'
captionTable='Regression on investment profile using country fixed effects, robust standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
varDef = cbind( unique(unlist(ivAll)),  unique(unlist(ivsName)) )
varDef = varDef[c(1,nrow(varDef)-1,nrow(varDef),2:(nrow(varDef)-2)),]

digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varDef[,1]), ncol=1+noModels)
tableResults[,1] = rep(varDef[,1],2)
colnames(tableResults) = c('Variable',paste0('Model',1:noModels))
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

# setwd(pathGraphics)
# print.xtable(xtable(tableFinal, align='llcccccc', caption=captionTable),
# 	include.rownames=FALSE,
# 	sanitize.text.function = identity,
# 	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats),
# 	size="footnotesize",	
# 	file=fileTable )
#######################################################################################

#######################################################################################
# fix up r squared
modForm=lapply(ivAll, function(x){
	as.formula( paste0(paste(dv,  paste(x, collapse=' + '), sep=' ~ '), ' + factor(ccode) - 1')) })
modResults=lapply(modForm, function(x) FUN=lm(x, data=aData) )
lapply(modResults, function(x){
	c(summary(x)$'r.squared', summary(x)$'adj.r.squared')
	})
#######################################################################################

#######################################################################################
# Coefficient plot
# add conf ints
ggCoefData = lapply(modSumm, function(x){
	hi95 = x[,1] + qnorm(.975)*x[,2]
	lo95 = x[,1] - qnorm(.975)*x[,2]
	hi90 = x[,1] + qnorm(.95)*x[,2]
	lo90 = x[,1] - qnorm(.95)*x[,2]
	x = data.frame( cbind(x, hi95, lo95, hi90, lo90) )
	x$var = varDef[,2][match(rownames(x), varDef[,1])]
	x$mod = x$var[1]
	x$var[1] = 'ICSID'
	return(x) })
ggCoefData = do.call('rbind', ggCoefData)
ggCoefData$var = factor(ggCoefData$var, levels=rev(c('ICSID', varDef[,2][-(1:3)])))
ggCoefData$sig = NULL
ggCoefData$sig[ggCoefData$lo90 > 0 & ggCoefData$lo95 < 0] = "Positive at 90"
ggCoefData$sig[ggCoefData$lo95 > 0] = "Positive"
ggCoefData$sig[ggCoefData$hi90 < 0 & ggCoefData$hi95 > 0] = "Negative at 90"
ggCoefData$sig[ggCoefData$hi95 < 0] = "Negative"
ggCoefData$sig[ggCoefData$lo90 < 0 & ggCoefData$hi90 > 0] = "Insig"
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
                "Negative"= rgb(222, 45, 38, maxColorValue=255),
                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                "Insig" = rgb(150, 150, 150, maxColorValue=255))

ggCoefData = ggCoefData[grepl('Not ICSID',char(ggCoefData$mod)),] ; coefName = 'coefpRep_notICSIDv2'; ggCoefData$var = char(ggCoefData$var) ; ggCoefData$var[ggCoefData$var=='ICSID'] = 'Not ICSID' ; ggCoefData$var = factor(ggCoefData$var, levels=rev(c('Not ICSID', varDef[,2][-(1:3)])))
# ggCoefData = ggCoefData[!grepl('Not ICSID',char(ggCoefData$mod)),] ; coefName = 'coefpRep_ICSID'
coefp = ggplot(ggCoefData, aes(x=factor(var), y=Estimate, color=sig))
coefp = coefp + geom_linerange(aes(ymin=lo95, ymax=hi95), alpha = .3, size = 0.3)
coefp = coefp + geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1)
coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
coefp = coefp + geom_point(size=4, shape=20)
coefp = coefp + geom_errorbar(aes(ymin=lo95,ymax=hi95),linetype = 1,width = 0.1)
coefp = coefp + facet_wrap(~mod, nrow=1)
coefp = coefp + scale_colour_manual(values = coefp_colors)
coefp = coefp + coord_flip() + xlab('') + ylab('')
coefp = coefp + theme(
	legend.position='none',
	panel.grid = element_blank(), axis.ticks=element_blank() )
setwd(pathGraphics)
coefp
tikz(file=coefName,width=8,height=6,standAlone=F)
coefp
dev.off()
#######################################################################################