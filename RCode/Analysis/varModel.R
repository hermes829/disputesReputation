####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData.rda'))
#######################################################################################
# Setting up models
dv=c( 'invProf', 'rfdiLog' )

# Cumulative disputes
ivDisp=c( 'iDispC','iDispBC', 'iuDispC' )

# Two year moving sum of disputes
dispVars=c('iDisp', 'iDispB', 'iuDisp')
ivDisp=paste0('mvs2_',dispVars)

# Other covariates
ivOther=c(
	'gdpGr'
	,'popLog'
	,'inflLog'
	, 'intConf'	
	,'rbitNoDuplC'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1) ) )

# Setting up variables names for display
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'ICSID-UNCTAD' )
ivOtherName=c(
	'\\%$\\Delta$ GDP'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
ivsName=lapply(ivDispName, function(x) FUN= c(lagLabName(x,TRUE), lagLabName(ivOtherName)))
#######################################################################################

#######################################################################################
# load var model
modData = na.omit(aData[,c('cname','ccode','year', dv, ivDisp, ivOther)])
modData$ccode = factor( modData$ccode  )

library(vars)
varMod = VAR(y=modData[,dv], p=1, type='none', exogen=modData[,c(ivDisp[1], ivOther, 'ccode')])
summary(varMod)
#######################################################################################