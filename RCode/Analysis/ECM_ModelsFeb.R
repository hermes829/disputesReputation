### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')

### Throw out upper income countries
modelData = allData[allData$upperincome==0,]

#### Most recent code from Remmer
	# xtpcse pch_Investment_Profile
		# lag_Investment_Profile pch_cum_icsidtreaty L.cum_icsidtreaty
		# pch_ratifiedbits L.ratifiedbits pch_LNgdp lag_LNgdp
		# pch_LNpopulation lag_LNpopulation pch_kaopen lag_kaopen
		# pch_polity lag_polity
		# pch_lncinflation lag_lncinflation
		# lag_LNgdpCAP 
			# i.ccode if upperincome~=1, pairwise corr (psar1)

##########################################################################################
# Setting up models

# Choosing DV
# dv='pch_Investment.Profile'
dv='pch_Property.Rights'
ivDV=paste('lag',substr(dv, 4, nchar(dv)),sep='')

# Cum. Dispute vars
ivDisp=c('Cicsidtreaty_case','Ckicsidcase','Csettle', 'Cunsettled_icsid_treaty', 'Ccunctadcase')

# Other covariates
ivOther=c(
	'ratifiedbits',
	'LNgdp', 'LNpopulation',
	'kaopen', 'lncinflation',
	'polity', 'Internal.Conflict'
	)

pchLab=function(x){ paste('pch_',x,sep='') }
lagLab=function(x){ paste('lag_',x,sep='') }
ivAll=lapply(ivDisp, function(x) FUN= c(ivDV ,lagLab(x), lagLab(ivOther), pchLab(x), pchLab(ivOther)) )
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), '+ (1|ccode)', collapse='') ))

# Setting up variables names for display
# dvName='Investment Profile'
dvName='Property Rights'
ivDispName=c('ICSID Treaty', 'ICSID Non-Treaty', 'Settled', 'Unsettled', 'UNCTAD' )
ivOtherName=c('Ratif. BITs', 'Ln(GDP)', 'Ln(Pop.)', 'Capital Openness', 'Ln(Inflation)', 'Polity', 'Internal Stability')

pchLabName=function(x){ paste('\\%$\\Delta$ Change',x,sep=' ') }
lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
ivAllNames=lapply(ivDispName, function(x) FUN= c(lagLabName(dvName) ,
	lagLabName(x), lagLabName(ivOtherName), pchLabName(x), pchLabName(ivOtherName)) )
##########################################################################################

##########################################################################################
# Running random effect models
modResults=lapply(modForm, function(x) FUN=lmer(x, data=modelData))
##########################################################################################

##########################################################################################
# Saving results for further analysis
setwd(pathResults)
# save(modResults, ivAll, ivAllNames, dv, dvName, file='invProfRE.rda')
save(modResults, ivAll, ivAllNames, dv, dvName, file='propRightsRE.rda')
##########################################################################################