### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')
karenData=read.dta('Dispute Data.dta')
karenData$cyear=paste(karenData$ccode, karenData$year, sep='')

### Throw out upper income countries
modelData = allData[allData$upperincome==0,]
modelData = modelData[modelData$year>1986,]

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
dv='pch_Investment.Profile'; dvName='Investment Profile'; fileRE='invProfRE.rda'; fileFE='invProfFE.rda'; fileAR1='invProfAR1.rda'
# dv='pch_Property.Rights'; dvName='Property Rights'; fileRE='propRightsRE.rda'; fileFE='propRightsFE.rda'; fileAR1='propRightsAR1.rda'
ivDV=paste('lag',substr(dv, 4, nchar(dv)),sep='')

# Cum. Dispute vars
ivDisp=c('Cicsidtreaty_case','Ckicsidcase','Csettle', 'Cunsettled_icsid_treaty', 'Ccunctadcase')

# Other covariates
ivOther=c(
	'ratifiedbits',
	'LNgdp', 'LNpopulation',
	'kaopen', 'lncinflation',
	'polity'
	, 'Internal.Conflict'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)

pchLab=function(x){ paste('pch_',x,sep='') }
lagLab=function(x){ paste('lag_',x,sep='') }
ivAll=lapply(ivDisp, function(x) FUN= c(ivDV ,lagLab(x), lagLab(ivOther), pchLab(x), pchLab(ivOther)) )

# Setting up variables names for display
ivDispName=c('ICSID Treaty', 'ICSID Non-Treaty', 'Settled', 'Unsettled', 'UNCTAD' )
ivOtherName=c('Ratif. BITs', 'Ln(GDP)', 'Ln(Pop.)', 'Capital Openness', 'Ln(Inflation)', 'Polity'
	, 'Internal Stability'
	)
ivsName=c(ivDispName, ivOtherName)

# pchLabName=function(x){ paste('\\%$\\Delta$ Change',x,sep=' ') }
# lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
# ivAllNames=lapply(ivDispName, function(x) FUN= c(lagLabName(dvName) ,
# 	lagLabName(x), lagLabName(ivOtherName), pchLabName(x), pchLabName(ivOtherName)) )
##########################################################################################

### Check balance of panel
panelBalance(ivs=ivAll[[1]], dv=dv, group='cname', time='year', regData=modelData)

## Create balanced panel based off
# All vars used in model
temp=na.omit(modelData[,c('cname','ccode','year', ivDV, ivDisp, ivOther)])
# Just ICRG
# temp=na.omit(modelData[,c('cname','ccode','year', 'Investment.Profile')])
temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
names(temp2)=unique(temp$cname); temp3=unlist(temp2)
drop=names(temp3[temp3<max(temp3)])
modelData = modelData[which(!modelData$cname %in% drop),]

panelBalance(ivs=ivAll[[1]], dv=dv, group='cname', time='year', regData=modelData)

# Check in stata
setwd(pathData)
write.dta(modelData, file='temp.dta')
##########################################################################################
# Running random effect models
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), '+ (1|ccode)', collapse='') ))
modResults=lapply(modForm, function(x) FUN=lmer(x, data=modelData))

# Saving results for further analysis
setwd(pathResults)
save(modResults, ivAll, dv, ivs, ivsName, dvName, file=fileRE)
##########################################################################################

##########################################################################################
# Running PCSE models with p-specific AR1 autocorr structure
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), '+ factor(ccode)-1', collapse='') ))
modResults=lapply(modForm, function(x) FUN=panelAR(x, modelData, 'ccode', 'year', 
	autoCorr = c("psar1"), panelCorrMethod="pcse",rhotype='breg', complete.case=TRUE  ) )
modSumm=lapply(modResults, function(x) FUN=coeftest(x) )

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileAR1)
##########################################################################################

##########################################################################################
# Running fixed effect models with plm
plmData=pdata.frame( modelData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))
modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=function(x) vcovBK(x, type="HC1", cluster="group")))

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
##########################################################################################