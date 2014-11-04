### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

#######################################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.9.dta')

# Use cumulative number of disputes 
colnames(modelData)[colnames(modelData)=='lagcumcunctadcase']='lag_cumcunctadcase'
colnames(modelData)[colnames(modelData)=='lagcum_icsidtreaty_case']='lag_cum_icsidtreaty_case'
colnames(modelData)[colnames(modelData)=='lagcum_kicsidcase']='lag_cum_kicsidcase'
colnames(modelData)[colnames(modelData)=='lagpch_gdp']='lag_pch_gdp'

lagVars=c('cumunsettled_icsid_treaty','cum_alltreaty')
modelData$cyear=numSM(modelData$cyear)
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 1)
colnames(modelData)[(ncol(modelData)-1):ncol(modelData)]=paste0('lag_',lagVars)

# For modeling
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

# Create two year moving sum of dispute variables
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'cunctadcase', 'alltreaty')
modelData=movePanel(modelData, 'ccode', 'year', dispVars, 2, sum=TRUE)
modelData=lagDataSM(modelData, 'cyear', 'ccode', paste0('mvs2_',dispVars), 1)
colnames(modelData)[(ncol(modelData)-4):ncol(modelData)]=paste0('lag_',paste0('mvs2_',dispVars))

# For modeling
ivDisp=paste0('mvs2_',dispVars)

# Narrow sample to relevant range
modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
#######################################################################################

#######################################################################################
# Setting up models
dv='Investment_Profile'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# Other covariates
ivOther=c(
	'pch_gdp'
	,'LNpopulation'
	,'lncinflation'
	, 'Internal_Conflict'	
	,'ratifiedbits'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x), lagLab(ivOther) ) )

# Setting up variables names for display
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'UNCTAD','ICSID-UNCTAD' )
ivOtherName=c(
	'\\%$\\Delta$ GDP'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
ivsName=c(ivDispName, ivOtherName)
#######################################################################################

# #####################################################################################
# ### Create balanced panel based off
# # All vars used in model
# temp=na.omit(modelData[,c('cname','ccode','year', ivDisp, ivOther)])
# # Just ICRG
# # temp=na.omit(modelData[,c('cname','ccode','year', 'Investment.Profile')])
# temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
# names(temp2)=unique(temp$cname); temp3=unlist(temp2)
# drop=names(temp3[temp3<max(temp3)])
# modelData = modelData[which(!modelData$cname %in% drop),]
# #####################################################################################

#######################################################################################
# Running fixed effect models with plm
plmData=pdata.frame( modelData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Peak at dispute var results
lapply(modSumm, function(x) x[1,,drop=FALSE])

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('B',fileFE))
#######################################################################################