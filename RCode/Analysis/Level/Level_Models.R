# Main modeling file

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('modelData.rda')

#######################################################################################
# Setting up models
dv='Investment_Profile'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# Cumulative disputes
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

# Two year moving sum of disputes
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'alltreaty')
ivDisp=paste0('mvs2_',dispVars)

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
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'ICSID-UNCTAD' )
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

# #####################################################################################
# ### Create semi-balanced panel based off
# # All vars used in model
# temp=na.omit(modelData[,c('cname','ccode','year', ivDisp, ivOther)])
# # Just ICRG
# # temp=na.omit(modelData[,c('cname','ccode','year', 'Investment.Profile')])
# temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
# names(temp2)=unique(temp$cname); temp3=unlist(temp2)
# drop=names(temp3[temp3<quantile(temp3,probs=.25)])
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
print(lapply(modSumm, function(x) x[1,,drop=FALSE]))

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('B',fileFE))
#######################################################################################

# Aside with interaction of year and dispute
plmData$yr07=ifelse(numSM(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag_mvs2_kicsidcase*plmData$yr07

form=formula(paste0('Investment_Profile ~ lag_mvs2_kicsidcase + yr07 + dispYr07 + lag_pch_gdp + lag_LNpopulation + 
    lag_lncinflation + lag_Internal_Conflict + lag_ratifiedbits + 
    lag_kaopen + lag_polity'))

modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))



plmData$yr07=ifelse(numSM(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag_mvs2_icsidtreaty_case*plmData$yr07

form=formula(paste0('Investment_Profile ~ lag_mvs2_icsidtreaty_case + yr07 + dispYr07 + lag_pch_gdp + lag_LNpopulation + 
    lag_lncinflation + lag_Internal_Conflict + lag_ratifiedbits + 
    lag_kaopen + lag_polity'))

modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))



plmData$yr07=ifelse(numSM(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag_mvs2_unsettled_icsid_treaty*plmData$yr07

form=formula(paste0('Investment_Profile ~ lag_mvs2_unsettled_icsid_treaty + yr07 + dispYr07 + lag_pch_gdp + lag_LNpopulation + 
    lag_lncinflation + lag_Internal_Conflict + lag_ratifiedbits + 
    lag_kaopen + lag_polity'))

modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))



plmData$yr07=ifelse(numSM(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag_mvs2_alltreaty*plmData$yr07

form=formula(paste0('Investment_Profile ~ lag_mvs2_alltreaty + yr07 + dispYr07 + lag_pch_gdp + lag_LNpopulation + 
    lag_lncinflation + lag_Internal_Conflict + lag_ratifiedbits + 
    lag_kaopen + lag_polity'))

modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))