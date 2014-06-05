### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

#######################################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.7.dta')
colnames(modelData)[colnames(modelData)=='lagcumcunctadcase']='lag_cumcunctadcase'
colnames(modelData)[colnames(modelData)=='lagcum_icsidtreaty_case']='lag_cum_icsidtreaty_case'
colnames(modelData)[colnames(modelData)=='lagcum_kicsidcase']='lag_cum_kicsidcase'
colnames(modelData)[colnames(modelData)=='lagpch_gdp']='lag_pch_gdp'

lagVars=c('cumunsettled_icsid_treaty','cum_alltreaty')
modelData$cyear=numSM(modelData$cyear)
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 1)
colnames(modelData)[(ncol(modelData)-1):ncol(modelData)]=paste0('lag_',lagVars)

# Adding yearly number of disputes
lagVars=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 2)
diffData=modelData[,paste0('lag_',lagVars)]-modelData[,paste0('lag2_',lagVars)]
colnames(diffData)=paste0('diff_',lagVars)
modelData=cbind(modelData,diffData)

modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
#######################################################################################

#######################################################################################
# Detrending
modelData$year1=modelData$year; modelData$year2=modelData$year^2
modelData$year3=modelData$year^3; modelData$year4=modelData$year^4

t=lm(kaopen ~ year1 + year2 + factor(ccode)-1, data=modelData)
t=lm(kaopen ~ poly(year, 4) + factor(ccode)-1, data=modelData)
summary(t)
summary(t)$'coefficients'[paste0('year',1:4),]

t=lm(Investment_Profile ~ year1 + year2  + year3 + factor(ccode)-1, data=modelData)
t=lm(Investment_Profile ~ poly(year, 3, raw=TRUE) + factor(ccode)-1, data=modelData)
# summary(t)$'coefficients'[paste0('year',1:3),]
modelData$resid=t$residuals

temp=summaryBy(Investment_Profile + resid ~ year, data=modelData, FUN=mean,na.rm=T)
plot(temp$year,temp$resid.mean, type='l')
plot(temp$year,temp$Investment_Profile.mean, type='l')

fm=function(var,time,roots){formula(paste0(
	paste0(var, '~'), paste0(time,1:roots,collapse='+'),'+factor(ccode)-1'))}
sumLM=function(x,data){summary(lm(x,data))$'coefficients'}
retSig=function(lmS, sig=0.05){lmS[which(lmS[,4]<=sig),]}

dv='kaopen'
roots=4
time='year'
data=modelData

# qDetrend=function(data,dv,time)

o=lapply(1:roots,function(x) FUN=retSig(sumLM(fm(dv,time,x), modelData)))

root=roots
while(root>0){

	x=try( o[[root]][paste0(time,1:root),], silent=T )
	if(class(x) =='try-error'){ 
		resid=lm(fm(dv,time,root), data=modelData  )$residuals		
		root=root-1  
	}
	 else { 
	 resid=lm(fm(dv,time,root), data=modelData  )$residuals
	 root=0
	  }
}

#######################################################################################

#######################################################################################
# Setting up models

dv='Investment_Profile'; dvName='Investment Profile'; fileRE='LinvProfRE.rda'; fileFE='LinvProfFE.rda'

ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

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

lagLab=function(x){ paste('lag_',x,sep='') }
diffLab=function(x){ paste('diff_',x,sep='') }
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

# pchLabName=function(x){ paste('\\%$\\Delta$ Change',x,sep=' ') }
# lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
# ivAllNames=lapply(ivDispName, function(x) FUN= c(lagLabName(dvName) ,
# 	lagLabName(x), lagLabName(ivOtherName), pchLabName(x), pchLabName(ivOtherName)) )
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
c(dv, unique(unlist(ivAll)))
plmData=pdata.frame( modelData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('B',fileFE))
#######################################################################################

# ###################################################################################
# # Model checks

# # Random vs. fixed
# fePLM=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
# rePLM=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='random') )
# mapply(function(x,y) phtest(x,y), x=fePLM, y=rePLM) # Indicates use fixed effects

# # Both serial corerlation & hetero: so we use arellano ses
# # Serial correlation
# mapply(function(x) pbgtest(x), x=fePLM) # Serial correlation present

# # Heteroskedasticity
# bpForm=lapply(ivAll, function(x) 
# 	FUN=as.formula( paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), '+ factor(ccode)', collapse='') ))
# mapply(function(x) bptest(x, data=modelData, studentize=F), x=bpForm) # Hetero present
# ###################################################################################