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
# Detrend by country
detVars=c('Investment_Profile', 'pch_gdp' ,'LNpopulation' ,'lncinflation' , 'Internal_Conflict', 
'ratifiedbits' ,'kaopen' ,'polity', 'cum_kicsidcase','cum_icsidtreaty_case', 
'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty')

detDat=list()

for(jj in 1:length(detVars)){

	varSlice=na.omit(modelData[,c(detVars[jj], 'year', 'ccode', 'cyear')])
	cntries=unique(varSlice$ccode)

	detVar=NULL

	for(ii in cntries){

		varSlice$dv=varSlice[,detVars[jj]]
		r=lapply(1:4, function(x) FUN=
			lm(dv ~ poly(year,x,raw=TRUE), 
				data=varSlice[which(varSlice$ccode==ii),]) )

		# remove list items that had errors
		sig=0
		roots=unlist(lapply(r, function(x) FUN=nrow(na.omit(coeftest(x)))-1 ))
		if(roots[4]==4) { s=r[[4]]; ro=4
			} else {
				if(roots[3]==3) { s=r[[3]]; ro=3
			} else {
				if(roots[2]==2) {s=r[[2]]; ro=2
			} else {
				if(roots[1]==1) {s=r[[1]]; ro=1
			} else {
				if(roots[1]!=1) {sig=1; ro=0
			}
		} } } } 

		# check sig
		coefs=lapply(r, function(x) FUN=coeftest(x))
		while(sig==0){

			if(ro!=1){
					sig=sum(coefs[[ro]][paste0('poly(year, x, raw = TRUE)',1:ro),4]<0.05)/ro
					} else { 
						sig=sum(coefs[[ro]]['poly(year, x, raw = TRUE)',4]<0.05)/ro
					}
			if(sig!=1){ro=ro-1}
			if(ro==0){sig=1}
		}
		
		# Create det var
		if(ro>0){ t=r[[ro]]$'residuals'  } else { t = varSlice[which(varSlice$ccode==ii), detVars[jj] ] }
		detVar=append(detVar,t)
	}

	varSlice$detVar=detVar
	detDat[[jj]]=varSlice
}

# Unpack results into original dataframe
names(detDat)=detVars
for(ii in detVars){
	modelData[,ii] = detDat[[ii]][,'detVar'][match( modelData$cyear, detDat[[ii]][,'cyear']  )]
}
#######################################################################################

# #######################################################################################
# # Detrend with fixed effects
# t=lm(Investment_Profile ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$Investment_Profile[!is.na(modelData$Investment_Profile)]=t$residuals

# t=lm(pch_gdp ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$pch_gdp[!is.na(modelData$pch_gdp)]=t$residuals

# t=lm(LNpopulation ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$LNpopulation[!is.na(modelData$LNpopulation)]=t$residuals

# t=lm(lncinflation ~ poly(year,4, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$lncinflation[!is.na(modelData$lncinflation)]=t$residuals

# t=lm(Internal_Conflict ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$Internal_Conflict[!is.na(modelData$Internal_Conflict)]=t$residuals

# t=lm(ratifiedbits ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$ratifiedbits[!is.na(modelData$ratifiedbits)]=t$residuals

# t=lm(kaopen ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$kaopen[!is.na(modelData$kaopen)]=t$residuals

# t=lm(polity ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$polity[!is.na(modelData$polity)]=t$residuals

# t=lm(cum_kicsidcase ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$cum_kicsidcase[!is.na(modelData$cum_kicsidcase)]=t$residuals

# t=lm(cum_icsidtreaty_case ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$cum_icsidtreaty_case[!is.na(modelData$cum_icsidtreaty_case)]=t$residuals

# t=lm(cumunsettled_icsid_treaty ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$cumunsettled_icsid_treaty[!is.na(modelData$cumunsettled_icsid_treaty)]=t$residuals

# t=lm(cumcunctadcase ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$cumcunctadcase[!is.na(modelData$cumcunctadcase)]=t$residuals

# t=lm(cum_alltreaty ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
# modelData$cum_alltreaty[!is.na(modelData$cum_alltreaty)]=t$residuals
# #######################################################################################

#######################################################################################
# Setting up models
dv='Investment_Profile'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

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
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('B',fileFE))
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('det',fileFE))
#######################################################################################