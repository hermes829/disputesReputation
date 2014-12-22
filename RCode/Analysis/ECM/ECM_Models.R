### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###############################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.11.dta')
colnames(modelData)[colnames(modelData)=='lagcumcunctadcase']='lag_cumcunctadcase'
colnames(modelData)[colnames(modelData)=='lagcum_icsidtreaty_case']='lag_cum_icsidtreaty_case'
colnames(modelData)[colnames(modelData)=='lagcum_kicsidcase']='lag_cum_kicsidcase'
colnames(modelData)[colnames(modelData)=='lagpch_gdp']='lag_pch_gdp'
colnames(modelData)[colnames(modelData)=='pch_cumunsettled_icsidtreaty']='pch_cumunsettled_icsid_treaty'

lagVars=c('cumunsettled_icsid_treaty','cum_alltreaty')
modelData$cyear=numSM(modelData$cyear)
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 1)
colnames(modelData)[(ncol(modelData)-1):ncol(modelData)]=paste0('lag_',lagVars)

# Adding yearly number of disputes
lagVars=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty')
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 2)
diffData=modelData[,paste0('lag_',lagVars)]-modelData[,paste0('lag2_',lagVars)]
colnames(diffData)=paste0('diff_',lagVars)
modelData=cbind(modelData,diffData)

modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
###############################################################################

###############################################################################
# Setting up models
# xtpcse pch_Investment_Profile lag_Investment_Profile pch_cum_kicsidcase 
# lagcum_kicsidcase pch_LNgdp lag_LNgdp pch_LNpopulation lag_LNpopulation 
# pch_lncinflation lag_lncinflation pch_Internal_Conflict lag_Internal_Conflict 
# pch_ratifiedbits lag_ratifiedbits pch_kaopen lag_kaopen pch_polity lag_polity 
# i.ccode if upperincome ~=1 & year>1986, pairwise corr(psar1) 

# Choosing DV
dv='pch_Investment_Profile'; dvName='Investment Profile'; fileAR1='invProfAR1.rda'
ivDV=paste('lag',substr(dv, 4, nchar(dv)),sep='')

# Cum. Dispute vars
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cum_alltreaty' )

# Add temporal dummy
yrCut=2008
modelData$time=0; modelData$time[modelData$year>yrCut]=1

# Interaction lagged variables
modelData$L_kicsid_T = modelData$lag_cum_kicsidcase*modelData$time
modelData$L_icsid_T = modelData$lag_cum_icsidtreaty_case*modelData$time
modelData$L_unsett_T = modelData$lag_cumunsettled_icsid_treaty*modelData$time
modelData$L_all_T = modelData$lag_cum_alltreaty*modelData$time

# Interaction perc change vars
modelData$C_kicsid_T = modelData$pch_cum_kicsidcase*modelData$time
modelData$C_icsid_T = modelData$pch_cum_icsidtreaty_case*modelData$time
modelData$C_unsett_T = modelData$pch_cumunsettled_icsid_treaty*modelData$time
modelData$C_all_T = modelData$pch_cum_alltreaty*modelData$time

# Other covariates
ivOther=c(
	'LNgdp'
	,'LNpopulation'
	,'lncinflation'
	, 'Internal_Conflict'	
	,'ratifiedbits'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDV, ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) 
	FUN= c(ivDV ,lagLab(x), lagLab(ivOther), pchLab(x), pchLab(ivOther)) )

# Setting up variables names for display
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'ICSID-UNCTAD' )
ivOtherName=c(
	'Ln(GDP)'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
ivsName=c(ivDispName, ivOtherName)
ivAllNames=lapply(ivDispName, function(x) FUN= c(lagLabName(dvName) ,
	lagLabName(x), lagLabName(ivOtherName), pchLabName(x), pchLabName(ivOtherName)) )

# Add temporal interactions
lagI=c('L_kicsid_T', 'L_icsid_T', 'L_unsett_T', 'L_all_T')
pchI=c('C_kicsid_T', 'C_icsid_T', 'C_unsett_T', 'C_all_T')
for(ii in 1:4){ 
	ivAll[[ii]] = append(ivAll[[ii]], 'time', 0)
	ivAll[[ii]] = append(ivAll[[ii]], lagI[ii], 3)
	ivAll[[ii]] = append(ivAll[[ii]], pchI[ii], 12)
}

tName=paste0('Post ', yrCut)
lagI=paste(lagLabName(ivDispName), tName, sep=' x ')
pchI=paste(pchLabName(ivDispName), tName, sep=' x ')
for(ii in 1:4){
	ivAllNames[[ii]] = append(ivAllNames[[ii]], tName, 0)
	ivAllNames[[ii]] = append(ivAllNames[[ii]], lagI[ii], 3)
	ivAllNames[[ii]] = append(ivAllNames[[ii]], pchI[ii], 12)	
}
###############################################################################

###############################################################################
### Check balance of panel
panelBalance(ivs=ivAll[[1]], dv=dv, group='cname', time='year', regData=modelData)

## Create balanced panel based off
# All vars used in model
temp=na.omit(modelData[,c('cname','ccode','year', ivDV, ivDisp, ivOther)])
temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
names(temp2)=unique(temp$cname); temp3=unlist(temp2)

# Cutoff for dropping
# drop=names(temp3[temp3<max(temp3)])
drop=names(temp3[temp3<10])

# New data
modelData = modelData[which(!modelData$cname %in% drop),]
###############################################################################

###############################################################################
# Running PCSE models with p-specific AR1 autocorr structure
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), '+ factor(ccode)-1', collapse='') ))
modResults=lapply(modForm, function(x) FUN=panelAR(x, modelData, 'ccode', 'year', 
	autoCorr = c("psar1"), panelCorrMethod="pcse",rhotype='breg', complete.case=TRUE  ) )
modSumm=lapply(modResults, function(x) FUN=coeftest(x) )

lapply(modSumm, function(x){ print(x[c(1,3,4,12,13),1:3]) })

# Saving results for further analysis
setwd(pathResults)
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileAR1)
###############################################################################