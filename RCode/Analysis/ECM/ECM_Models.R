####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData.rda'))
###############################################################################
# Setting up models
## Stata code
# xtpcse pch_Investment_Profile lag_Investment_Profile pch_cum_kicsidcase 
# lagcum_kicsidcase pch_LNgdp lag_LNgdp pch_LNpopulation lag_LNpopulation 
# pch_lncinflation lag_lncinflation pch_Internal_Conflict lag_Internal_Conflict 
# pch_ratifiedbits lag_ratifiedbits pch_kaopen lag_kaopen pch_polity lag_polity 
# i.ccode i.year if coecd ~=1 & year>1986, pairwise corr(psar1) 

# Choosing DV
dv='pch_invProf'; dvName='Investment Profile'
ivDV=paste0('lag1_',substr(dv, 5, nchar(dv)))

# Cumulative disputes
ivDisp=c( 'iDispC','iDispBC', 'iuDispC',
	paste0(allCombos(c('i','u','iu'), allCombos( c('Oil','Elec','OilElec'), c('','B') ) ), 'C')
)

# Other covariates
ivOther=c(
	'gdpLog'
	,'popLog'
	,'inflLog'
	, 'intConf'	
	,'rbitNoDuplC'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
chLab = function(s='diff',x){ paste0(s,'_',x) } 
ivs=c(ivDV, ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) 
	FUN= c(ivDV ,lagLab(x,1), lagLab(ivOther,1), lagLab(chLab(x=x),1), lagLab(chLab(x=ivOther),1) ) )

# Setting up variables names for display
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'ICSID-UNCTAD' )
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
	lagLabName(x), lagLabName(ivOtherName), lagLabName(chLabName(x)), lagLabName(chLabName(ivOtherName))) )

# Add 20097 interaction terms
aData$yr07 = ( aData$year >= 2007 ) + 0
aData$lagLongInt = aData$lag1_iuDispC * aData$yr07
aData$lagShortInt = aData$diff_iuDispC * aData$yr07

# ivAll[[3]] = c(ivAll[[3]], 'yr07', 'lagLongInt', 'lagShortInt')

# Check to make sur everything exists in dataframe
ivAll %>% unlist() %>% unique() %>% setdiff(., names(aData)) %>% length %>% print()
###############################################################################

# ###############################################################################
# ### Check balance of panel
# panelBalance(ivs=ivAll[[1]], dv=dv, group='cname', time='year', regData=aData)

# ## Create balanced panel based off
# # All vars used in model
# temp=na.omit(aData[,c('cname','ccode','year', ivDV, ivDisp, ivOther)])
# temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
# names(temp2)=unique(temp$cname); temp3=unlist(temp2)

# # Cutoff for dropping
# drop=names(temp3[temp3<10])

# # New data
# aData = aData[which(!aData$cname %in% drop),]
# ###############################################################################

###############################################################################
# Running PCSE models with p-specific AR1 autocorr structure
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( 
		paste(paste(dv, paste(x, collapse=' + '), sep=' ~ '), 
			# '+ factor(ccode) + factor(year) - 1', collapse='') ))
			'+ factor(ccode) - 1', collapse='') ))

# aData=aData[aData$year<=2011,]
# lapply(ivAll, function(x){ 
# 	slice = aData[,c(x,dv,'ccode','year')]
# 	dim(na.omit(slice))
#  })

modResults=lapply(modForm, function(x) FUN=panelAR(
	x, aData, 'ccode', 'year', 
	autoCorr = c("psar1"), panelCorrMethod="pcse",rhotype='breg', 
	complete.case=FALSE, rho.na.rm=TRUE ) )
modSumm=lapply(modResults, function(x) FUN=coeftest( x ) )
# lapply(modSumm, function(x){ x[c(2,10),1:3] })

modSumm[[1]]

# Saving results for further analysis
# setwd(pathResults)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, ivAllNames, file=fileAR1)
# ###############################################################################