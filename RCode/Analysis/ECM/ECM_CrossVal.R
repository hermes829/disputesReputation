### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###############################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.13.dta')
colnames(modelData)[colnames(modelData)=='cumunsettled_icsidtreaty']='cumunsettled_icsid_treaty'
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

modelData = modelData[modelData$coecd==0,]
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

# Create interaction terms
modelData$L_kicsid_lac = modelData$lag_cum_kicsidcase*modelData$lac
modelData$L_icsid_lac = modelData$lag_cum_icsidtreaty_case*modelData$lac
modelData$L_unsett_lac = modelData$lag_cumunsettled_icsid_treaty*modelData$lac
modelData$L_all_lac = modelData$lag_cum_alltreaty*modelData$lac

# Add interactions to variable lists
lagI=c('L_kicsid_lac', 'L_icsid_lac', 'L_unsett_lac', 'L_all_lac')
for(ii in 1:4){ 
	ivAll[[ii]] = append(ivAll[[ii]], 'lac', 1)
	ivAll[[ii]] = append(ivAll[[ii]], lagI[ii], 3)
}

iName='LAC'
lagI=paste(lagLabName(ivDispName), iName, sep=' x ')
for(ii in 1:4){
	ivAllNames[[ii]] = append(ivAllNames[[ii]], iName, 1)
	ivAllNames[[ii]] = append(ivAllNames[[ii]], lagI[ii], 3)
}
###############################################################################

# ###############################################################################
# ### Check balance of panel
# panelBalance(ivs=ivAll[[1]], dv=dv, group='cname', time='year', regData=modelData)

# ## Create balanced panel based off
# # All vars used in model
# temp=na.omit(modelData[,c('cname','ccode','year', ivDV, ivDisp, ivOther)])
# temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
# names(temp2)=unique(temp$cname); temp3=unlist(temp2)

# # Cutoff for dropping
# # drop=names(temp3[temp3<max(temp3)])
# drop=names(temp3[temp3<10])

# # New data
# modelData = modelData[which(!modelData$cname %in% drop),]
# ###############################################################################

###############################################################################
# Run cross val models
modForm=lapply(ivAll, function(x) 
	FUN=as.formula( 
		paste(paste(dv, 
			paste(x, collapse=' + '), sep=' ~ '), '+ factor(ccode) + factor(year)-1', collapse='') ))

# Random cross val
modelCntries=unique(modelData$ccode)
set.seed(round(runif(1,100,10000),0))
randSamp=data.frame(cbind(ccode=modelCntries, 
	rand=sample(1:6,length(modelCntries),replace=T)))
modelData$rand=randSamp$rand[match(modelData$ccode,randSamp$ccode)]

rands=sort(unique(na.omit(modelData[,'rand'])))
coefCross=NULL
for(ii in 1:length(rands)){

	slice=modelData[modelData$rand == rands[ii], ]
	print(paste0('cross ',rands[ii], ' has ', nrow(slice), ' obs from ',
		length(unique(slice$ccode)), ' countries'))
	temp=unique(slice[,c('lac', 'cname')])
	print( paste0(sum(temp[,1]), ' of those countries are LAC') )
	print( temp[temp[,1]==1, 2] )

	modResults=lapply(modForm, function(x) FUN=panelAR(x, slice, 'ccode', 'year', 
		autoCorr = c("psar1"), panelCorrMethod="pcse",rhotype='breg', complete.case=FALSE  ) )

	modSumm=lapply(modResults, function(x) FUN=coeftest(x) )

	dispSumm=do.call(rbind, lapply(modSumm,function(x)FUN=x[c(3:4, 12),]))
	
	coefCross=rbind(coefCross, cbind(dispSumm,cross=ii))
}
###############################################################################

###############################################################################
# Plotting
# VARS=unique(rownames(coefCross))[seq(1,10,2)]
VARS=unique(rownames(coefCross))[seq(2, 11, 3)]
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'ICSID-UNCTAD' )
VARSname=lagLabName(ivDispName)

temp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(2,2),
  facetBreaks=1:6, facetLabs=1:6, allBlack=TRUE
  )
temp
setwd(pathPaper)
tikz(file='crossValECM.tex',width=8,height=6,standAlone=F)
temp
dev.off()
###############################################################################