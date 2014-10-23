# Creating coefficient plots

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.9.dta')
colnames(modelData)[colnames(modelData)=='lagcumcunctadcase']='lag_cumcunctadcase'
colnames(modelData)[colnames(modelData)=='lagcum_icsidtreaty_case']='lag_cum_icsidtreaty_case'
colnames(modelData)[colnames(modelData)=='lagcum_kicsidcase']='lag_cum_kicsidcase'
colnames(modelData)[colnames(modelData)=='lagpch_gdp']='lag_pch_gdp'

lagVars=c('cumunsettled_icsid_treaty','cum_alltreaty')
modelData$cyear=numSM(modelData$cyear)
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 1)
colnames(modelData)[(ncol(modelData)-1):ncol(modelData)]=paste0('lag_',lagVars)

ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 1)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 2)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 3)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 4)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 5)

modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
##########################################################################################

##########################################################################################
# Coef plot to show change over time

# Set up models
dv='Investment_Profile'; dvName='Investment Profile'

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

lagLab=function(x,y=NULL){ paste('lag',y,'_',x,sep='') }
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x), lagLab(ivOther) ) )

id=cbind(rep(ivDisp, each=5),1:5)
id2=tapply(id,rep(1:nrow(id),ncol(id)),function(i)i)

ivAll=lapply(id2, function(x) FUN=c( lagLab(x[1],x[2]), lagLab(ivOther) ) )

# Run models
plmData=pdata.frame( modelData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Pull out relevant information
dispSumm=matrix(unlist(lapply(modSumm,function(x)FUN=numSM(x[1,]))),
	ncol=4, byrow=T,
	dimnames=list(unlist(as.character(lapply(ivAll,function(x)FUN=x[1]))),
		c('Estimate','Std. Error','tstat','pval'))
	)
dispData=cbind(
	year=rep(1:5,length(ivDisp)),
	dispSumm[,1:2]
	)
dispData
rownames(dispData)=substr(rownames(dispSumm),6,100)
VARS=unique(rownames(dispData))
VARSname=c('All ICSID Disputes', 'ICSID Treaty-Based',
	'Unsettled ICSID', 'UNCTAD','ICSID-UNCTAD' )

temp <- ggcoefplot(coefData=dispData, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetName='year',
  # , facetDim=c(3,2), 
  facetBreaks=1:5,
  facetLabs=paste0('$\\beta_{t-',1:5,'}$'),
  colorGrey=T,grSTA=0.2,grEND=0.8
  )
temp
setwd(pathPaper)
tikz(file='lagEffect.tex',width=8,height=6,standAlone=T)
temp
dev.off()
##########################################################################################