# Cross-validation

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('modelData.rda')

###############################################################################
# Model setup
# Set up models
dv='Investment_Profile'; dvName='Investment Profile'

# Cumulative disputes
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

# Two year moving sum of disputes
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'cunctadcase', 'alltreaty')
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

# Setting up diff models
lagLab=function(x,y=NULL){ paste('lag',y,'_',x,sep='') }
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x), lagLab(ivOther) ) )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))
###############################################################################

###############################################################################
# Run cross val models

# Random cross val
modelCntries=unique(modelData$ccode)
set.seed(round(runif(1,100,10000),0))
randSamp=data.frame(cbind(ccode=modelCntries, 
	rand=sample(1:6,length(modelCntries),replace=T)))
modelData$rand=randSamp$rand[match(modelData$ccode,randSamp$ccode)]

rands=sort(unique(na.omit(modelData[,'rand'])))
coefCross=NULL
for(ii in 1:length(rands)){

	slice=modelData[which(modelData$rand %in% rands[ii]), ]
	print(paste0('cross ',rands[ii], ' has ', nrow(slice), ' obs from ',
		length(unique(slice$ccode)), ' countries'))	

	plmSlice=pdata.frame( slice[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

	modResults=lapply(modForm, function(x)
		FUN=plm(x, data=plmSlice, model='within') )

	modSumm=lapply(modResults, function(x)
		FUN=coeftest(x, vcov=vcovHC(x,method='arellano',cluster="group")))	

	dispSumm=do.call(rbind, lapply(modSumm,function(x)FUN=x[1,,drop=FALSE]))
	
	coefCross=rbind(coefCross, cbind(dispSumm,cross=ii))
}
###############################################################################

###############################################################################
# Plotting
VARS=unique(rownames(coefCross))
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 
	'UNCTAD','ICSID-UNCTAD' )
VARSname=lagLabName(ivDispName)

temp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(2,3),
  facetBreaks=NULL, facetLabs=NULL, allBlack=TRUE
  )
temp
setwd(pathPaper)
tikz(file='crossValLevel.tex',width=8,height=6,standAlone=F)
temp
dev.off()
###############################################################################