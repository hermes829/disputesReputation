# Cross-validation

### Load setup
if(Sys.info()['user']=='janus829'){
	source('~/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R') }

if(Sys.info()['user']=='s7m'){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }	

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

rands=sort(unique(modelData$year))
modelData$rand=modelData$year

# rands=1:5
# modelData$rand=0
# modelData$rand[modelData$year>=1987 & modelData$year<1992]=1
# modelData$rand[modelData$year>=1992 & modelData$year<1997]=2
# modelData$rand[modelData$year>=1997 & modelData$year<2002]=3
# modelData$rand[modelData$year>=2002 & modelData$year<2007]=4
# modelData$rand[modelData$year>=2007]=5

coefCross=NULL
for(ii in 1:length(rands)){

	slice=modelData[which(modelData$rand %in% rands[ii]), ]
	print(paste0('cross ',rands[ii], ' has ', nrow(slice), ' obs from ',
		length(unique(slice$ccode)), ' countries'))	

	# plmSlice=pdata.frame( slice[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
	# 		index=c('ccode','year') )

	# modResults=lapply(modForm, function(x)
	# 	FUN=plm(x, data=plmSlice, model='within') )

	# modSumm=lapply(modResults, function(x)
	# 	FUN=coeftest(x, vcov=vcovHC(x,method='arellano',cluster="group")))	

	modResults=lapply(modForm, function(x)
		FUN=lm(x, data=slice) )	

	modSumm=lapply(modResults, function(x)
		FUN=coeftest(x))		

	dispSumm=do.call(rbind, lapply(modSumm,function(x)FUN=x[2,,drop=FALSE]))
	
	# coefCross=rbind(coefCross, cbind(dispSumm,cross=ii))

	coefCross=rbind(coefCross, cbind(dispSumm,cross=rands[ii]))	
}
coefCross=coefCross[which(!rownames(coefCross) %in% 'lag_pch_gdp'),]
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
  facetBreaks=seq(1987,2011,3),
  facetLabs=seq(1987,2011,3),
  allBlack=FALSE
  )
temp
setwd(pathPaper)
tikz(file='crossValLevel.tex',width=8,height=6,standAlone=F)
temp
dev.off()
###############################################################################

# Number of disputes by year
yrDisp=summaryBy(data=modelData, FUN=sum, keep.names=TRUE,
	formula(paste0(paste(dispVars, collapse=' + '), '~ year')))
ggDisp=melt(yrDisp, id='year')
ggDisp$varLab=ivDispName[match(ggDisp$variable, dispVars)]

tmp=ggplot(ggDisp, aes(x=year, y=value))
tmp=tmp + geom_bar(stat='identity') + facet_wrap(~ varLab)
tmp=tmp + xlab('') + ylab('Frequency')
tmp=tmp + theme(legend.position='none', legend.title=element_blank(),
	axis.ticks=element_blank(), panel.grid.major=element_blank(),
	panel.grid.minor=element_blank())
tmp