# Creating coefficient plots

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###############################################################################
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

ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
###############################################################################

#######################################################################################
# Detrend
t=lm(Investment_Profile ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$Investment_Profile[!is.na(modelData$Investment_Profile)]=t$residuals

t=lm(pch_gdp ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$pch_gdp[!is.na(modelData$pch_gdp)]=t$residuals

t=lm(LNpopulation ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$LNpopulation[!is.na(modelData$LNpopulation)]=t$residuals

t=lm(lncinflation ~ poly(year,4, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$lncinflation[!is.na(modelData$lncinflation)]=t$residuals

t=lm(Internal_Conflict ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$Internal_Conflict[!is.na(modelData$Internal_Conflict)]=t$residuals

t=lm(ratifiedbits ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$ratifiedbits[!is.na(modelData$ratifiedbits)]=t$residuals

t=lm(kaopen ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$kaopen[!is.na(modelData$kaopen)]=t$residuals

t=lm(polity ~ poly(year,3, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$polity[!is.na(modelData$polity)]=t$residuals

t=lm(cum_kicsidcase ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$cum_kicsidcase[!is.na(modelData$cum_kicsidcase)]=t$residuals

t=lm(cum_icsidtreaty_case ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$cum_icsidtreaty_case[!is.na(modelData$cum_icsidtreaty_case)]=t$residuals

t=lm(cumunsettled_icsid_treaty ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$cumunsettled_icsid_treaty[!is.na(modelData$cumunsettled_icsid_treaty)]=t$residuals

t=lm(cumcunctadcase ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$cumcunctadcase[!is.na(modelData$cumcunctadcase)]=t$residuals

t=lm(cum_alltreaty ~ poly(year,2, raw=TRUE) + factor(ccode)-1, data=modelData)
modelData$cum_alltreaty[!is.na(modelData$cum_alltreaty)]=t$residuals
#######################################################################################

###############################################################################
# Model setup
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

# Setting up diff models
lagLab=function(x,y=NULL){ paste('lag',y,'_',x,sep='') }
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x), lagLab(ivOther) ) )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))
###############################################################################

###############################################################################
# Run cross val models

# Cross val by cuts of inv prof var
# avgRat=summaryBy(Investment_Profile ~ ccode, FUN=mean, na.rm=T, data=modelData)
# # slice=na.omit(modelData[,c('ccode','year','Investment_Profile')])
# # cyrs=numSM(paste0(unique(slice$ccode),ddply(slice, .(ccode), summarize, min(year))[,2]))

# avgRat=modelData[which(modelData$cyear %in% cyrs),c('ccode','Investment_Profile')]
# avgRat=avgRat[order(avgRat[,2]),]
# avgRat$rand=c(1,rep(1:5,each=18))
# modelData$rand=avgRat$rand[match(modelData$ccode,avgRat$ccode)]

# Random cross val
modelCntries=unique(modelData$ccode)
set.seed(round(runif(1,100,10000),0))
randSamp=data.frame(cbind(ccode=modelCntries, 
	rand=sample(1:9,length(modelCntries),replace=T)))
modelData$rand=randSamp$rand[match(modelData$ccode,randSamp$ccode)]

rands=sort(unique(na.omit(modelData[,'rand'])))
coefCross=NULL
for(ii in 1:length(rands)){

	slice=modelData[which(modelData$rand %in% rands[ii]), ]

	plmSlice=pdata.frame( slice[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

	modResults=lapply(modForm, function(x)
		FUN=plm(x, data=plmSlice, model='within') )

	modSumm=lapply(modResults, function(x)
		FUN=coeftest(x, vcov=vcovHC(x,method='arellano',cluster="group")))	

	dispSumm=matrix(unlist(lapply(modSumm,function(x)FUN=numSM(x[1,]))), 
		ncol=4, byrow=T, 
		dimnames=list(
			unlist(as.character(lapply(ivAll,function(x)FUN=x[1]))), 
			c('Estimate','Std. Error','tstat','pval')) )
	
	coefCross=rbind(coefCross, cbind(dispSumm,cross=ii))
}
###############################################################################

###############################################################################
# Plotting
VARS=unique(rownames(coefCross))
VARSname=c('All ICSID Disputes', 'ICSID Treaty-Based',
	'Unsettled ICSID', 'UNCTAD','ICSID-UNCTAD' )

temp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=TRUE, revVar=FALSE,
  facet=TRUE, facetColor=TRUE, colorGrey=TRUE,
  facetName='cross', facetDim=c(2,3),
  facetBreaks=NULL, facetLabs=NULL
  )
temp
# setwd(pathPaper)
# tikz(file='crossVal.tex',width=8,height=6,standAlone=T)
# temp
# dev.off()
# ###############################################################################