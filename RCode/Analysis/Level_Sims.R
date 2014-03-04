# Running simulations for sub effects

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Load data
setwd(pathData)
load('forAnalysis.rda')
# Throw out upper income countries
modelData = allData[allData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
##########################################################################################

# ##########################################################################################
# setwd(pathResults)
# load('LinvProfFE.rda'); dv='Investment Profile'
# # load('LpropRightsFE.rda'); dv='Property Rights'

# x=modResults[[1]] # model results for vars similar across dispute variables
# coefs=ivAll[[1]]; coefName=ivsName
# estimates=x$coefficient
# varcov=vcovHC(x, method="arellano", cluster="time")
# colnames(varcov)=rownames(varcov)=names(estimates)
# serrors=sqrt(diag( varcov ))
# # error = sqrt(sum(x$residuals^2)/x$df.residual)
# error = sqrt(mean(x$residuals^2))
# ##########################################################################################

##########################################################################################
setwd(pathResults)
load('LinvProfRE.rda'); dv='Investment Profile'
# load('LpropRightsRE.rda'); dv='Property Rights'
x=modResults[[1]] # model results for vars similar across dispute variables
coefs=ivAll[[1]]; coefName=ivsName
estimates=x@fixef
varcov=vcov(x); colnames(varcov)=rownames(varcov)=names(estimates)
serrors=sqrt(diag( varcov ))
RSS = sum(x@resid^2)
dfResid = x@dims['n']-length(x@fixef) - length(x@ranef) + 1
error = sqrt(RSS/dfResid)
##########################################################################################

##########################################################################################
varsTable=unlist(lapply(ivs, function(x) FUN=paste( c('lag_'), x, sep='' )))
lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
varsTableNames=unlist( lapply(ivsName, function(x) FUN= c(lagLabName(x))) )
varDef=cbind(varsTable, varsTableNames)
varDef=varDef[c(1,6:13),]
##########################################################################################

##########################################################################################
setwd(pathGraphics)
ggPlots=list()
tSeqCuts=c(5, 10, 2, 3, 1, 2, 4, .05, 3)

for(ii in 1:nrow(varDef)){
	toTest = varDef[ii,1]; toTestName=varDef[ii,1]
	testRange=quantile(modelData[,toTest], seq(0,1,.1), na.rm=T)
	# tRange=round(seq(testRange[1], testRange[length(testRange)], .05),3)
	tRange=round(seq(testRange[1], testRange[length(testRange)], tSeqCuts[ii]),3)
	tRange
	tRbreak=round(tRange,3)[seq(1,length(tRange),2)]
	tRbreak

	temp = ggsimplot(sims=10000, simData=na.omit(modelData[,coefs]), 
	  vars=coefs, vi=toTest, vRange=tRange, ostat=median,
	  betas=estimates, vcov=varcov, sigma=error, intercept=FALSE,
	  ylabel=dv, 
	  xlabel=toTestName,
	  specX=TRUE, ggxbreaks=tRbreak
	  # , specY=FALSE, ggybreaks=seq(10,30,5), ggylims=c(-1,1)
	  )
	ggPlots[[ii]]=temp
}

multiplot(ggPlots, 3)

# tikz("cinflationSIM.tex",width=4, height=2,standAlone=F)
# tikz("pch_cinflationSIM.tex",width=4, height=2,standAlone=F)
# tikz("pch_istability.tex",width=4, height=2,standAlone=F)
# temp
# dev.off()
#########################################################################################