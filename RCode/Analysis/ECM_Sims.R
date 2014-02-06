# Running simulations for sub effects

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
load('invProfRE.rda'); dv='Investment Profile'
# load('propRightsRE.rda'); dv='Property Rights'

# Load data
setwd(pathData)
load('forAnalysis.rda')
# Throw out upper income countries
modelData = allData[allData$upperincome==0,]
##########################################################################################

##########################################################################################
x=modResults[[1]] # model results for vars similar across dispute variables
coefs=ivAll[[1]]; coefName=ivAllNames[[1]]
estimates=x@fixef
varcov=vcov(x); colnames(varcov)=rownames(varcov)=names(estimates)
serrors=sqrt(diag( varcov ))
RSS = sum(x@resid^2)
dfResid = x@dims['n']-length(x@fixef) - length(x@ranef) + 1
error = sqrt(RSS/dfResid)
##########################################################################################

##########################################################################################
setwd(pathGraphics)
toTest = coefs[17]; toTestName=coefName[17]
testRange=quantile(modelData[,toTest], seq(0,1,.1), na.rm=T)
tRange=round(seq(testRange[1], testRange[length(testRange)], .5),3)
tRange
tRbreak=round(tRange,3)[seq(1,length(tRange),2)]
tRbreak

temp = ggsimplot(sims=10000, simData=na.omit(modelData[,coefs]), 
  vars=coefs, vi=toTest, vRange=tRange, ostat=median,
  betas=estimates, vcov=varcov, sigma=error, intercept=TRUE,
  ylabel=paste('\\%$\\Delta$ Change',dv,sep=' '), 
  xlabel=toTestName,
  specX=TRUE, ggxbreaks=tRbreak,
  specY=FALSE, ggybreaks=seq(10,30,5), ggylims=c(-1,1)
  )
temp
# tikz("cinflationSIM.tex",width=4, height=2,standAlone=F)
# tikz("pch_cinflationSIM.tex",width=4, height=2,standAlone=F)
tikz("pch_istability.tex",width=4, height=2,standAlone=F)
temp
dev.off()
#########################################################################################