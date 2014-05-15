# Running simulations for sub effects

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Load data
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

modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
##########################################################################################

##########################################################################################
setwd(pathResults)
load('LinvProfFE.rda'); dv='Investment Profile'

x=modResults[[1]] # model results for vars similar across dispute variables
coefs=ivAll[[1]]; coefName=ivsName
estimates=x$coefficient
varcov=vcovHC(x, method="arellano", cluster="time")
colnames(varcov)=rownames(varcov)=names(estimates)
serrors=sqrt(diag( varcov ))
# error = sqrt(sum(x$residuals^2)/x$df.residual)
error = 0
##########################################################################################

# ##########################################################################################
# setwd(pathResults)
# load('LinvProfRE.rda'); dv='Investment Profile'
# # load('LpropRightsRE.rda'); dv='Property Rights'
# x=modResults[[1]] # model results for vars similar across dispute variables
# coefs=ivAll[[1]] ; coefName=ivsName
# estimates=x@fixef
# varcov=vcov(x); colnames(varcov)=rownames(varcov)=names(estimates)
# serrors=sqrt(diag( varcov ))
# RSS = sum(x@resid^2)
# dfResid = x@dims['n']-length(x@fixef) - length(x@ranef) + 1
# error = sqrt(RSS/dfResid)
# error = 0
# ##########################################################################################

##########################################################################################
varsTable=unlist(lapply(ivs, function(x) FUN=paste( c('lag_'), x, sep='' )))
lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
varsTableNames=unlist( lapply(ivsName, function(x) FUN= c(lagLabName(x))) )
varDef=cbind(varsTable, varsTableNames)
varDef=varDef[c(1,6:nrow(varDef)),]
##########################################################################################

##########################################################################################
# setwd(pathGraphics)
# ggPlots=list()

# tSeqCuts=round(
# 	(apply(modelData[,varDef[,1]],2,function(x) FUN=max(x,na.rm=T))-
# 		apply(modelData[,varDef[,1]],2,function(x) FUN=min(x,na.rm=T)))/10, 1)

# for(ii in 1:nrow(varDef)){
# 	toTest = varDef[ii,1]; toTestName=varDef[ii,1]
# 	testRange=quantile(modelData[,toTest], seq(0,1,.1), na.rm=T)
# 	# tRange=round(seq(testRange[1], testRange[length(testRange)], .05),3)
# 	tRange=round(seq(testRange[1], testRange[length(testRange)], tSeqCuts[ii]),3)
# 	tRange
# 	tRbreak=round(tRange,3)[seq(1,length(tRange),2)]
# 	tRbreak

# 	temp = ggsimplot(sims=10000, simData=na.omit(modelData[,coefs]), 
# 	  vars=coefs, vi=toTest, vRange=tRange, ostat=median,
# 	  betas=estimates, vcov=varcov, sigma=error, intercept=FALSE,
# 	  ylabel=dv, 
# 	  xlabel=toTestName,
# 	  specX=TRUE, ggxbreaks=tRbreak
# 	  # , specY=FALSE, ggybreaks=seq(10,30,5), ggylims=c(-1,1)
# 	  )
# 	ggPlots[[ii]]=temp
# }

# multiplot(ggPlots, 3)

# tikz("cinflationSIM.tex",width=4, height=2,standAlone=F)
# tikz("pch_cinflationSIM.tex",width=4, height=2,standAlone=F)
# tikz("pch_istability.tex",width=4, height=2,standAlone=F)
# temp
# dev.off()
#########################################################################################

#########################################################################################
# Distributions
# Plotting density distributions
toTest=varDef[1,1]
vi=toTest
sims=10000
simData=na.omit(modelData)
vars=coefs
vRange=quantile(modelData[,vi],probs=seq(0,1,.1))[c(2,10)]
ostat=median
betas=estimates
vcov=varcov
sigma=0
intercept=FALSE
specX=FALSE
specY=TRUE
# ylabel="Inv. Profile$_{t}$"; ggybreaks=seq(0,12,2); ggylims=c(2,12)

# # Set up scenario
# scenCol = length(vars); scenRow = length(vRange)
# scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
# colnames(scenario) = c(vars)
# scenario[,vi] = vRange

# viPos = which(vi==vars)
# ovals = apply(simData[,vars[-viPos]], 2, ostat)
# scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
# if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
# vars2 = colnames(scenario)

# draws = mvrnorm(n = sims, betas[vars2], vcov[vars2,vars2])
# modelPreds = draws %*% t(scenario)
# modelExp = apply(modelPreds, 2, function(x) FUN=rnorm(sims, x, sigma))

# colnames(modelExp)=1:ncol(modelExp)
# modelExp2=melt(modelExp)[,-1]
# ggMeans = ddply(modelExp2, .(X2), summarise, sMean=mean(value))
# ggDensity = ddply(modelExp2, .(X2), .fun=function(x){
#   tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
#   q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
#   q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
#   data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

# ggMeans$X2 = as.factor(ggMeans$X2)
# ggDensity$X2 = as.factor(ggDensity$X2)

# temp = ggplot()
# temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,color=X2,linetype=X2),size=1.5)
# temp = temp + geom_vline(data=ggMeans,
#   aes(xintercept=sMean, color=X2),linetype='solid',size=1)
# temp = temp + geom_ribbon(data=subset(ggDensity,q95),
#   aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.3)
# temp = temp + geom_ribbon(data=subset(ggDensity,q90),
#   aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.6)
# temp = temp + xlab(ylabel) + ylab('Density')
# # temp = temp + scale_x_continuous(limits=ggylims, breaks=ggybreaks)
# temp = temp + theme(legend.position='none', legend.title=element_blank(),
#   axis.ticks = element_blank(), 
#   panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
#   axis.title.x = element_text(vjust=-0.2), 
#   axis.title.y = element_text(vjust=0.2),
#   panel.border = element_blank(), axis.line = element_line(color='black'))
# temp

# One difference distribution
modelExp2=data.frame(modelExp[,1]-modelExp[,2])
colnames(modelExp2)=c('value')
modelExp2$X2=1

ggMean=mean(modelExp2$value)
ggMeans = ddply(modelExp2, .(X2), summarise, sMean=mean(value))
ggDensity = ddply(modelExp2, .(X2), .fun=function(x){
  tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
  q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
  q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
  data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

ggMeans$X2 = as.factor(ggMeans$X2)
ggDensity$X2 = as.factor(ggDensity$X2)

temp = ggplot()
temp = temp + geom_line(data=ggDensity, aes(x=x,y=y),size=1.5)
temp = temp + geom_vline(data=ggMeans,
  aes(xintercept=sMean),linetype='solid',size=1)
temp = temp + geom_ribbon(data=subset(ggDensity,q95),
  aes(x=x,ymax=y),ymin=0,alpha=0.3)
temp = temp + geom_ribbon(data=subset(ggDensity,q90),
  aes(x=x,ymax=y),ymin=0,alpha=0.6)
temp = temp + xlab(ylabel) + ylab('Density')
temp = temp + theme(legend.position='none', legend.title=element_blank(),
  axis.ticks = element_blank(), 
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
  axis.title.x = element_text(vjust=-0.2), 
  axis.title.y = element_text(vjust=0.2),
  panel.border = element_blank(), axis.line = element_line(color='black'))
temp
#########################################################################################