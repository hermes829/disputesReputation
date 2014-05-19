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

# diffPlots=list()
preds=NULL
for(ii in 1:length(modResults)){

  x=modResults[[ii]] # model results for vars similar across dispute variables
  vars=ivAll[[ii]]; coefName=ivsName
  estimates=x$coefficient
  varcov=vcovHC(x, method="arellano", cluster="time")
  colnames(varcov)=rownames(varcov)=names(estimates)
  serrors=sqrt(diag( varcov ))
  error = sqrt(sum(x$residuals^2)/x$df.residual)
  ##########################################################################################

  ##########################################################################################
  varsTable=unlist(lapply(ivs, function(x) FUN=paste( c('lag_'), x, sep='' )))
  lagLabName=function(x){ paste(x, '$_{t-1}$', sep='') }
  varsTableNames=unlist( lapply(ivsName, function(x) FUN= c(lagLabName(x))) )
  varDef=cbind(varsTable, varsTableNames)
  varDef=varDef[c(ii,6:nrow(varDef)),]
  ##########################################################################################

  #########################################################################################
  # Distributions
  # Plotting density distributions
  vi=varDef[1,1]
  sims=10000
  simData=na.omit(modelData)
  vRange=quantile(simData[,vi],probs=seq(0,1,.1))[c(2,10)]
  intercept=FALSE
  specX=FALSE
  specY=TRUE
  ylabel="Inv. Profile$_{t}$"

  # Set up scenario
  scenCol = length(vars); scenRow = length(vRange)
  scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
  colnames(scenario) = c(vars)
  scenario[,vi] = vRange

  viPos = which(vi==vars)
  ovals = apply(simData[,vars[-viPos]], 2, median)
  scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
  if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
  vars2 = colnames(scenario)

  draws = mvrnorm(n = sims, estimates[vars2], varcov[vars2,vars2])
  modelPreds = draws %*% t(scenario)
  modelExp = apply(modelPreds, 2, function(x) FUN=rnorm(sims, x, error))

  modelPreds=modelPreds+mean(fixef(x))
  colnames(modelPreds)=c(paste0('lo',vi),paste0('hi',vi))
  preds=cbind(preds,modelPreds)
}

qSM=function(x){cbind(quantile(x, probs=c(0.25,0.75)))}
summPreds=apply(preds, 2, function(x) FUN= rbind(mean(x), qSM(x))  )
summPreds=data.frame(t(summPreds)); colnames(summPreds)=c('mean','lo','hi')
summPreds$scen=rep(LETTERS[1:2],nrow(summPreds)/2)
summPreds$disp=ivs[1:5]

temp=ggplot(summPreds, aes(x=factor(scen), y=mean,ymax=hi,ymin=lo,group=disp))
temp=temp+geom_linerange() + geom_point() + facet_wrap(~ disp)
temp

#   # One difference distribution
#   diff=data.frame(modelPreds[,1]-modelPreds[,2]); colnames(diff)='value';diff$X2=1
#   # diff=data.frame(modelExp[,1]-modelExp[,2]); colnames(diff)='value';diff$X2=1
#   diffList[[ii]]=diff

#   # Dist for each scenario
#   # diff=melt(modelPreds)[,-1]

#   ggMeans = ddply(diff, .(X2), summarise, sMean=mean(value))
#   ggDensity = ddply(diff, .(X2), .fun=function(x){
#     tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
#     q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
#     q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
#     data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

#   ggMeans$X2 = as.factor(ggMeans$X2)
#   ggDensity$X2 = as.factor(ggDensity$X2)

#   temp = ggplot()
#   temp = temp + geom_line(data=ggDensity, aes(x=x,y=y),size=1.5)
#   # temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,fill=X2,color=X2,linetype=X2),size=1.5)
#   temp = temp + geom_vline(data=ggMeans,
#     aes(xintercept=sMean),linetype='solid',size=1)
#     # aes(xintercept=sMean,color=X2),linetype='solid',size=1)
#   temp = temp + geom_ribbon(data=subset(ggDensity,q95),
#     aes(x=x,ymax=y),ymin=0,alpha=0.3)
#     # aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.3)
#   temp = temp + geom_ribbon(data=subset(ggDensity,q90),
#     aes(x=x,ymax=y),ymin=0,alpha=0.6)
#     # aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.6)
#   temp = temp + xlab(ylabel) + ylab('Density')
#   temp = temp + theme(legend.position='none', legend.title=element_blank(),
#     axis.ticks = element_blank(), 
#     panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
#     axis.title.x = element_text(vjust=-0.2), 
#     axis.title.y = element_text(vjust=0.2),
#     panel.border = element_blank(), axis.line = element_line(color='black'))
#   # temp
#   diffPlots[[ii]]=temp
# }

# multiplot(diffPlots, 2)
# #########################################################################################