# Running simulations for sub effects

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('modelData.rda')

####################################################################
setwd(pathResults)
load('LinvProfFE.rda'); dv='Investment Profile'; modNames=ivsName

preds=NULL
for(ii in 1:length(modResults)){

  x=modResults[[ii]] # model results for vars similar across dispute variables
  vars=ivAll[[ii]]; coefName=ivsName
  estimates=x$coefficient
  varcov=vcovHC(x, method="arellano", cluster="time")
  colnames(varcov)=rownames(varcov)=names(estimates)
  serrors=sqrt(diag( varcov ))
  error = sqrt(sum(x$residuals^2)/x$df.residual)
  ####################################################################

  ####################################################################
  varDef= cbind(ivAll[[ii]], ivsName[[ii]])
  ####################################################################

  ###################################################################
  # Distributions
  # Plotting density distributions
  vi=varDef[1,1]
  sims=10000
  simData=na.omit(modelData[,c(varDef[,1],'Investment_Profile')])
  vRange=quantile(simData[,vi],probs=seq(0,1,.01))[c('0%','99%')]
  intercept=FALSE
  specX=FALSE
  specY=TRUE
  ylabel="Inv. Profile$_{t}$"

  #########
  # Set up disputes scenario
  scenCol = length(vars); scenRow = length(vRange)
  scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
  colnames(scenario) = c(vars)
  scenario[,vi] = vRange

  viPos = which(vi==vars)
  ovals = apply(simData[,vars[-viPos]], 2, median)
  scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
  if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
  vars2 = colnames(scenario)
  #########  

  ##########
  # Draw pred values from mvnorm
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
summPreds$disp=rep(unlist(lapply(modNames, function(x) x[1])), each=2)

tmp=ggplot(summPreds, aes(x=factor(scen), y=mean,ymax=hi,ymin=lo,group=disp))
tmp=tmp+geom_linerange() + geom_point() + facet_wrap(~ disp)
tmp=tmp+ylab('Predicted Investment Profile Rating')
tmp=tmp+scale_x_discrete('',labels=c(
  'A'='Zero Disputes', 'B'='High Disputes'))  
tmp=tmp+scale_y_continuous(breaks=c(0,4,8,12),labels=c(0,4,8,12))
tmp = tmp + theme(
  axis.ticks=element_blank(), panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(), axis.title.y=element_text(vjust=1)
  )
tmp
setwd(pathGraphics)
tikz(file='simResults.tex',width=8,height=6,standAlone=F)
tmp
dev.off()
###################################################################