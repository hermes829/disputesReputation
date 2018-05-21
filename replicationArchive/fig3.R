#######################################################################################
### source in the setup.R script using the path on your computer
source('~/Research/disputesReputation/replicationArchive/setup.R')

### Load data
load('analysisData.rda')
modelData=aData
#######################################################################################

####################################################################
# Running simulations for sub effects
load('LinvProfFEv2.rda'); dv='Investment Profile'; modNames=ivsName=ivAll

preds=NULL
dRanges=list()
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
  simData=na.omit(modelData[,c(varDef[,1],'invProf')])
  vRange=quantile(simData[,vi],probs=seq(0,1,.01))[c('0%','99%')]
  dRanges[[ii]] = vRange
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
  modelExp = modelPreds = draws %*% t(scenario)

  modelPreds=modelPreds+mean(fixef(x))
  colnames(modelPreds)=c(paste0('lo',vi),paste0('hi',vi))
  preds=cbind(preds,modelPreds)
}

qSM=function(x){cbind(quantile(x, probs=c(0.25,0.75)))}
summPreds=apply(preds, 2, function(x) FUN= rbind(mean(x), qSM(x))  )
summPreds=data.frame(t(summPreds)); colnames(summPreds)=c('mean','lo','hi')
summPreds$scen=rep(LETTERS[1:2],nrow(summPreds)/2)
summPreds$disp=rep(unlist(lapply(modNames, function(x) x[1])), each=2)

# Relabel facets
summPreds = summPreds[which(!summPreds$disp %in% c('lag1_mvs2_niDispB', 'lag1_mvs5_niDispB', 'lag1_niDispBC')),]
summPreds$disp[summPreds$disp=='lag1_mvs2_iDispB'] = 'ICSID (past two years)'
summPreds$disp[summPreds$disp=='lag1_mvs5_iDispB'] = 'ICSID (past five years)'
summPreds$disp[summPreds$disp=='lag1_iDispBC'] = 'Cumulative ICSID$_{t-1}$'
summPreds$disp = factor(summPreds$disp, levels=c(
  'ICSID (past two years)','ICSID (past five years)','Cumulative ICSID$_{t-1}$'
  ))

# plot
tmp=ggplot(summPreds, aes(x=factor(scen), y=mean,ymax=hi,ymin=lo,group=disp))
tmp=tmp+geom_linerange() + geom_point() + facet_wrap(~ disp,nrow=1)
tmp=tmp+ylab('Predicted Investment Profile Rating')
tmp=tmp+scale_x_discrete('',labels=c(
  'A'='Zero Disputes', 'B'='High Disputes'))  
tmp=tmp+scale_y_continuous(breaks=seq(0,12,2),labels=seq(0,12,2))
tmp = tmp + theme(
  axis.ticks=element_blank(), 
  panel.border=element_blank()
  )
ggsave(tmp, file='fig3.pdf',width=8,height=3.2)
###################################################################