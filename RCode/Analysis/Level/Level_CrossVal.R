# Cross-validation

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('modelData.rda')

###############################################################################
# Model setup
# Set up models
dv='Investment_Profile'; dvName='Investment Profile'

# Cumulative disputes
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cum_alltreaty' )

# Two year moving sum of disputes
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'alltreaty')
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
# Run yearly models
yrs=sort(unique(modelData$year))
coefCross=NULL
for(ii in 1:length(yrs)){

	slice=modelData[which(modelData$year %in% yrs[ii]), ]
	print(paste0('cross ',yrs[ii], ' has ', nrow(slice), ' obs from ',
		length(unique(slice$ccode)), ' countries'))	

	modResults=lapply(modForm, function(x)
		FUN=lm(x, data=slice) )	

	modSumm=lapply(modResults, function(x)
		FUN=coeftest(x))		

	dispSumm=do.call(rbind, lapply(modSumm,function(x)FUN=x[2,,drop=FALSE]))

	coefCross=rbind(coefCross, cbind(dispSumm,cross=yrs[ii]))	
}
coefCross=coefCross[which(!rownames(coefCross) %in% 'lag_pch_gdp'),]
###############################################################################

###############################################################################
# Plotting
VARS=unique(rownames(coefCross))
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'ICSID-UNCTAD' )
VARSname=lagLabName(ivDispName)

tmp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(2,2), 
  facetBreaks=seq(1987,2011,3),
  facetLabs=seq(1987,2011,3),
  allBlack=FALSE
  )
setwd(pathGraphics)
tikz(file='crossValLevel.tex',width=8,height=6,standAlone=F)
tmp
dev.off()
###############################################################################

###############################################################################
# Substantive effect
sims=1000
yrs=1999:2011
modelYrPreds=NULL
for(Year in yrs){
	slice=modelData[which(modelData$year %in% Year), ]
	yrMod=lapply(modForm, function(x) lm(x, data=slice) )

	# scenario
	# Control vars same for each model
	vars=names(coef(yrMod[[1]]))[c(-1,-2)]
	means=apply(slice[,vars], 2, function(x) mean(x, na.rm=TRUE))
	dispVar=lapply(yrMod, function(x) names(coef(x))[2] )
	minMaxDisp=lapply(dispVar, function(x) quantile(slice[,x], probs=c(0,1), na.rm=TRUE) )
	scen=lapply(minMaxDisp, function(x) rbind( c(1, x[1], means), c(1, x[2], means) ) )
	
	# sims
	draws=lapply(yrMod, function(x) mvrnorm(sims, coef(x), vcov(x)) )
	modelPreds=lapply(1:length(scen), function(x){ 
		preds=data.frame( draws[[x]] %*% t(scen[[x]]) )
		names(preds)=c('lodisp', 'hidisp')
		preds$varYr=paste(dispVar[[x]], Year, sep='__')
		preds } )
	modelPreds=do.call('rbind', modelPreds)

	# Aggregate results
	modelYrPreds=rbind(modelYrPreds, modelPreds)
}

# Aggregate
qSM=function(x){quantile(x, probs=c(0.025,0.975))}
aggData=lapply(unique(modelYrPreds$varYr), function(x){ modelYrPreds[modelYrPreds$varYr==x,] })
aggStats=do.call('rbind', lapply(aggData, function(x){
	stats=t(apply(x[,1:2], 2, function(y){ c(mean(y), qSM(y)) }))
	colnames(stats)=c('mu','qlo','qhi')
	rownames(stats)=NULL
	cbind(unique(x[,3]), c('Low', 'High'), stats) }) )
aggStats=data.frame(aggStats); names(aggStats)[1:2]=c('varYr', 'Scenario')
aggStats$mu=numSM(aggStats$mu); aggStats$qlo=numSM(aggStats$qlo); aggStats$qhi=numSM(aggStats$qhi)
aggStats$dispVar=unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[1]))
aggStats$Year=numSM(unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[2])))

# Plot labeling
aggStats$Scenario=mapVar(aggStats$Scenario, c('Low','High'), paste0(c('Zero ', 'High '), 'Disputes'))
aggStats$dispVar=mapVar(aggStats$dispVar, paste0('lag_', ivDisp), ivDispName )
aggStats$Year[aggStats$Scenario=='Zero Disputes']=aggStats$Year[aggStats$Scenario=='Zero Disputes']-.12

tmp=ggplot(aggStats, aes(x=Year, color=Scenario)) + scale_color_grey(start=.6, end=0)
tmp=tmp + geom_linerange(aes(ymax=qhi,ymin=qlo), lwd=.75) + geom_point(aes(y=mu), cex=2.5) 
tmp=tmp + scale_x_continuous('',breaks=seq(1999, 2011, 2)) + ylab('Predicted Investment Profile Rating')
tmp=tmp + facet_wrap(~dispVar) 
tmp=tmp + theme(
	panel.grid=element_blank(),
	axis.text.x=element_text(angle=45,hjust=1), 
	legend.position='top', legend.title=element_blank())
tmp
setwd(pathGraphics)
tikz(file='crossValSim.tex',width=8,height=6,standAlone=F)
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