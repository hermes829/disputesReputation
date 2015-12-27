####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

# Cross-validation

### Load data
load(paste0(pathBin, 'analysisData.rda'))

# Bring in upperincome data
load(paste0(pathData,'/Old/modelData.rda'))
ui = modelData[,c('cname', 'upperincome', 'oecd')] %>% unique()
toDrop = setdiff(aData$cname, modelData$cname)
aData = aData[which(!aData$cname %in% toDrop),]

# Subset to post 1987
aData = aData[aData$year>=1987,]

###############################################################################
# Model setup
# Set up models
dv='invProf'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# Cumulative disputes
ivDisp=c( 'iDispC','iDispBC', 'iuDispC', 'niDispC' )

# # Two year moving sum of disputes
# dispVars=c('iDisp', 'iDispB', 'iuDisp',
# 	allCombos(c('i','u','iu'), allCombos( c('Oil','Elec','OilElec'), c('','B') ) )	
# 	)
# ivDisp=paste0('mvs2_',dispVars)

# Other covariates
ivOther=c(
	'gdpGr'
	,'popLog'
	,'inflLog'
	, 'intConf'	
	,'rbitNoDuplC'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1) ) )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))
###############################################################################

###############################################################################
# Run yearly models
yrs=1995:2014

# Add yearly breaks
# aData$yrBrk = cut(aData$year, 7)
# yrs = unique(aData$yrBrk)

coefCross=NULL
for(ii in 1:length(yrs)){

	# By year
	slice=aData[which(aData$year %in% yrs[ii]), ]
	modResults=lapply(modForm, function(x) FUN=lm(x, data=slice) )	
	modSumm=lapply(modResults, function(x) FUN=coeftest(x))			
	
	# # By buckets of years
	# slice=aData[which(aData$yrBrk %in% yrs[ii]), ]
	# plmData=pdata.frame( slice[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
	# 			index=c('ccode','year') )
	# modForm=lapply(ivAll, function(x) 
	# 	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))
	# modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
	# modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	# 	vcov=vcovHC(x,method='arellano',cluster="group")))

	# Combining results
	dispSumm=do.call(rbind, lapply(modSumm,function(x)FUN=x[2,,drop=FALSE]))
	dispSumm=dispSumm[which(rownames(dispSumm) %in% lagLab(ivDisp, 1)), ]
	coefCross=rbind(coefCross, cbind(dispSumm,cross=yrs[ii]))	
}
###############################################################################

###############################################################################
# Plotting
VARS=unique(rownames(coefCross))
ivDispName=c('All ICSID', 'ICSID Treaty-Based', 'ICSID-UNCTAD', 'Not ICSID' )
VARSname=lagLabName(ivDispName,FALSE)

tmp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(2,2), 
  # facetBreaks=seq(yrs[1],2014,3),
  # facetLabs=seq(yrs[1],2014,3),
  facetBreaks=yrs,
  facetLabs=yrs,  
  allBlack=FALSE
  )
tmp=tmp + ylab('$\\beta$ for Dispute Variables')
tmp=tmp + theme(axis.title.y=element_text(vjust=1))
# tmp=tmp+scale_color_manual(values=brewer.pal(9,'Greys')[c(5,9,7)])
# setwd(pathGraphics)
# tikz(file='crossValLevel.tex',width=8,height=6,standAlone=F)
tmp
# dev.off()
###############################################################################

###############################################################################
# Substantive effect
sims=1000
yrs=1995:2014
modelYrPreds=NULL
for(Year in yrs){
	slice=aData[which(aData$year %in% Year), ]
	yrMod=lapply(modForm, function(x) lm(x, data=slice) )

	# scenario
	# Control vars same for each model
	vars=names(coef(yrMod[[1]]))[c(-1,-2)]
	means=apply(slice[,vars], 2, function(x) mean(x, na.rm=TRUE))
	dispVar=lapply(yrMod, function(x) names(coef(x))[2] )
	minMaxDisp=lapply(dispVar, function(x) quantile(slice[,x], probs=c(0,.99), na.rm=TRUE) )
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
aggStats$mu=num(aggStats$mu); aggStats$qlo=num(aggStats$qlo); aggStats$qhi=num(aggStats$qhi)
aggStats$dispVar=unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[1]))
aggStats$Year=num(unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[2])))

# Plot labeling
aggStats$Scenario=mapVar(aggStats$Scenario, c('Low','High'), paste0(c('Zero ', 'High '), 'Disputes $\\; \\; \\;$'))
aggStats$dispVar=mapVar(aggStats$dispVar, paste0('lag_', ivDisp), lagLabName(ivDispName,TRUE) )
# aggStats$Year[aggStats$Scenario=='Zero Disputes']=aggStats$Year[aggStats$Scenario=='Zero Disputes \;\;\;']-.12

tmp=ggplot(aggStats, aes(x=Year, color=Scenario)) + scale_color_grey(start=.6, end=0)
tmp=tmp + geom_linerange(aes(ymax=qhi,ymin=qlo), lwd=.75) + geom_point(aes(y=mu,shape=Scenario), cex=2.5) 
tmp=tmp + scale_x_continuous('',breaks=seq(yrs[1], 2011, 3)) + ylab('Predicted Investment Profile Rating')
tmp=tmp + facet_wrap(~dispVar) 
tmp=tmp + theme(
	panel.grid=element_blank(),
	axis.text.x=element_text(angle=45,hjust=1), 
	axis.title.y=element_text(vjust=1),
	axis.ticks=element_blank(),
	legend.position='top', legend.title=element_blank())
setwd(pathGraphics)
tikz(file='crossValSim.tex',width=8,height=6,standAlone=F)
tmp
dev.off()
###############################################################################