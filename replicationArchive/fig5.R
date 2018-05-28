#######################################################################################
### source in the setup.R script using the path on your computer
source('setup.R')

### Load data
load('analysisData.rda')

# Bring in upperincome data
load('upperIncomeData.rda')
ui = modelData[,c('cname', 'upperincome', 'oecd')] %>% unique()
toDrop = setdiff(aData$cname, modelData$cname)
aData = aData[which(!aData$cname %in% toDrop),]

# Subset to post 1987
aData = aData[aData$year>=1987,]
#######################################################################################

###############################################################################
# Model setup
# Set up models
dv='invProf'; dvName='Investment Profile'; fileFE='LinvProfFE.rda' ; yrRange = 1994:2014 ; crossValFileName = 'crossValLevel.tex'; ylabBrk = seq(1990,2014,4)

# dispute var
ivDisp = c(
	'mvs2_iDispB', 'mvs5_iDispB','iDispBC'
	)

# Other covariates
ivOther=c(
	'gdpGr'
	,'gdpCapLog'
	,'popLog'
	,'inflLog'
	, 'intConf'	
	,'extConf'
	,'rbitNoDuplC'	
	,'kaopen'	
	,'polity'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1) ) )

##########
# Sample assessment
dvSamp = aData[,c('ccode','year',dv, unique(unlist(ivAll)))] %>% na.omit()
length(unique(dvSamp$ccode))
summary(dvSamp$year)
##########

modForm=lapply(ivAll, function(x){
	as.formula( 
		paste(dv, paste(x, collapse=' + '), sep=' ~ ')
		)
	})
###############################################################################

###############################################################################
# Substantive effect
sims=1000
yrs=1994:2014
modelYrPreds=NULL
for(Year in yrs){
	slice=aData[which(aData$year %in% Year), ]
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
aggStats$mu=num(aggStats$mu); aggStats$qlo=num(aggStats$qlo); aggStats$qhi=num(aggStats$qhi)
aggStats$dispVar=unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[1]))
aggStats$Year=num(unlist(lapply(strsplit(char(aggStats$varYr), '__'), function(x) x[2])))

# Plot labeling
aggStats$Scenario=mapVar(aggStats$Scenario, c('Low','High'), paste0(c('Zero ', 'High '), 'Disputes $\\; \\; \\;$'))
VARS=c("lag1_mvs2_iDispB", "lag1_mvs5_iDispB", "lag1_iDispBC")
VARSname=c(
	'ICSID (past two years)','ICSID (past five years)','Cumulative ICSID$_{t-1}$'
	)
aggStats$dispVar=mapVar(aggStats$dispVar, VARS, VARSname )

tmp=ggplot(aggStats, aes(x=Year, color=Scenario)) + scale_color_grey(start=.6, end=0)
tmp=tmp + geom_linerange(aes(ymax=qhi,ymin=qlo), lwd=.75) + geom_point(aes(y=mu,shape=Scenario), cex=2.5) 
tmp=tmp + scale_x_continuous('',breaks=seq(yrs[1], 2014, 4)) + ylab('Predicted Investment Profile Rating')
tmp=tmp + facet_wrap(~dispVar) 
tmp=tmp + theme(
	legend.position='top',
	legend.title=element_blank(),
  	legend.key = element_blank(),
	panel.border=element_blank(),
	axis.ticks=element_blank(),
	axis.text.x=element_text(angle=45)	
	)
ggsave(tmp, file='fig5.pdf', width=8,height=4.5)
###############################################################################