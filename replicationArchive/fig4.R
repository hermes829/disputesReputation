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
# Cross-validation
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
# Run yearly models
yrs=yrRange
coefCross=NULL
for(ii in 1:length(yrs)){

	# By year
	slice=aData[which(aData$year == yrs[ii]), ]
	regData=slice[,c('ccode','year',dv, unique(unlist(ivAll)))] %>% na.omit()
	mult = lapply(ivAll, function(x){ sd(regData[,x[1]])/sd(regData[,dv]) })
	modResults=lapply(modForm, function(x) FUN=lm(x, data=regData) )	
	modSumm=lapply(modResults, function(x) FUN=coeftest(x))

	# Combining results
	dispSumm=do.call(rbind, 
		lapply(1:length(modSumm),function(x){
			if( sum( grepl(c('ispB'), rownames(modSumm[[x]])) ) == 0 ){
				missVar = modResults[[x]] $model %>% names() %>% .[2]
				empty = matrix(NA, nrow=1, ncol=4, dimnames=list(missVar, NULL))
				return(empty) }
			betaStats=modSumm[[x]][2,,drop=FALSE]
			betaStats[1:2]=betaStats[1:2]*mult[[x]]
			return(betaStats) } ) )
	dispSumm=dispSumm[which(rownames(dispSumm) %in% lagLab(ivDisp, 1)), ]
	coefCross=rbind(coefCross, cbind(dispSumm,cross=yrs[ii]))	
}
###############################################################################

###############################################################################
# Plotting
VARS=unique(rownames(coefCross))
VARSname=c(
	'ICSID (past two years)','ICSID (past five years)','Cumulative ICSID$_{t-1}$'
	)

tmp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(2,3), 
  facetBreaks=yrs,
  facetLabs=yrs,  
  allBlack=FALSE
  )
tmp=tmp + ylab('$\\beta$ for Dispute Variables') + scale_x_discrete(breaks=ylabBrk,labels=ylabBrk)
tmp=tmp + theme(axis.title.y=element_text(vjust=1))
tmp = tmp + theme_bw()
tmp = tmp + theme(
	legend.position='none',
	panel.border=element_blank(),
	axis.ticks=element_blank(),
	axis.text.x=element_text(angle=45)
	)
ggsave(tmp, file='fig4.pdf',width=8,height=3.5)
###############################################################################