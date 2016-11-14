####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/disputesReputation/RCode/setup.R') }
####

# Cross-validation

### Load data
# load(paste0(pathBin, 'analysisData.rda'))
load(paste0(pathBin, 'analysisData_upperincIncluded.rda'))

###############################################################################
# Model setup
# Set up models
aData$fdiLog = log(aData$fdi + abs(min(aData$fdi, na.rm=TRUE)) + 1)
aData$fdiLog2 = logNeg(aData$fdi)
dv='fdiLog2'

# dispute var
# dispVars = c( 'iDispB', 'iDispB' )
# ivDisp=paste0(dispVars, 'C') # Cumulative
# ivDisp=paste0('mvs2_',dispVars) # Two year moving sum of disputes
ivDisp = c('iDispBC', 'mvs2_iDispB', 'mvs5_iDispB')

# Other covariates
ivOther=c(
	'gdpGr', 'gdpCapLog', 'popLog','inflLog',
	'intConf', 'extConf',
	'rbitNoDuplC', 'kaopen', 'polity', 'propRights'
	)

# Untrans IVs
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1), 'globSumRFDI' ) )

modForm=lapply(ivAll, function(x){
	as.formula( 
		paste(dv, paste(x, collapse=' + '), sep=' ~ ')
		)
	})
###############################################################################

###############################################################################
# Run yearly models
yrs=1994:2014
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
			if( sum( grepl('Disp', rownames(modSumm[[x]])) ) == 0 ){
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
ivDispName=c('Cumulative', '2 year', '5 year' )
# ivDispName = c('ICSID','ICSID')
VARSname=lagLabName(ivDispName,FALSE)

tmp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', facetDim=c(1,3), 
  facetBreaks=yrs,
  facetLabs=yrs,  
  allBlack=FALSE
  )
tmp=tmp + ylab('$\\beta$ for Dispute Variables') + scale_x_discrete(breaks=seq(1990,2014,4),labels=seq(1990,2014,4))
tmp=tmp + theme(axis.title.y=element_text(vjust=1))
tmp = tmp + theme_bw()
tmp = tmp + theme(
	legend.position='none',
	panel.border=element_blank(),
	axis.ticks=element_blank(),
	axis.text.x=element_text(angle=45)
	)
tmp
# tmp=tmp+scale_color_manual(values=brewer.pal(9,'Greys')[c(5,9,7)])
setwd(pathGraphics)
# tikz(file='crossValLevel_FDI.tex',width=8,height=3.5,standAlone=F)
# tmp
# dev.off()
###############################################################################