### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

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

anData = modelData[modelData$upperincome==0,]
anData = anData[anData$year>1986,]
#############################################################################

#############################################################################
# Subsetting to vars of interest
dv='Investment_Profile'

ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

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

vars=c(dv, ivDisp, ivOther)

# Setting up variables names for display
dvName='Investment Profile'
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'Unsettled ICSID', 'UNCTAD','ICSID-UNCTAD' )
ivOtherName=c(
	'\\%$\\Delta$ GDP'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
varNames=c(dvName, ivDispName, ivOtherName)

# Subsetting dataset
anData=anData[,c(vars,'year','ccode')]
#############################################################################

#############################################################################
# Running desc stats
summSM=function(var,group,data){
	slice=na.omit(data[,c(var,group)])
	N=length(unique(slice[,group])); n=nrow(slice)
	avg=mean(slice[,var]); std=sd(slice[,var])
	lo=min(slice[,var]); hi=max(slice[,var])
	c(N, n, avg, std, lo, hi)
}

results=matrix(unlist(lapply(vars, function(x) FUN=summSM(x, 'ccode', anData))),
	ncol=6,byrow=TRUE,
	dimnames=c(list(varNames, c('N','n','Mean','Std. Dev.', 'Min','Max')))
	)
round(results,2)

setwd(pathPaper)
# print.xtable(xtable(round(results,2), align='llccccc', 
# 	digits=c(0,0,0,2,2,2,2),
# 	# caption=captionTable
# 	), include.rownames=TRUE,
# 	# sanitize.text.function = function(x) x,
# 	# sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
# 	sanitize.text.function = identity,		
# 	hline.after=c(0,0,nrow(results),nrow(results)), 	
# 	size="footnotesize",	
# 	file='descTable.tex'
# 	)
#############################################################################

#############################################################################
# Geographic distribution of BITs
mapData <- cshp(date = as.Date("2011-12-30"), useGW=TRUE)

mapData$CNTRY_NAME=as.character(mapData$CNTRY_NAME)
mapData$CNTRY_NAME[mapData$CNTRY_NAME=='Congo, DRC']="Congo, Democratic Republic of"
mData=data.frame( cbind(
	cntry=as.character(mapData$CNTRY_NAME),
	oid=mapData$GWCODE,
	ccode=panel$ccode[match(mapData$CNTRY_NAME,panel$CNTRY_NAME)] ) )

slice=modelData[modelData$year==max(modelData$year),]
mData$bits=slice$ratifiedbits[match(mData$ccode,slice$ccode)]
mData$disputes=slice$cum_alltreaty[match(mData$ccode,slice$ccode)]

# Creating map
gpclibPermit()
require(RColorBrewer); require(grid)
mColors <- brewer.pal(9, 'Greys')
ggMap = fortify(mapData, region="GWCODE")
ggMap=merge(ggMap, mData, by.x='id',by.y='oid',all.x=T)

temp <- ggplot(ggMap, aes(long,lat,group=group,fill=bits))
# temp <- ggplot(ggMap, aes(long,lat,group=group,fill=disputes))
temp <- temp + geom_polygon(colour='black',lwd=1e-2) 
temp <- temp + scale_fill_gradient(
	limits=c(0,max(mData$bits,na.rm=T)),
	# limits=c(0,max(mData$disputes,na.rm=T)),
	low=mColors[1],high=mColors[9], 
	space = "Lab", na.value = "grey50", guide = "colourbar")
temp <- temp + labs(x='',y='',
	title='BITs Ratified by 2011')
	# title='Disputes by 2011')
temp <- temp + theme(
	axis.text=element_blank(), axis.ticks = element_blank(),
	panel.border = element_blank(), 
	panel.grid.major=element_blank(), panel.grid.minor=element_blank(),     
	legend.position="top", legend.title=element_blank(),
	panel.background = element_rect(fill="white")
	,legend.key.width=unit(3,'cm')
	)
temp
#############################################################################