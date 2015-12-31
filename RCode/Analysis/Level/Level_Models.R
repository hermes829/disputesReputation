####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData.rda'))
load(paste0(pathBin, 'textYr.rda'))
aData$storyCnt =  textYr$count[match(aData$year, textYr$year)]
aData$storyCntLog = log( aData$storyCnt + 1 )
#######################################################################################
# Setting up models
dv='invProf'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# disputes
dispVars =  c( 'iDispB', 'niDisp')
dispLabs = c('ICSID', 'Not ICSID' )
ivDisp=c( paste0('mvs2_',dispVars), paste0('mvs5_',dispVars), paste0(dispVars, 'C') )
ivDispName = c( lagLabName(dispLabs,T,2), lagLabName(dispLabs,T,5), paste0('Cumulative ', lagLabName(dispLabs,F) ))

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

# Setting up variables names for display
ivOtherName=c(
	'\\%$\\Delta$ GDP'
	, 'Ln(Pop.)'
	, 'Ln(Inflation)'	
	, 'Internal Stability'	
	,'Ratif. BITs'	
	,'Capital Openness'	
	,'Polity'
	)
ivsName=lapply(ivDispName, function(x) FUN= c(lagLabName(x,TRUE), lagLabName(ivOtherName)))
#######################################################################################

# #####################################################################################
# ### Create semi-balanced panel based off
# # All vars used in model
# temp=na.omit(aData[,c('cname','ccode','year', ivDisp, ivOther)])
# # Just ICRG
# # temp=na.omit(aData[,c('cname','ccode','year', 'Investment.Profile')])
# temp2=lapply(unique(temp$cname), function(x) FUN=nrow(temp[which(temp$cname %in% x), ]) )
# names(temp2)=unique(temp$cname); temp3=unlist(temp2)
# drop=names(temp3[temp3<quantile(temp3,probs=.25)])
# aData = aData[which(!aData$cname %in% drop),]
# #####################################################################################

#######################################################################################
# Running fixed effect models with plm
aData$year2 = aData$year
plmData=pdata.frame( aData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year', 'year2', 'storyCnt', 'storyCntLog') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Peak at dispute var results
do.call('rbind',lapply(modSumm, function(x) x[1,,drop=FALSE]))

sub = res = data.frame( do.call('rbind',lapply(modSumm, function(x) x[1,,drop=FALSE])) )

# Calculate sd effects
sdEffect = function(var, coef, data){
	beta = coef[var,1]
	sdX = sd(data[,var],na.rm=T)
	sdY = sd(data[,'invProf'],na.rm=T)
	eff = beta*(sdX/sdY)
	return(eff)
}

sub$eff = lapply(rownames(sub), function(x){ sdEffect(x,sub,aData) }) %>% unlist()
sub[order(sub$eff),c(1,3,5)]

# Saving results for further analysis
setwd(pathResults)
fileFE2 = strsplit(fileFE, '\\.') %>% unlist() %>% paste(.,collapse='v2.')
save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE2)
#######################################################################################

#######################################################################################
# Interaction terms
# mvs2_disp 
plmData$yr07 = ifelse(num(plmData$year2)>=2007,1,0)
plmData$dispYr07 = plmData$lag1_mvs2_iDispB * plmData$yr07

form=formula(paste0('invProf ~ lag1_mvs2_iDispB + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag1_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))

# mvs5_disp 
plmData$yr07 = ifelse(num(plmData$year2)>=2007,1,0)
plmData$dispYr07 = plmData$lag1_mvs5_iDispB * plmData$yr07

form=formula(paste0('invProf ~ lag1_mvs5_iDispB + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag1_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))

# dispC
plmData$yr07 = ifelse(num(plmData$year2)>=2007,1,0)
plmData$dispYr07 = plmData$lag1_iDispBC * plmData$yr07

form=formula(paste0('invProf ~ lag1_iDispBC + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag1_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))
#######################################################################################

yrD=data.frame(year=sort(unique(aData$year)))
yrD$cntr = 1:nrow(yrD)
yrD$cntr = ifelse(yrD$year>=2010,1,0) 
aData$cntr = yrD$cntr[match(aData$year, yrD$year)]

aData$dispVar = aData$lag1_iDispBC
aData$dispYr = aData$dispVar*aData$cntr
form=formula(paste0('invProf ~ dispVar + cntr + dispYr + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag1_polity + factor(ccode) -1'))

mod = lm(form, data=aData)
coeftest(mod)[1:3,]