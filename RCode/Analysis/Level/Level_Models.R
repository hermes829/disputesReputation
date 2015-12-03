####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

### Load data
load(paste0(pathBin, 'analysisData.rda'))
#######################################################################################
# Setting up models
dv='invProf'; dvName='Investment Profile'; fileFE='LinvProfFE.rda'

# Cumulative disputes
# ivDisp=c(
	# 'dispC','dispBC','iDispC','iDispBC', 'uDispC', 'uDispBC', 'iuDispC', 'iuDispBC', 
	# paste0(allCombos(c('oil', 'elec', 'oilElec'), c('', 'B')), 'C'),
# 	paste0(allCombos(c('i','u','iu'), allCombos( c('Oil','Elec','OilElec'), c('','B') ) ), 'C')
# )

# Two year moving sum of disputes
dispVars=c(
	'iDisp','iDispB', 'niDisp','niDispB', 
	allCombos(c('i','ni'), allCombos( c('Oil','Elec','OilElec'), c('','B') ) )		
	# 'disp','dispB','iDisp','iDispB', 'uDisp', 'uDispB', 'iuDisp', 'iuDispB', 
	# allCombos(c('i','u','iu'), allCombos( c('Oil','Elec','OilElec'), c('','B') ) )	
	)
ivDisp=paste0('mvs2_',dispVars)

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
ivDispName=c('All ICSID Disputes', 'ICSID Treaty-Based', 'ICSID-UNCTAD' )
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
plmData=pdata.frame( aData[,c(dv, unique(unlist(ivAll)), 'ccode', 'year') ], 
			index=c('ccode','year') )

modForm=lapply(ivAll, function(x) 
	FUN=as.formula( paste(dv, paste(x, collapse=' + '), sep=' ~ ') ))

modResults=lapply(modForm, function(x) FUN=plm(x, data=plmData, model='within') )
modSumm=lapply(modResults, function(x) FUN=coeftest(x, 
	vcov=vcovHC(x,method='arellano',cluster="group")))

# Peak at dispute var results
# do.call('rbind',lapply(modSumm, function(x) x[1,,drop=FALSE]))

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
save(modResults, modSumm, ivAll, dv, ivs, file=fileFE2)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=fileFE)
# save(modResults, modSumm, ivAll, dv, ivs, ivsName, dvName, file=paste0('B',fileFE))
#######################################################################################

# Aside with interaction of year and dispute
plmData$yr07=ifelse(num(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag1_mvs2_iDisp*plmData$yr07
form=formula(paste0('invProf ~ lag1_mvs2_iDisp + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag1_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))


plmData$yr07=ifelse(num(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag1_mvs2_iDispB*plmData$yr07
form=formula(paste0('invProf ~ lag1_mvs2_iDispB + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))


plmData$yr07=ifelse(num(plmData$year)>=2007,1,0)
plmData$dispYr07=plmData$lag1_mvs2_iuDisp*plmData$yr07
form=formula(paste0('invProf ~ lag1_mvs2_iuDisp + yr07 + dispYr07 + lag1_gdpGr + lag1_popLog + 
    lag1_inflLog + lag1_intConf + lag1_rbitNoDuplC + 
    lag1_kaopen + lag_polity'))
modRes=plm(form, data=plmData, model='within')
coeftest(modRes, vcov=vcovHC(modRes,method='arellano',cluster="group"))
