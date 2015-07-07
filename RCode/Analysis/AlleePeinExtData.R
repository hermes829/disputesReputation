###
# Extension of A&P Data to 2011
###

# Setting workspace
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

# Load remmer/minhas data
load(paste0(pathData, '/modelData.rda'))
names(modelData)[which(names(modelData) == 'lagcbdcrisis')] = 'lag_cbdcrisis'

# Add measure for exchange rate volatility
xrate = WDI(country='all', indicator='PA.NUS.FCRF', start=1985, end=2013)
meanNA = function(x){ mean(x, na.rm=TRUE) }
xrate$xRateMean = with(xrate, ave(PA.NUS.FCRF, country, FUN = meanNA))
xrate$xRateVol = abs( xrate$PA.NUS.FCRF - xrate$xRateMean )

# Create matching countrycodes using panel
xrate$cname = toupper(countrycode(xrate$iso2c, 'iso2c', 'country.name'))
xrate = xrate[!is.na(xrate$cname),]
xrate$ccode = panel$ccode[match(xrate$cname, panel$cname)]
xrate = xrate[!is.na(xrate$ccode),]

# Merge into modelData
xrate$cyear = paste(xrate$ccode, xrate$year, sep='_')
modelData$cyear = paste(modelData$ccode, modelData$year, sep='_')
modelData$xRateVol = xrate$xRateVol[match(modelData$cyear, xrate$cyear)]

# Add lagged version
xrate$yearL = xrate$year + 1
xrate$cyearL = paste(xrate$ccode, xrate$yearL, sep='_')
modelData$lag_xRateVol = xrate$xRateVol[match(modelData$cyear, xrate$cyearL)]

# Add global fdi varibale
sumNA = function(x){ sum(x, na.rm=TRUE) }
modelData$globFDI = with(modelData, ave(r_fdi, year, FUN=sumNA	))
modelData$globLnFDI = with(modelData, ave(LNr_fdi, year, FUN=sumNA))

# Vars from modelData
# FDI
dv = c('r_fdi', 'LNr_fdi')
# IVs
ctrls = c(
	'lag_ratifiedbits',
	'lag_cbdcrisis',
	'lag_domestic9',
	'lag_External_Conflict',
	'lag_polity',
	'lag_Property_Rights',
	'lag_LNpopulation',
	'lag_LNr_gdpCAP',
	'pch_r_gdp',
	'lag_xRateVol',	
	'lag_kaopen',
	'globFDI'
	)

# Cumulative disputes
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

# Two year moving sum of disputes
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'alltreaty')
ivDisp2=paste0('mvs2_',dispVars)
ivDisp5=paste0('mvs5_',dispVars)

# Subset modeldata
modelData = modelData[,c('country','ccode','year',dv,ctrls,ivDisp,ivDisp2,ivDisp5)]

# Save
save(dv, ctrls, ivDisp, dispVars, ivDisp2, ivDisp5, modelData, file=paste0(pathData, '/apExt.rda'))