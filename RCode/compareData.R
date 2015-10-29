# cleanup
rm(list=ls())

# source lag data script
source("~/Research/RemmerProjects/disputesReputation/RCode/tscsHelpers.R")

pathNew = '~/Dropbox/Research/RemmerProjects/disputesReputation/Data/'
path = '~/Dropbox/Research/RemmerProjects/disputesReputation/Data/Old/'
list.files(path)

# Libs
library(foreign)
library(magrittr)
library(panelAR)
library(lmtest)

# helpful functions
lagLab = function(x){ paste0('lag_',x) }
pchLab = function(x){ paste0('pch_',x) }
num = function(x){ as.numeric(as.character(x)) }

# Load my version of data
mnew = paste0(pathNew, 'analysisData.dta') %>% read.dta( . ) 
mold = paste0(path, 'oldData.dta') %>% read.dta( . )
rold = paste0(path, 'Components/Investment Profile Data.13.dta') %>% read.dta( . )

length( setdiff(mold$cyear, rold$cyear) ) ; length( setdiff(rold$cyear, mold$cyear) )

# Regression model obs diffs
lagVars = c('pch_cum_kicsidcase', 'cum_kicsidcase', 'pch_LNgdp', 'LNgdp', 'lncinflation', 
	'pch_lncinflation', 'pch_LNpopulation', 'LNpopulation', 'pch_Internal_Conflict', 
	'Internal_Conflict', 'pch_ratifiedbits', 'ratifiedbits', 'pch_kaopen', 'kaopen', 
	'pch_polity', 'polity', 'Investment_Profile')

mold$cyear = num( mold$cyear ); mold$ccode = num( mold$ccode )
rold$cyear = num( rold$cyear ); rold$ccode = num( rold$ccode )

mold = lagData(mold, 'cyear', 'ccode', lagVars, lag=1)
rold = lagData(rold, 'cyear', 'ccode', lagVars, lag=1)

mold = mold[,c('pch_Investment_Profile', 'ccode', 'cname', 'year', 'coecd', 'cyear', paste0('lag1_',lagVars))] %>% na.omit()
rold = rold[,c('pch_Investment_Profile', 'ccode', 'cname', 'year', 'coecd', 'cyear', paste0('lag1_',lagVars))] %>% na.omit()

length( setdiff(mold$cyear, rold$cyear) ) ; length( setdiff(rold$cyear, mold$cyear) )

# Subset both to relevant year
mold = mold[mold$year>1986,]
rold = rold[rold$year>1986,]

# remove OECD countries
mold = mold[mold$coecd==0,]
rold = rold[rold$coecd==0,]

length( setdiff(mold$cyear, rold$cyear) ) ; length( setdiff(rold$cyear, mold$cyear) )

dim(mold); dim(rold)


diffs = setdiff(rold$cyear, mold$cyear)
diffs = cbind(num(substr(diffs, 1, nchar(diffs)-4)), num(substr(diffs, nchar(diffs)-3, nchar(diffs)))) %>% data.frame()
diffs = diffs[order(diffs$X2),]
names(diffs) = c('ccode', 'year')
diffs$cname = mnew$cname[match(diffs$ccode, mnew$ccode)]




# See differences with new
mnew = mnew[mnew$year>1986,]

diffs = setdiff(mnew$cyear, mold$cyear)
diffs = cbind(num(substr(diffs, 1, nchar(diffs)-4)), num(substr(diffs, nchar(diffs)-3, nchar(diffs)))) %>% data.frame()
diffs = diffs[order(diffs$X2),]
names(diffs) = c('ccode', 'year')
diffs$cname = mnew$cname[match(diffs$ccode, mnew$ccode)]

# # Change name of lagged cum icsid treaty cases in rold
# names(rold)[names(rold) == 'lagcum_kicsidcase'] = 'lag_cum_kicsidcase'

# # Vars in ECM model
# ids = c(
# 	'country', 'year', 'ccode', 'cyear'
# 	)

# # Choosing DV
# dv='pch_Investment_Profile'
# ivDV=paste('lag',substr(dv, 4, nchar(dv)),sep='')

# # Cum. Dispute vars
# ivDisp=c('cum_kicsidcase' )

# # Other covariates
# ivOther=c(
# 	'LNgdp'
# 	,'LNpopulation'
# 	,'lncinflation'
# 	, 'Internal_Conflict'	
# 	,'ratifiedbits'	
# 	,'kaopen'	
# 	,'polity'
# 	)

# # Untrans IVs
# ivs=c(ivDV, ivDisp, ivOther)
# ivAll=unlist( lapply(ivDisp, function(x) 
# 	FUN= c(ivDV ,lagLab(x), lagLab(ivOther), pchLab(x), pchLab(ivOther)) ) )

# # Vars to check
# vars = c(ids, dv, ivAll)

# # Make sure in both datasets
# setdiff(vars, names(rold))
# setdiff(vars, names(mold))

# # Different country-year obs
# setdiff(rold$cyear, mold$cyear)
# setdiff(mold$cyear, rold$cyear)

# # na omit
# mold = na.omit( mold[,vars] )
# rold = na.omit( rold[,vars] )

# # Check dims and compare what's missing
# dim(mold)
# dim(rold)

# # Run model
# modForm=as.formula( 
# 		paste(paste(dv, paste(ivAll, collapse=' + '), sep=' ~ '), 
# 			'+ factor(ccode) + factor(year) - 1', collapse='') )

# modResults=panelAR(modForm, mold, 'ccode', 'year', 
# 	autoCorr = c("psar1"), panelCorrMethod="pcse",rhotype='breg', 
# 	complete.case=FALSE, rho.na.rm=TRUE )
# names(modResults)
# length(modResults$residuals)