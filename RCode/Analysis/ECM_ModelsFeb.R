### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')

### Throw out upper income countries
modelData = allData[allData$upperincome==0,]

#### Most recent code from Remmer
	# xtpcse pch_Investment_Profile
		# lag_Investment_Profile pch_cum_icsidtreaty L.cum_icsidtreaty
		# pch_ratifiedbits L.ratifiedbits pch_LNgdp lag_LNgdp
		# pch_LNpopulation lag_LNpopulation pch_kaopen lag_kaopen
		# pch_polity lag_polity
		# pch_lncinflation lag_lncinflation
		# lag_LNgdpCAP 
			# i.ccode if upperincome~=1, pairwise corr (psar1)

model=lm(pch_Investment.Profile ~ 
	lag_Investment.Profile + 
	pch_icsidtreaty_case + lag_Cicsidtreaty_case +
	pch_ratifiedbits + lag_ratifiedbits +
	pch_LNgdp + lag_LNgdp + 
	pch_LNpopulation + lag_LNpopulation +
	pch_kaopen + lag_kaopen + 
	pch_polity + lag_polity + 
	pch_lncinflation + lag_lncinflation +
	lag_LNgdpCAP + 
	as.factor(ccode), 
	data=modelData
	)

summary(model)