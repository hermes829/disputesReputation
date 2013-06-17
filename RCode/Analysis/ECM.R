### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')

# DV 
# Investment Profile from PRS Group: Investment.Profile

# Model covariates
# + GDP: gdp.y
# + GDP growth: chgdp
# + Polity / Polity^2 / Change in polity / Years in democracy: polity
# + Banks conflict index
# + Trade balance: tradebalance
# + BITs: signedbits
# + Settlement of concluded cases [[ would be fun if there was a positive relationship ]]: settle
# + Inflation: 
# + Dummy for upper income (World Bank Classification): 
# + ICSID disputes: conc_disputes
# + Spatial disputes [ distance ]: cshapes
# + FDI
# + Natural resource
#     - Possible interaction between this and ICSID disputes

# Variable manipulation
vars <- c('ccode', 'cname', 'year', 'cyear',
	'Investment.Profile', 'gdp.y', 'chgdp', 'polity', 
	'signedbits', 'settle', 'conc_disputes', 'exprop')
modelData <- combData[,vars]
dim(modelData)

# change from t-1
countries <- unique(modelData$cname)
years <- 1984:2011
chData <- NULL
for(ii in 1:length(countries)){
	slice <- modelData[modelData$cname==countries[ii],]
	for(jj in 2:length(years)){
		cyear <- slice[slice$year==years[jj],'cyear']
		tData <- slice[slice$year==years[jj],5:ncol(slice)]
		t_1Data <- slice[slice$year==(years[jj]-1),5:ncol(slice)]
		chSlice <- tData - t_1Data
		colnames(chSlice) <- paste('ch_' ,colnames(tData),sep='')
		chSlice <- cbind(cyear, chSlice)
		chData <- rbind(chData, chSlice) }
	print(ii) }
dim(modelData); dim(chData)
modelData <- merge(modelData, chData, by='cyear', all.x=T)
dim(modelData); dim(chData)
# dirty lag
modelData <- ddply(modelData,.(ccode),transform,
	Investment.ProfileLag = c(NA, Investment.Profile[-length(Investment.Profile)]))
modelData <- ddply(modelData,.(ccode),transform,
	gdp.yLag = c(NA, gdp.y[-length(gdp.y)]))
modelData <- ddply(modelData,.(ccode),transform,
	chgdpLag = c(NA, chgdp[-length(chgdp)]))
modelData <- ddply(modelData,.(ccode),transform,
	polityLag = c(NA, polity[-length(polity)]))
modelData <- ddply(modelData,.(ccode),transform,
	signedbitsLag = c(NA, signedbits[-length(signedbits)]))
modelData <- ddply(modelData,.(ccode),transform,
	settleLag = c(NA, settle[-length(settle)]))
modelData <- ddply(modelData,.(ccode),transform,
	conc_disputesLag = c(NA, conc_disputes[-length(conc_disputes)]))
modelData <- ddply(modelData,.(ccode),transform,
	expropLag = c(NA, exprop[-length(exprop)]))

# polity correction
combData[combData$polity==-88,'polity'] <- -10
combData[combData$polity==-77,'polity'] <- -10
combData[combData$polity==-66,'polity'] <- -10

# conc_disputes spatial


# Running model
modelData <- na.omit(modelData)
# Form of ECM
# ∆Yt = α + β0*∆Xt - β1(Yt-1 - β2Xt-1) + ε
formula = Investment.Profile ~ gdp.y + chgdp + polity + tradebalance + signedbits + 
			settle + conc_disputes

