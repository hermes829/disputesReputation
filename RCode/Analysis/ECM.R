### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')

# DV 
# Investment Profile from PRS Group: Investment.Profile
# Property rights from PRS group:
# First need to rescale each of the IVs
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

combData$Investment.ProfileRescale <- rescale(combData$Investment.Profile,10,0)
combData$Bureaucracy.QualityRescale <- rescale(combData$Bureaucracy.Quality,10,0)
combData$CorruptionRescale <- rescale(combData$Corruption,10,0)
combData$Law.and.OrderRescale <- rescale(combData$Law.and.Order,10,0)

# cumul settle variable
temp <- combData[combData$year>=1971 & combData$year<2012,]
temp$csettle <- 0
countries <- unique(combData$cname)
years <- 1971:2011
fullData <- NULL
for(ii in 1:length(countries)){
	slice <- temp[temp$cname==countries[ii],c('cyear','year','settle','csettle')]
	for(jj in 2:length(years)){
		slice[slice$year==years[jj],'csettle'] <- 
			slice[slice$year==years[jj],'settle'] + 
				slice[slice$year==(years[jj]-1),'csettle'] }
	fullData <- rbind(fullData, slice) 
	print(ii)}
dim(combData); dim(fullData)
combData <- merge(combData, fullData[,c(1,4)], by='cyear', all.x=T, all.y=F)
dim(combData); dim(fullData)

# Other var transformations
combData$Property.Rights <- (combData$Investment.ProfileRescale + 
	combData$Bureaucracy.QualityRescale +
	combData$CorruptionRescale + combData$Law.and.OrderRescale)/4

combData$cp_disputes <- combData$conc_disputes + combData$pend_disputes
combData$cp_disputes_three <- combData$conc_disputes_three + combData$pend_disputes_three
combData$cp_disputes_five <- combData$conc_disputes_five + combData$pend_disputes_five

combData$disputesNoSettle <- combData$cp_disputes - combData$csettle

# log transformation
combData$LNgdp.y <- log(combData$gdp.y)
combData$LNtradebalance <- log( combData$tradebalance + 200 )

# polity correction
combData$polity[combData$polity==-88] <- -10
combData$polity[combData$polity==-77] <- -10
combData$polity[combData$polity==-66] <- -10
combData$polity <- combData$polity + 10

# Model covariates
# + GDP: LNgdp.y
# + GDP growth: chgdp
# + Polity / Polity^2 / Change in polity / Years in democracy: polity
# + Banks conflict index
# + Trade balance: LNtradebalance
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
	'Investment.Profile', 
	'Property.Rights',
	'LNtradebalance',
	'LNgdp.y', 
	# 'chgdp', 
	'polity', 
	'signedbits',
	'conc_disputes',
	'pend_disputes',
	'cp_disputes',
	'csettle',
	'disputesNoSettle'
	)
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
		colnames(chSlice) <- paste('dch_' ,colnames(tData),sep='')
		chSlice <- cbind(cyear, chSlice)
		chData <- rbind(chData, chSlice) }
	print(ii) }
dim(modelData); dim(chData)
modelData <- merge(modelData, chData, by='cyear', all.x=T, all.y=F)
dim(modelData); dim(chData)

countries <- unique(modelData$cname)
years <- 1984:2011
chData <- NULL
for(ii in 1:length(countries)){
	slice <- modelData[modelData$cname==countries[ii],1:length(vars)]
	for(jj in 2:length(years)){
		cyear <- slice[slice$year==years[jj],'cyear']
		tData <- slice[slice$year==years[jj],5:ncol(slice)]
		t_1Data <- slice[slice$year==(years[jj]-1),5:ncol(slice)]
		t_1DataDenom <- data.matrix(t_1Data)
		t_1DataDenom[t_1DataDenom==0] <- 1
		chSlice <- (tData - t_1Data)/t_1DataDenom
		colnames(chSlice) <- paste('pch_' ,colnames(tData),sep='')
		chSlice <- cbind(cyear, chSlice)
		chData <- rbind(chData, chSlice) }
	print(ii) }
dim(modelData); dim(chData)
modelData <- merge(modelData, chData, by='cyear', all.x=T, all.y=F)
dim(modelData); dim(chData)

# dirty lag
modelData <- ddply(modelData,.(ccode),transform,
	Investment.ProfileLag = c(NA, Investment.Profile[-length(Investment.Profile)]))
modelData <- ddply(modelData,.(ccode),transform,
	Property.RightsLag = c(NA, Property.Rights[-length(Property.Rights)]))
modelData <- ddply(modelData,.(ccode),transform,
	LNtradebalanceLag = c(NA, LNtradebalance[-length(LNtradebalance)]))
modelData <- ddply(modelData,.(ccode),transform,
	LNgdp.yLag = c(NA, LNgdp.y[-length(LNgdp.y)]))
# modelData <- ddply(modelData,.(ccode),transform,
# 	chgdpLag = c(NA, chgdp[-length(chgdp)]))
modelData <- ddply(modelData,.(ccode),transform,
	polityLag = c(NA, polity[-length(polity)]))
modelData <- ddply(modelData,.(ccode),transform,
	signedbitsLag = c(NA, signedbits[-length(signedbits)]))
modelData <- ddply(modelData,.(ccode),transform,
	disputesNoSettleLag = c(NA, disputesNoSettle[-length(disputesNoSettle)]))
modelData <- ddply(modelData,.(ccode),transform,
	csettleLag = c(NA, csettle[-length(csettle)]))
modelData <- ddply(modelData,.(ccode),transform,
	cp_disputesLag = c(NA, cp_disputes[-length(cp_disputes)]))
modelData <- ddply(modelData,.(ccode),transform,
	conc_disputesLag = c(NA, conc_disputes[-length(conc_disputes)]))
modelData <- ddply(modelData,.(ccode),transform,
	pend_disputesLag = c(NA, pend_disputes[-length(pend_disputes)]))
# modelData <- ddply(modelData,.(ccode),transform,
# 	expropLag = c(NA, exprop[-length(exprop)]))

# conc_disputes spatial


# # Running model
setwd(pathData)
modelData <- na.omit(modelData)
forKaren <- merge(modelData, 
	combData[,c('cyear', setdiff(names(combData), names(modelData)))],
	by='cyear',all.x=T,all.y=F)
write.csv(forKaren, file='forKaren.csv')

# ### Descriptive statistics
summary(modelData)
modelSumm <- modelData[,5:ncol(modelData)]
means <- apply(modelSumm, 2, mean)
sds <- apply(modelSumm, 2, sd)
quants <- t(apply(modelSumm, 2, function(x) FUN=quantile(x,seq(0,1,.25))))
xtable(cbind(means, sds, quants),digits=3)

# Form of ECM
# ∆Yt = α + β0*∆Xt - β1(Yt-1 - β2Xt-1) + ε
# formula = Investment.Profile ~ LNgdp.y + chgdp + polity + LNtradebalance + signedbits + 
			# settle + conc_disputes

# formula = ch_Investment.Profile ~ 
			# Investment.ProfileLag + 
formula = ch_Property.Rights ~ 
			Property.RightsLag + 			
			ch_LNgdp.y + ch_LNtradebalance + ch_polity + ch_signedbits + ch_disputesNoSettle +
			LNgdp.yLag + LNtradebalanceLag + polityLag + signedbitsLag + disputesNoSettleLag +
			as.factor(ccode)-1

modelResults <- lm(formula, data=modelData)
modelLatex <- xtable(modelResults)[1:11,]
# rownames(modelLatex) <- c('Investment Profile$_{t-1}$',
# 	'$\\Delta$GDP$_{t}$',
# 	'$\\Delta$GDP Growth$_{t}$',
# 	'$\\Delta$Polity$_{t}$',
# 	'$\\Delta$BITs$_{t}$',
# 	'$\\Delta$ICSID Settlements$_{t}$',
# 	'$\\Delta$ICSID Disputes$_{t}$',
# 	'$\\Delta$Expropriation$_{t}$',
# 	'GDP$_{t-1}$',
# 	'GDP Growth$_{t-1}$',
# 	'Polity$_{t-1}$',
# 	'BITs$_{t-1}$',
# 	'ICSID Settlement$_{t-1}$',
# 	'ICSID Dispute$_{t-1}$',
# 	'Expropriation$_{t-1}$')
# print(xtable(modelLatex, sanitize.text.function=function(x){x}))
modelLatex

# Prelim graphics
setwd(pathGraphics)
pdf(file='ch_inv_hist.pdf')
hist(modelData$ch_Investment.Profile)
dev.off()