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

# signedbitsSM settle variable
temp <- combData[combData$year>=1960 & combData$year<=2012,]
temp$csignedbitsSM <- 0
countries <- unique(combData$cname)
years <- 1960:2012
fullData <- NULL
for(ii in 1:length(countries)){
	slice <- temp[temp$cname==countries[ii],c('cyear','year','signedbitsSM','csignedbitsSM')]
	for(jj in 2:length(years)){
		slice[slice$year==years[jj],'csignedbitsSM'] <- 
			slice[slice$year==years[jj],'signedbitsSM'] + 
				slice[slice$year==(years[jj]-1),'csignedbitsSM'] }
	fullData <- rbind(fullData, slice) 
	print(ii)}
dim(combData); dim(fullData)
combData <- merge(combData, fullData[,c(1,4)], by='cyear', all.x=T, all.y=F)
dim(combData); dim(fullData)

# ratifiedbitsSM settle variable
temp <- combData[combData$year>=1960 & combData$year<=2012,]
temp$cratifiedbitsSM <- 0
countries <- unique(combData$cname)
years <- 1960:2012
fullData <- NULL
for(ii in 1:length(countries)){
	slice <- temp[temp$cname==countries[ii],c('cyear','year','ratifiedbitsSM','cratifiedbitsSM')]
	for(jj in 2:length(years)){
		slice[slice$year==years[jj],'cratifiedbitsSM'] <- 
			slice[slice$year==years[jj],'ratifiedbitsSM'] + 
				slice[slice$year==(years[jj]-1),'cratifiedbitsSM'] }
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

# Logged disputes and bits measures
lvars <- c('cratifiedbitsSM', 'csignedbitsSM', 'csettle',
	'conc_disputes', 'pend_disputes', 'cp_disputes',
	'disputesNoSettle')
for(ii in 1:length(lvars)){ 
	combData[paste('log_',lvars[ii],sep='')] <- NA
	combData[,paste('log_',lvars[ii],sep='')] <- log(combData[,lvars[ii]]+0.0001) 
}

# combData$cratifiedbitsSM <- combData$log_cratifiedbitsSM
# combData$csignedbitsSM <- combData$log_csignedbitsSM
# combData$csettle <- combData$log_csettle
# combData$conc_disputes <- combData$log_conc_disputes
# combData$pend_disputes <- combData$log_pend_disputes
# combData$cp_disputes <- combData$log_cp_disputes
# combData$disputesNoSettle <- combData$log_disputesNoSettle

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
	'csignedbitsSM',
	'cratifiedbitsSM',
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
	csignedbitsSMLag = c(NA, csignedbitsSM[-length(csignedbitsSM)]))
modelData <- ddply(modelData,.(ccode),transform,
	cratifiedbitsSMLag = c(NA, cratifiedbitsSM[-length(cratifiedbitsSM)]))
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
save(modelData, file='forAnalysis.rda')

anData <- merge(modelData, 
	combData[,c('cyear', setdiff(names(combData), names(modelData)))],
	by='cyear',all.x=T,all.y=F)
save(anData, file='forAnalysisFull.rda')
write.csv(anData, file='forKaren.csv')