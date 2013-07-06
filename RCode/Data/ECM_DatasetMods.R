### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

################################################################################
# Function to generate cumulative of TS variables
cumulTS <- function(
	data=combData, cntry_var='cname', time_var='year', key='cyear', start=1971, end=2012, var){
	print(paste('Progress in calculating cumulative sum for ', var))
	cum_var <- paste('c',var,sep='')
	temp <- combData[data[,time_var]>=start & data[,time_var]<end,c(key,cntry_var,time_var,var)]
	temp <- cbind(temp, 0)
	names(temp) <- c('cyear', 'cntry', 'year', var, cum_var)
	
	countries <- unique(data[,cntry_var]); years <- start:end; fullData <- NULL

	for(ii in 1:length(countries)){
		slice <- temp[temp$cntry==countries[ii],]
			for(jj in 2:length(years)){
				slice[slice$year==years[jj],cum_var] <- 
					slice[slice$year==years[jj],var] + 
						slice[slice$year==(years[jj]-1),cum_var] }
			fullData <- rbind(fullData, slice) 
			if(ii==1 | ii%%20==0 | ii==length(countries)){
				cat(paste(round(100*ii/length(countries),0),'% ',sep=''))}
		}
	print(' Completed '); fullData[,c(key, var, cum_var)]
}
# Run cumulTS function across relevant vars
vars <- c('settle', 'energycase', 'icsidcase')
cumul_data <- lapply(vars, function(x) FUN=cumulTS(var=x))

# Merge results back into original dataframe
dim(combData)
for(ii in 1:length(cumul_data)){
	toMerge <- cumul_data[[ii]]
	combData <- merge(combData, toMerge[,c(1,3)], by='cyear', all.x=T, all.y=F) }
dim(combData)
################################################################################

################################################################################
# DV Investment Profile from PRS Group: Investment.Profile
# Property rights from PRS group:
# First need to rescale each of the IVs
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

combData$Investment.ProfileRescale <- rescale(combData$Investment.Profile,10,0)
combData$Bureaucracy.QualityRescale <- rescale(combData$Bureaucracy.Quality,10,0)
combData$CorruptionRescale <- rescale(combData$Corruption,10,0)
combData$Law.and.OrderRescale <- rescale(combData$Law.and.Order,10,0)
combData$Property.Rights <- (combData$Investment.ProfileRescale + 
	combData$Bureaucracy.QualityRescale +
	combData$CorruptionRescale + combData$Law.and.OrderRescale)
################################################################################

################################################################################
# Dispute var modifications
combData$cp_disputes <- combData$conc_disputes + combData$pend_disputes
combData$cp_disputes_three <- combData$conc_disputes_three + combData$pend_disputes_three
combData$cp_disputes_five <- combData$conc_disputes_five + combData$pend_disputes_five

# Use data from Remmer since there are still differences between her 
# and my icsid case variables
combData$disputesNoSettle <- combData$cicsidcase - combData$csettle
################################################################################

################################################################################
# Dividing current US$ variables by US GDP deflator to get 2011 real values
# Vars to calc real values for: fdi, gdp, gdpCAP
yearDefl <- na.omit(summaryBy(gdpDeflator ~ year, data=combData[combData$cname=='UNITED STATES',], na.rm=T))
names(yearDefl) <- c('year', 'USgdpDeflYr')
yearDefl$USgdpDefl11 <- yearDefl[yearDefl$year==2011,2]
combData <- merge(combData, yearDefl, by='year', all.x=T, all.y=F)
combData$r_fdi <- (combData$fdi/combData$USgdpDeflYr)*combData$USgdpDefl11
combData$r_gdp <- (combData$gdp/combData$USgdpDeflYr)*combData$USgdpDefl11
combData$r_gdpCAP <- (combData$gdpCAP/combData$USgdpDeflYr)*combData$USgdpDefl11

# Log transformations for GDP, GDP capita, FDI
logNeg <- function(z){
	x <- z[!is.na(z)]; y <- x
	y[x>0] <- log(x[x>0]); y[x<0] <- -log(abs(x[x<0])); y[x==0] <- 0
	z[!is.na(z)] <- y; z
}

combData$LNfdi <- logNeg(combData$fdi); combData$LNr_fdi <- logNeg(combData$r_fdi)
combData$LNtradebalance <- logNeg(combData$tradebalance)
combData$LNgdp <- log(combData$gdp); combData$LNr_gdp <- log(combData$r_gdp)
combData$LNgdpCAP <- log(combData$gdpCAP); combData$LNr_gdpCAP <- log(combData$r_gdpCAP)
################################################################################

################################################################################
# polity correction
combData$polity[combData$polity==-88] <- -10
combData$polity[combData$polity==-77] <- -10
combData$polity[combData$polity==-66] <- -10
combData$polity <- combData$polity + 10
################################################################################

################################################################################
# Calculating change in TS variables
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

# Vars for Analysis
vars <- c("cyear", "ccode", "cname", "year",                  

	"energycase", "icsidcase", "settle", "cunctadcase", "icsidmember",

	"conc_disputes", "conc_disputes_five", "conc_disputes_three",
	"pend_disputes", "pend_disputes_five", "pend_disputes_three",

	"signedbits", "ratifiedbits", "signedbitsSM", "ratifiedbitsSM",

	"fdi", "fdiGdp", "gdp", "gdpCAP", "GDP_Deflator_US", "population",
	"debtGDP",  "exprop", "debt", "tradebalance", "kaopen",

	"WB_Firms_Privatized", "WB_Firms_Privatized_Energy", "WB_Firms_Privatized_Infra",

	"Internal.Conflict", "domestic9",
	"domestic1", "domestic2", "domestic3", "domestic4", "domestic5", "domestic6",
	"domestic7", "domestic8",  "External.Conflict",

	"polity", "polity2", "xrreg", "xrcomp", "xropen", "xconst", "parreg",
	"parcomp", "exrec", "exconst", "polcomp",

	"Investment.Profile", "Corruption", "Law.and.Order", "Bureaucracy.Quality",

	"regQual",

	"overall.score", "property.rights", "freedom.from.corruption",
	"trade.freedom", "investment.freedom", "financial.freedom",

	"X2A..Judicial.independence",
	"X2B..Impartial.courts",
	"X2C..Protection.of.property.rights",
	"X2E.Integrity.of.the.legal.system",
	"X2F.Legal.enforcement.of.contracts",
	"X2..Legal.System...Property.Rights",
	"X4Bi..Non.tariff.trade.barriers",
	"X4B..Regulatory.trade.barriers",
	"X4Di..Foreign.ownership.investment.restrictions",
	"X4..Freedom.to.trade.internationally",
	"SUMMARY.INDEX"
)

# Subsetting dataset
combData <- combData[combData$year>=1984 & combData$year<=2011,]
temp <- na.omit(combData[,c('cname', 'ccode', 'cyear', 'year', 'Investment.Profile', 'Bureaucracy.Quality',
	'Corruption','Law.and.Order')])
toKeep <- unique(temp$cyear)
combData <- combData[which(combData$cyear %in% toKeep), vars]

combData$country <- panel$CNTRY_NAME[match(combData$cyear,panel$ccodeYear)]


# # Running model
setwd(pathData)
modelData <- na.omit(modelData)
save(modelData, file='forAnalysis.rda')

anData <- merge(modelData, 
	combData[,c('cyear', setdiff(names(combData), names(modelData)))],
	by='cyear',all.x=T,all.y=F)
save(anData, file='forAnalysisFull.rda')
write.csv(anData, file='forKaren.csv')