### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')
# Adding in real country names that vary by year
combData$country <- panel$CNTRY_NAME[match(combData$cyear,panel$ccodeYear)]
col_idx <- grep("country", names(combData))
combData <- combData[, c(col_idx, (1:ncol(combData))[-col_idx])]
combData <- combData[combData$year>=1970 & combData$year<2012,] # Subsetting data a little bit

col_idx <- grep("WB_Fuel_Exports", names(combData))
combData <- combData[, c((1:ncol(combData))[-col_idx])]
col_idx <- grep("WB_Ore_Exports", names(combData))
combData <- combData[, c((1:ncol(combData))[-col_idx])]

combData$WB_Firms_Privatized[is.na(combData$WB_Firms_Privatized)] <- 0
combData$WB_Firms_Privatized_Energy[is.na(combData$WB_Firms_Privatized_Energy)] <- 0
combData$WB_Firms_Privatized_Infra[is.na(combData$WB_Firms_Privatized_Infra)] <- 0
################################################################################

################################################################################
# Function to generate cumulative of TS variables
setwd(pathData)
# cumulTS <- function(
# 	data=combData, cntry_var='cname', time_var='year', key='cyear', start=1971, end=2012, var){
# 	print(paste('Progress in calculating cumulative sum for ', var))
# 	cum_var <- paste('c',var,sep='')
# 	temp <- combData[data[,time_var]>=start & data[,time_var]<end,c(key,cntry_var,time_var,var)]
# 	temp <- cbind(temp, 0)
# 	names(temp) <- c('cyear', 'cntry', 'year', var, cum_var)
	
# 	countries <- unique(data[,cntry_var]); years <- start:end; fullData <- NULL

# 	for(ii in 1:length(countries)){
# 		slice <- temp[temp$cntry==countries[ii],]
# 			for(jj in 2:length(years)){
# 				slice[slice$year==years[jj],cum_var] <- 
# 					slice[slice$year==years[jj],var] + 
# 						slice[slice$year==(years[jj]-1),cum_var] }
# 			fullData <- rbind(fullData, slice) 
# 			if(ii==1 | ii%%20==0 | ii==length(countries)){
# 				cat(paste(round(100*ii/length(countries),0),'% ',sep=''))}
# 		}
# 	print(' Completed '); fullData[,c(key, var, cum_var)]
# }
# # Run cumulTS function across relevant vars
# vars <- c('settle', 'energycase', 'icsidcase', 'signedbitsSM', 'ratifiedbitsSM')
# cumul_data <- lapply(vars, function(x) FUN=cumulTS(var=x))
# save(cumul_data, file='cumulData.rda')
load('cumulData.rda')

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

Investment.ProfileRescale <- rescale(combData$Investment.Profile,10,0)
Bureaucracy.QualityRescale <- rescale(combData$Bureaucracy.Quality,10,0)
CorruptionRescale <- rescale(combData$Corruption,10,0)
Law.and.OrderRescale <- rescale(combData$Law.and.Order,10,0)
combData$Property.Rights <- (Investment.ProfileRescale + 
	Bureaucracy.QualityRescale +
	CorruptionRescale + Law.and.OrderRescale)
################################################################################

################################################################################
# Dispute var modifications
combData$cp_disputes <- combData$conc_disputes + combData$pend_disputes
combData[combData$ccode==2,'cp_disputes'] <- combData[combData$ccode==2,'icsidcase']
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
combData$LNpopulation <- log(combData$population)
################################################################################

################################################################################
# polity correction
combData$polity[combData$polity==-88] <- -10
combData$polity[combData$polity==-77] <- -10
combData$polity[combData$polity==-66] <- -10
combData$polity <- combData$polity + 10
################################################################################

################################################################################
# spatial vars
setwd(pathData)
# distMats <- list()
# years <- 1984:2011
# # date <- paste(years, '-12-31', sep='')
# # for(ii in 1:length(years)){
# # 	distMats[[ii]] <- distmatrix(as.Date(date[ii]), type="mindist", useGW=TRUE)
# # 	print(years[ii]) }
# # names(distMats) <- years
# # save(distMats, file='mindistMatrices.rda')
# load('mindistMatrices.rda')

# BitsDispMindist <- NULL
# for(i in 1:length(years)){
# 	distMat <- distMats[[i]]
# 	# rownames for matrices
# 	distNames <- as.numeric(rownames(distMat))
# 	ndistNames <- panel$ccode[match(distNames, panel$GWCODE)]
# 	rownames(distMat) <- ndistNames; colnames(distMat) <- ndistNames
# 	# setting bordering countries as having distance of 1
# 	distMat[distMat==0] <- 1
# 	inv_dmat <- 1/distMat
#   # Setting country x to x values to 0
# 	diag(inv_dmat) <- 0
# 	# Applying row standardized weights
# 	inv_dmat_rowst <- inv_dmat/apply(inv_dmat,1,sum)
# 	# Bringing in fdi dataset
# 	spat_vars <- c('ccode',
# 		"cenergycase", "cicsidcase", "csettle", 'disputesNoSettle',
# 		"conc_disputes", "pend_disputes", "cp_disputes",
# 	"signedbits", "ratifiedbits", "csignedbitsSM", "cratifiedbitsSM")
# 	dataYear <- combData[combData$year==years[i], spat_vars]
# 	dataYear <- dataYear[which(dataYear$ccode %in% ndistNames),]
# 	o <- as.character(dataYear$ccode)
# 	inv_dmat_rowst <- inv_dmat_rowst[o,o]
# 	# data rows with NAs that are in distance matrix
# 	# this is equivalent to just dropping them from teh 
# 	# spatial variable calculation
# 	dataYear[is.na(dataYear)] <- 0
# 	for(j in 1:nrow(inv_dmat_rowst)){
# 	row_weights <- NULL
# 	row_weights <- t(t(dataYear[,c(2:ncol(dataYear))]) %*%  inv_dmat_rowst[j,])
# 	row_weights2 <- NULL
# 	row_weights2 <- cbind(row_weights, years[i], dataYear$ccode[j])
# 	BitsDispMindist <- rbind(BitsDispMindist, row_weights2)
# 	}
# }
# BitsDispMindist <- data.frame(BitsDispMindist, row.names=NULL)
# names(BitsDispMindist) <- c(
# 	paste('sp_',names(BitsDispMindist)[1:(length(spat_vars)-1)],sep=''),
# 	'year','ccode')
# BitsDispMindist$cyear <- paste(BitsDispMindist$ccode, BitsDispMindist$year, sep='')
# save(BitsDispMindist, file='spatialData.rda')
load('spatialData.rda')
dim(combData); dim(BitsDispMindist)
combData <- merge(combData, BitsDispMindist[,c(1:11,14)], by='cyear', all.x=T)
dim(combData); dim(BitsDispMindist)
################################################################################

################################################################################
# Calculating change and lag in TS variables
setwd(pathData)
# combData$cyear <- as.numeric(as.character(combData$cyear))

# chgIN <- function(x) diff(c(NA, x))
# combData <- combData[order(combData$cyear),]
# dchData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
# 	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, chgIN))))
# dchData <- data.frame(dchData, row.names=NULL)
# names(dchData) <- c('cyear', paste('dch_', names(dchData)[2:length(dchData)], sep=''))
# save(dchData, file='ddiffData.rda'); dim(dchData)

# pchgIN <- function(x) diff(c(NA, x))/ifelse(abs(c(NA,x[-length(x)]))==0,1,abs(c(NA,x[-length(x)])))
# combData <- combData[order(combData$cyear),]
# pchData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
# 	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, pchgIN))))
# pchData <- data.frame(pchData, row.names=NULL)
# names(pchData) <- c('cyear', paste('pch_', names(pchData)[2:length(pchData)], sep=''))
# save(pchData, file='pdiffData.rda'); dim(pchData)

# lagTS <- function(x) c(NA, x[-length(x)])
# combData <- combData[order(combData$cyear),]
# lagData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
# 	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, lagTS))))
# lagData <- data.frame(lagData, row.names=NULL)
# names(lagData) <- c('cyear', paste('lag_', names(lagData)[2:length(lagData)], sep=''))
# save(lagData, file='lagData.rda'); dim(lagData)

load('lagData.rda'); load('pdiffData.rda'); load('ddiffData.rda')
################################################################################

################################################################################
# Read in OECD Data and upper income dummy
setwd(paste(pathData, '/Components/Controls',sep=''))
oecd <- read.csv('oecdMembers.csv')
oecd$cname <- countrycode(oecd$Country, 'country.name', 'country.name')
oecd$ccode <- panel$ccode[match(oecd$cname,panel$cname)]
# oecdFinal <- oecd[oecd$Year<=1984,]
oecdFinal <- oecd
combData$oecd <- 0
combData[which(combData$ccode %in% oecdFinal$ccode),'oecd'] <- 1
# combData <- combData[combData$oecd!=1,]

WBupperInc <- read.csv('WB_UpperInc.csv')
WBupperInc$cname <- countrycode(WBupperInc$Countries, 'country.name', 'country.name')
WBupperInc$ccode <- panel$ccode[match(WBupperInc$cname,panel$cname)]
WBupperInc <- na.omit(WBupperInc) # Dropping small islands
combData$upperincome <-  0
combData[which(combData$ccode %in% WBupperInc$ccode),'upperincome'] <- 1
################################################################################

################################################################################
# Cleaning up dataset and throwing away variables unlikely to be used
# in analysis
untransVars <- c("cyear", "ccode", "cname", "country", "year", "icsidmember", 
				"oecd", 'upperincome',
				"energycase", "icsidcase", "settle")

vars <- list(
	"cenergycase", "cicsidcase", "csettle", 'disputesNoSettle',
	"sp_cenergycase", "sp_cicsidcase", "sp_csettle", 'sp_disputesNoSettle',
	
	"cunctadcase", 

	"conc_disputes", "pend_disputes", "cp_disputes",
	"sp_conc_disputes", "sp_pend_disputes", "sp_cp_disputes",

	"signedbits", "ratifiedbits", "csignedbitsSM", "cratifiedbitsSM",
	"sp_signedbits", "sp_ratifiedbits", "sp_csignedbitsSM", "sp_cratifiedbitsSM",

	"fdiGdp", "fdi", "gdp", "gdpCAP", "r_fdi", "r_gdp", "r_gdpCAP", 
	"LNfdi", "LNgdp", "LNgdpCAP", "LNr_fdi", "LNr_gdp", "LNr_gdpCAP", 

	"USgdpDeflYr", 'USgdpDefl11', "population", "LNpopulation",

	"debtGDP",  "exprop", "debt", 

	"tradebalance", "kaopen",	

	"WB_Firms_Privatized", "WB_Firms_Privatized_Energy", "WB_Firms_Privatized_Infra",

	"Internal.Conflict", "domestic9",  "External.Conflict",

	"polity", "xrreg", "xrcomp", "xropen", "xconst", "parreg",
	"parcomp", "exrec", "exconst", "polcomp",

	"polconiii", 

	"Investment.Profile", "Corruption", "Law.and.Order", "Bureaucracy.Quality", "Property.Rights",

	"regQual",

	"overall.score", "property.rights", 

	"X2C..Protection.of.property.rights",
	"X2..Legal.System...Property.Rights",
	"SUMMARY.INDEX"
)

vars2 <- unlist(lapply(vars, function(x) FUN=paste(c('', 'pch_', 'lag_'), x,sep='')))
finVars <- c(untransVars, vars2)

dim(combData)
allData <- merge(combData, lagData, by='cyear')
allData <- merge(allData, dchData, by='cyear')
allData <- merge(allData, pchData, by='cyear')
dim(allData)

# Subsetting dataset
allData <- allData[allData$year>=1984 & allData$year<=2011,]
temp <- na.omit(allData[,c('cname', 'ccode', 'cyear', 'year', 'Investment.Profile', 
	'Bureaucracy.Quality', 'Corruption', 'Law.and.Order')])
toKeep <- unique(temp$cyear)
allData <- allData[which(allData$cyear %in% toKeep), finVars]
################################################################################

################################################################################
# Saving results for analysis
setwd(pathData)
save(allData, file='forAnalysis.rda')
write.csv(allData, file='forAnalysis.csv')
write.dta(allData, file='forAnalysis.dta')
################################################################################