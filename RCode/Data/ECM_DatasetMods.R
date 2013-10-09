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
# Fixed to icsid case variable
# I figured out the ICSID case list. Two cases should go from the 
# icsidcase: 
# Gabon v.  Societe Serete 1976, and 
# Govt of the Province of East Kalimantan v. PT Kaltim Coal and Others (Indonesia 2007).
# Two cases should be added: Egypt 1984 and Egypt 2009.
combData[combData$cname=='GABON' & combData$year==1976,'icsidcase'] <- 0
combData[combData$cname=='INDONESIA' & combData$year==2007,'icsidcase'] <- 0
combData[combData$cname=='EGYPT' & combData$year==1984,'icsidcase'] <- 1
combData[combData$cname=='EGYPT' & combData$year==2009,'icsidcase'] <- 1
################################################################################

################################################################################
# ICSID cases by three and five years moving sums
combData=moveAvgPanel(combData, 'ccode', 'year', 'icsidcase', 2)
combData$mva2_icsidcase=combData$mva2_icsidcase*2

combData=moveAvgPanel(combData, 'ccode', 'year', 'icsidcase', 3)
combData$mva3_icsidcase=combData$mva3_icsidcase*3

combData=moveAvgPanel(combData, 'ccode', 'year', 'icsidcase', 4)
combData$mva4_icsidcase=combData$mva4_icsidcase*4

combData=moveAvgPanel(combData, 'ccode', 'year', 'icsidcase', 5)
combData$mva5_icsidcase=combData$mva5_icsidcase*5
################################################################################

################################################################################
# Function to generate cumulative of TS variables
setwd(pathData)
vars <- c('settle', 'energycase', 'icsidcase', 'icsidtreaty_case',
	'signedbitsSM', 'ratifiedbitsSM')
cumul_data <- lapply(vars, function(x) FUN=cumulTS(
	data=combData, cntry_var='cname', time_var='year', key='cyear', 
	start=1960, end=2012,variable=x))

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

combData$xconst[combData$xconst==-88] <- NA
combData$xconst[combData$xconst==-77] <- NA
combData$xconst[combData$xconst==-66] <- NA

# Banks sum of conflicts
domvs=paste('domestic',1:8,sep='')
combData$domSUM=apply(combData[,domvs],1,function(x){FUN=sum(x,na.rm=T)})
################################################################################

################################################################################
# spatial vars
setwd(pathData)
years <- 1984:2011
load('mindistMatrices.rda')
load('DYADmat.rda')
distMatsD <- lapply(distMats, function(x){
	x <- ifelse(x<=200,1,0); diag(x) <- 0; x })

vars <- c("cicsidcase", "icsidcase", 
	"cicsidtreaty_case","icsidtreaty_case",
	"conc_disputes", "pend_disputes", "cp_disputes",
	"signedbits", "ratifiedbits", "csignedbitsSM", "cratifiedbitsSM")
wghtMats <- list(distMats, distMatsD, distMatsD, exportMats, tradeTotMats)
spNames <- c('distC_', 'distD_', 'distD2_', 'exp_', 'tra_')
inv <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
rst <- c(TRUE, FALSE, TRUE, TRUE, TRUE)

dim(combData)
for(ii in 1:length(wghtMats)){
	spData <- spatialBuild(spatList=wghtMats[[ii]],
		varData=combData, years=years, variable=vars,
		sp_suffix=spNames[ii], invert=inv[ii], rowST=rst[ii])
	combData <- merge(combData, 
		spData[,c(1:length(vars),ncol(spData))],by='cyear',all.x=T)
	print(spNames[ii])	}
dim(combData)
################################################################################

################################################################################
# Calculating change and lag in TS variables
setwd(pathData)
combData$cyear <- as.numeric(as.character(combData$cyear))

chgIN <- function(x) diff(c(NA, x))
combData <- combData[order(combData$cyear),]
dchData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, chgIN))))
dchData <- data.frame(dchData, row.names=NULL)
names(dchData) <- c('cyear', paste('dch_', names(dchData)[2:length(dchData)], sep=''))

pchgIN <- function(x) diff(c(NA, x))/ifelse(abs(c(NA,x[-length(x)]))==0,1,abs(c(NA,x[-length(x)])))
combData <- combData[order(combData$cyear),]
pchData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, pchgIN))))
pchData <- data.frame(pchData, row.names=NULL)
names(pchData) <- c('cyear', paste('pch_', names(pchData)[2:length(pchData)], sep=''))

lagTS <- function(x) c(NA, x[-length(x)])
combData <- combData[order(combData$cyear),]
lagData <- cbind(cyear=as.numeric(as.character(combData$cyear)), 
	apply(combData[,6:ncol(combData)], 2, function(x) FUN=unlist(by(x, combData$ccode, lagTS))))
lagData <- data.frame(lagData, row.names=NULL)
names(lagData) <- c('cyear', paste('lag_', names(lagData)[2:length(lagData)], sep=''))
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
				"energycase", "settle")

vars <- list(
	"cicsidcase", "icsidcase",
	'mva2_icsidcase', 'mva3_icsidcase', 
	'mva4_icsidcase', 'mva5_icsidcase', 

	"distC_cicsidcase", "distC_icsidcase",
	"distD_cicsidcase", "distD_icsidcase",	
	"distD2_cicsidcase", "distD2_icsidcase",		
	
	"cicsidtreaty_case", "icsidtreaty_case",
	"distC_cicsidtreaty_case", "distC_icsidtreaty_case",
	"distD_cicsidtreaty_case", "distD_icsidtreaty_case",	
	"distD2_cicsidtreaty_case", "distD2_icsidtreaty_case",		

	"cunctadcase", 

	"conc_disputes", "pend_disputes", "cp_disputes",
	"distC_conc_disputes", "distC_pend_disputes", "distC_cp_disputes",
	"distD_conc_disputes", "distD_pend_disputes", "distD_cp_disputes",
	"distD2_conc_disputes", "distD2_pend_disputes", "distD2_cp_disputes",	

	"signedbits", "ratifiedbits", "csignedbitsSM", "cratifiedbitsSM",
	"distC_signedbits", "distC_ratifiedbits", "distC_csignedbitsSM", "distC_cratifiedbitsSM",
	"distD_signedbits", "distD_ratifiedbits", "distD_csignedbitsSM", "distD_cratifiedbitsSM",			
	"distD2_signedbits", "distD2_ratifiedbits", "distD2_csignedbitsSM", "distD2_cratifiedbitsSM",		

	"fdiGdp", "fdi", "gdp", "gdpCAP", "r_fdi", "r_gdp", "r_gdpCAP", 
	"LNfdi", "LNgdp", "LNgdpCAP", "LNr_fdi", "LNr_gdp", "LNr_gdpCAP", 

	"USgdpDeflYr", 'USgdpDefl11', "population", "LNpopulation",

	"debtGDP",  "exprop", "debt", "inflation", 

	"tradebalance", "kaopen",	

	"WB_Firms_Privatized", "WB_Firms_Privatized_Energy", "WB_Firms_Privatized_Infra",

	"Internal.Conflict", "domestic9",  "domSUM", "External.Conflict",

	"polity", "xrreg", "xrcomp", "xropen", "xconst", "parreg",
	"parcomp", "exrec", "exconst", "polcomp",

	"polconiii", 

	"Investment.Profile", "Corruption", "Law.and.Order", "Bureaucracy.Quality", "Property.Rights",

	"regQual",

	"dbizRank", 'elecRank', 'elecTime', 'elecCost', 'invRank',
	'invDisc', 'invDirecLiab', 'invSuits', 'invProtect', 'traR',
	'traDocExp', 'traTimeExp', 'traCostExp', 'traDocImp', 'traTimeImp',
	'traCostImp', 'enfRank', 'enfTime', 'enfCost', 'enfProc',

	"overall.score", "property.rights", "fiscal.freedom",
	"government.spending", "business.freedom", "labor.freedom",
	"monetary.freedom", "trade.freedom", "investment.freedom",
	"financial.freedom",

	"X2C..Protection.of.property.rights",
	"X2..Legal.System...Property.Rights",
	"X2F.Legal.enforcement.of.contracts",
	"X4Bii..Compliance.costs.of.importing.and.exporting",
	"X4Dii..Capital.controls",
	
	"SUMMARY.INDEX"
)

vars2 <- unlist(lapply(vars, function(x) FUN=paste(c('', 'pch_', 'lag_'), x,sep='')))
finVars <- c(untransVars, vars2)

dim(combData)
allData <- merge(combData, lagData, by='cyear')
allData <- merge(allData, dchData, by='cyear')
allData <- merge(allData, pchData, by='cyear')
dim(allData)

# Saving data for descriptive analysis
setwd(pathData)
allData0 <- allData
save(allData0, file='forAnalysis0.rda')

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