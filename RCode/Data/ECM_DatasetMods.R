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
# Disputes by three and five years moving sums
dispVars=c('settle','kicsidcase','icsidtreaty_case','cunctadcase','unsettled_icsid_treaty','alltreaty')
wdows=2:5
for(ii in 1:length(wdows)){combData=movePanel(combData, 'ccode', 'year', dispVars, wdows[ii], sum=TRUE)} 
################################################################################

################################################################################
# Function to generate cumulative of TS variables
setwd(pathData)
dispVars=c('settle','kicsidcase','icsidtreaty_case','cunctadcase','unsettled_icsid_treaty','alltreaty')
combData=cumulTS(combData, 'ccode', 'year', dispVars)

otherVars=c('large_reversal','reversal','reform','large_reform','status_quo')
combData=cumulTS(combData, 'ccode', 'year', otherVars)
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

combData$democ=as.numeric(combData$polity>=16)

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

vars <- c("settle", "Csettle", 
	"kicsidcase","Ckicsidcase",
	"icsidtreaty_case", "Cicsidtreaty_case",
	"cunctadcase", "Ccunctadcase", 
	"unsettled_icsid_treaty", "Cunsettled_icsid_treaty",
	'alltreaty','Calltreaty',
	"ratifiedbits")
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

# WBupperInc <- read.csv('WB_UpperInc.csv')
# WBupperInc$cname <- countrycode(WBupperInc$Countries, 'country.name', 'country.name')
# WBupperInc$ccode <- panel$ccode[match(WBupperInc$cname,panel$cname)]
# WBupperInc <- na.omit(WBupperInc) # Dropping small islands
# combData$upperincome <-  0
# combData[which(combData$ccode %in% WBupperInc$ccode),'upperincome'] <- 1
################################################################################

################################################################################
# Cleaning up dataset and throwing away variables unlikely to be used
# in analysis
untransVars <- c("cyear", "ccode", "cname", "country", "year",
				"oecd", 'upperincome', 'icsidmember')

spLabs=function(x){paste(c('distC','distD','distD2'),x,sep='_')}

vars <- c(
	"settle", "Csettle", 
	# paste('mvs',2:5,'_settle',sep=''),
	# spLabs('settle'),spLabs('Csettle'),

	"kicsidcase", "Ckicsidcase",
	# paste('mvs',2:5,'_kicsidcase',sep=''),
	# spLabs('kicsidcase'),spLabs('Ckicsidcase'),

	"icsidtreaty_case", "Cicsidtreaty_case",
	# paste('mvs',2:5,'_icsidtreaty_case',sep=''),	
	# spLabs('icsidtreaty_case'), spLabs('Cicsidtreaty_case'),

	"cunctadcase", "Ccunctadcase",
	# paste('mvs',2:5,'_cunctadcase',sep=''),	
	# spLabs('cunctadcase'), spLabs('Ccunctadcase'),

	"unsettled_icsid_treaty", "Cunsettled_icsid_treaty",
	# paste('mvs',2:5,'_unsettled_icsid_treaty',sep=''),	
	# spLabs('unsettled_icsid_treaty'), spLabs('Cunsettled_icsid_treaty'),

	"alltreaty",'Calltreaty',

	"ratifiedbits",
	# spLabs('ratifiedbits'), 

	"fdiGdp", "fdi", "gdp", "gdpCAP", "r_fdi", "r_gdp", "r_gdpCAP", 
	"LNfdi", "LNgdp", "LNgdpCAP", "LNr_fdi", "LNr_gdp", "LNr_gdpCAP", 

	"USgdpDeflYr", 'USgdpDefl11', "population", "LNpopulation",

	"debtGDP",  "exprop", "debt", "inflation", "lncinflation", 'tradebalance',

	"kaopen", 'cbdcrisis',

	# "WB_Firms_Privatized", "WB_Firms_Privatized_Energy", "WB_Firms_Privatized_Infra",

	"Internal.Conflict", "domestic9",  "domSUM", "External.Conflict",

	"polity", "xrreg", "xrcomp", "xropen", "xconst", "parreg",
	"parcomp", "exrec", "exconst", "polcomp", 'democ',

	"polconiii", 

	# ICRG data
	"Investment.Profile", "Corruption", "Law.and.Order", "Bureaucracy.Quality", "Property.Rights",

	"regQual",

	# WB biz data
	# "dbizRank", 'elecRank', 'elecTime', 'elecCost', 'invRank',
	# 'invDisc', 'invDirecLiab', 'invSuits', 'invProtect', 'traR',
	# 'traDocExp', 'traTimeExp', 'traCostExp', 'traDocImp', 'traTimeImp',
	# 'traCostImp', 'enfRank', 'enfTime', 'enfCost', 'enfProc',

	# IMF data
	'directedcredit', 'creditceilings', 'creditcontrols', 'intratecontrols',
	'entrybarriers', 'bankingsuperv', 'privatization', 'intlcapital',
	'securitymarkets', 'finreform', 'finreform_n', 
	'large_reversal','reversal','reform','large_reform','status_quo',
	'Clarge_reversal','Creversal','Creform','Clarge_reform','Cstatus_quo',

	# # Heritage data
	# "overall.score", "property.rights", "fiscal.freedom",
	# "government.spending", "business.freedom", "labor.freedom",
	# "monetary.freedom", "trade.freedom", "investment.freedom",
	# "financial.freedom",

	# # Fraser data
	# "X2C..Protection.of.property.rights",
	# "X2..Legal.System...Property.Rights",
	# "X2F.Legal.enforcement.of.contracts",
	# "X4Bii..Compliance.costs.of.importing.and.exporting",
	# "X4Dii..Capital.controls",	
	# "SUMMARY.INDEX",

	# KoF data
	'actFlows','cultProx','infFlows','persContact',
	'econGlob','indexGlob','polGlob','restrictions','socGlob'
)

vars2=list()
for(ii in 1:length(vars)){vars2[[ii]]=vars[ii]}

vars2 <- unlist(lapply(vars, function(x) FUN=paste(c('', 'pch_','dch_', 'lag_'), x,sep='')))
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