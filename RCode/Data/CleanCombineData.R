### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### Load data
setwd(pathData)
load('allData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

###############################################################
# Organizing WB data
### Fx for Melting/Cleaning WB Data for Merge

cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations and small islands/territories
	drop <- c('Arab World', 'Caribbean small states', 
		'East Asia & Pacific (all income levels)', 
		'East Asia & Pacific (developing only)', 'Euro area', 
		'Europe & Central Asia (all income levels)', 
		'Europe & Central Asia (developing only)', 
		'European Union', 'Heavily indebted poor countries (HIPC)', 
		'High income', 'High income: nonOECD', 'High income: OECD', 
		'Latin America & Caribbean (all income levels)', 
		'Latin America & Caribbean (developing only)', 
		'Least developed countries: UN classification', 
		'Low & middle income', 'Low income', 'Lower middle income', 
		'Middle East & North Africa (all income levels)', 
		'Middle East & North Africa (developing only)', 'Middle income', 
		'North America', 'Not classified', 'OECD members', 
		'Other small states', 'Pacific island small states', 
		'Small states', 'South Asia', 
		'Sub-Saharan Africa (all income levels)', 
		'Sub-Saharan Africa (developing only)', 'Upper middle income', 
		'World',
		 "American Samoa",            "Aruba",                    
		 "Bermuda",                   "Cayman Islands", "Channel Islands",          
		 "Curacao",                   "Faeroe Islands",           
		 "French Polynesia",          "Greenland",                
		 "Guam",                      "Hong Kong SAR, China",     
		 "Isle of Man",               "Macao SAR, China",         
		 "New Caledonia",             "Northern Mariana Islands", 
		 "Puerto Rico",               "Sint Maarten (Dutch part)",
		 "St. Martin (French part)",  "Turks and Caicos Islands", 
		 "Virgin Islands (U.S.)",     "West Bank and Gaza")
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country.Name <- as.character(mdata$Country.Name)
	mdata$Country.Name[mdata$Country.Name=='Korea, Dem. Rep.'] <- 'North Korea' 
	mdata$Country.Name[mdata$Country.Name=='Korea, Rep.'] <- 'South Korea' 
	mdata$cname <- countrycode(mdata$Country.Name, 'country.name', 'country.name')
	mdata$cnameYear <- paste(mdata$cname, mdata$year, sep='')
	
	# Adding in codes from panel
	mdata$ccode <- panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear <- paste(mdata$ccode, mdata$year, sep='')
	mdata }

WBinflDeflatorClean <- cleanWbData(WBinflDeflator, 'inflDeflator')
WBgdpDeflatorClean <- cleanWbData(WBgdpDeflator, 'gdpDeflator')
WBfdiClean <- cleanWbData(WBfdi, 'fdi')
WBfdiGdpClean <- cleanWbData(WBfdiGdp, 'fdiGDP')
WBgdpClean <- cleanWbData(WBgdp, 'gdp')
WBgdpCapClean <- cleanWbData(WBgdpCap, 'gdpCAP')
WBpopClean <- cleanWbData(WBpop, 'population')
WBdebtClean <- cleanWbData(WBdebt, 'debtGDP')

# Make sure order matches
sum(WBinflDeflatorClean$cyear!=WBgdpDeflatorClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBfdiClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBfdiGdpClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBgdpClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBgdpCapClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBpopClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBdebtClean$cyear)

# combine data
setwd(pathData)
wbData <- data.frame(cbind(WBinflDeflatorClean,
	gdpDeflator=WBgdpDeflatorClean[,4], fdi=WBfdiClean[,4],
	fdiGdp=WBfdiGdpClean[,4], gdp=WBgdpClean[,4],
	gdpCAP=WBgdpCapClean[,4]), population=WBpopClean[,4],
	debtGDP=WBdebtClean[,4])
save(wbData, file='wbData.rda')
###############################################################

###############################################################
# kaopen (uses imf numeric codes, cn)
kaopen2 <- kaopen

kaopen2$country_name <- as.character(kaopen2$country_name)
kaopen2$country_name[kaopen2$country_name=="S? Tom\341and Principe"] <- 'Sao Tome'
kaopen2$country_name[kaopen2$country_name=="C?e d'Ivoire"] <- 'Ivory Coast'
drop <- c("Aruba", "Netherlands Antilles", "Hong Kong, China")
kaopen2 <- kaopen2[which(!kaopen2$country_name %in% drop),]
kaopen2$cname <- countrycode(kaopen2$country_name, 'country.name', 'country.name')

kaopen2$cnameYear <- paste(kaopen2$cname, kaopen2$year, sep='')

table(kaopen2$cnameYear)[table(kaopen2$cnameYear)>1] # Dupe check

# Adding in codes from panel
kaopen2$ccode <- panel$ccode[match(kaopen2$cname,panel$cname)]
kaopen2$cyear <- paste(kaopen2$ccode, kaopen2$year, sep='')
table(kaopen2$cyear)[table(kaopen2$cyear)>1] # Dupe check
###############################################################

###############################################################
# constraints
constraints2 <- constraints[constraints$year>=1960,1:10]
constraints2 <- constraints2[!is.na(constraints2$ccode),]

constraints2$cnts_country <- as.character(constraints2$cnts_country)
constraints2$cnts_country[constraints2$cnts_country=='CONGO (BRA)'] <- 'Congo, Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO (KIN)'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO DR'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='GERMAN DR'] <- "Germany Democratic Republic"

constraints2$cname <- countrycode(constraints2$cnts_country, 'country.name', 'country.name')
constraints2[is.na(constraints2$cname),'cname'] <- countrycode(
	constraints2[is.na(constraints2$cname),'polity_country'],
	'country.name', 'country.name')
constraints2$cname[constraints2$cnts_country=='VIETNAM REP'] <- 'S. VIETNAM'
constraints2$cname[constraints2$cnts_country=='YEMEN PDR'] <- 'S. YEMEN'
constraints2$cname[constraints2$cnts_country=="CZECHOS'KIA"] <- 'CZECH REPUBLIC'
constraints2$cname[constraints2$cnts_country=="YUGOSLAVIA"] <- 'SERBIA'
constraints2 <- constraints2[constraints2$cname!='HONG KONG',]

constraints2$cnameYear <- paste(constraints2$cname, constraints2$year, sep='')

names(table(constraints2$cnameYear)[table(constraints2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
constraints2$ccode <- panel$ccode[match(constraints2$cname,panel$cname)]
constraints2$cyear <- paste(constraints2$ccode, constraints2$year, sep='')
table(constraints2$cyear)[table(constraints2$cyear)>1] # Dupe check
###############################################################

###############################################################
# banks dataset
banks2 <- banks[banks$year>=1960,c('code', 'Wbcode', 'country', 
	'year', paste('domestic', 1:9, sep=''))]
banks2 <- banks2[!is.na(banks2$code),]
banks2$country <- trim(banks2$country)

banks2$country <- as.character(banks2$country)
banks2$country[banks2$country=='Congo (BRA)'] <- 'Congo, Republic of'
banks2$country[banks2$country=='Congo (KIN)'] <- 'Congo, Democratic Republic of'
banks2$country[banks2$country=='German DR'] <- "Germany Democratic Republic" 
banks2$country[banks2$country=='German FR'] <- "Germany" 
banks2 <- banks2[banks2$country!='Cyprus: Turkish Sector',]
banks2 <- banks2[banks2$country!='Cyprus: Greek Sector',]
banks2 <- banks2[banks2$country!='Senegambia',]
banks2 <- banks2[banks2$country!='Somaliland',]
banks2 <- banks2[banks2$code!=1145,] # Removing extra cases for Trinidad
banks2 <- banks2[banks2$code!=1247,] # Removing extra cases for Venezuela

banks2$cname <- countrycode(banks2$country, 'country.name', 'country.name')
banks2$cname[banks2$country=='Vietnam REP'] <- 'S. VIETNAM'
banks2$cname[banks2$country=='Yemen PDR'] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yemen PDR (So. Yemen)"] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yugoslavia"] <- 'SERBIA'
banks2 <- banks2[banks2$cname!='HONG KONG',]
banks2$cname[banks2$country=="Czechoslovakia"] <- 'CZECH REPUBLIC'

drop <- unique(banks2[is.na(banks2$cname),c('country')])
banks2 <- banks2[which(!banks2$country %in% drop),]

banks2$cnameYear <- paste(banks2$cname, banks2$year, sep='')

names(table(banks2$cnameYear)[table(banks2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
banks2$ccode <- panel$ccode[match(banks2$cname,panel$cname)]
banks2$cyear <- paste(banks2$ccode, banks2$year, sep='')
drop <- unique(banks2[is.na(banks2$ccode),'cname'])
banks2 <- banks2[which(!banks2$cname %in% drop),]
table(banks2$cyear)[table(banks2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Polity
polity2 <- polity[polity$year>=1960,3:ncol(polity)]

polity2$country <- as.character(polity2$country)
polity2$country[polity2$country=='UAE'] <- 'United Arab Emirates'
polity2$country[polity2$country=='Congo Brazzaville'] <- 'Congo, Republic of'
polity2$country[polity2$country=='Congo Kinshasa'] <- 'Congo, Democratic Republic of'
polity2$country[polity2$country=='Germany East'] <- "Germany Democratic Republic"
polity2$cname <- countrycode(polity2$country, 'country.name', 'country.name')
polity2$cname[polity2$country=='Yemen South'] <- "S. YEMEN"
polity2$cname[polity2$country=='Vietnam South'] <- "S. VIETNAM"
polity2[polity2$cname=='Yugoslavia', 'cname'] <- 'SERBIA'
polity2[polity2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

polity2$cnameYear <- paste(polity2$cname, polity2$year, sep='')

polity2$drop <- 0
polity2[polity2$scode=='ETH' & polity2$year==1993, 'drop'] <- 1
polity2[polity2$scode=='GMY' & polity2$year==1990, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==1991, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==2006, 'drop'] <- 1
polity2[polity2$scode=='SDN' & polity2$year==2011, 'drop'] <- 1
polity2[polity2$scode=='DRV' & polity2$year==1976, 'drop'] <- 1
polity2[polity2$scode=='YAR' & polity2$year==1990, 'drop'] <- 1
polity2 <- polity2[polity2$drop==0,]; polity2 <- polity2[,1:(ncol(polity2)-1)]

names(table(polity2$cnameYear)[table(polity2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
polity2$ccode <- panel$ccode[match(polity2$cname,panel$cname)]
polity2$cyear <- paste(polity2$ccode, polity2$year, sep='')
table(polity2$cyear)[table(polity2$cyear)>1] # Dupe check
###############################################################

###############################################################
# ICRG data from PRS group
icrg2 <- icrg

icrg2$Country <- as.character(icrg$Country)
icrg2$Country[icrg2$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
icrg2$Country[icrg2$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'
drop <- c("Hong Kong", "New Caledonia")
icrg2 <- icrg2[which(!icrg2$Country %in% drop),]
icrg2$cname <- countrycode(icrg2$Country, 'country.name', 'country.name')
icrg2[icrg2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

icrg2$cnameYear <- paste(icrg2$cname, icrg2$Year, sep='')

icrg2$drop <- 0
icrg2[icrg2$Country=='Serbia and Montenegro' & icrg2$Year>=2006, 'drop'] <- 1
icrg2[icrg2$Country=='Serbia' & icrg2$Year<2006, 'drop'] <- 1
icrg2[icrg2$Country=='Czechoslovakia' & icrg2$Year>=1993, 'drop'] <- 1
icrg2[icrg2$Country=='Czech Republic' & icrg2$Year<1993, 'drop'] <- 1
icrg2 <- icrg2[icrg2$drop==0,]; icrg2 <- icrg2[,1:(ncol(icrg2)-1)]

table(icrg2$cnameYear)[table(icrg2$cnameYear)>1]

# Adding in codes from panel
icrg2$ccode <- panel$ccode[match(icrg2$cname,panel$cname)]
icrg2$cyear <- paste(icrg2$ccode, icrg2$Year, sep='')
table(icrg2$cyear)[table(icrg2$cyear)>1] # Dupe check
###############################################################

###############################################################
# heritage
heritage2 <- heritage

heritage2$name <- as.character(trim(heritage2$name))

drop <- c("Hong Kong", "Macau" )
heritage2 <- heritage2[which(!heritage2$name %in% drop),]

heritage2$cname <- countrycode(heritage2$name, 'country.name', 'country.name')

heritage2$cnameYear <- paste(heritage2$cname, heritage2$index.year, sep='')

table(heritage2$cnameYear)[table(heritage2$cnameYear)>1] # Dupe check

# Adding in codes from panel
heritage2$ccode <- panel$ccode[match(heritage2$cname,panel$cname)]
heritage2$cyear <- paste(heritage2$ccode, heritage2$index.year, sep='')
table(heritage2$cyear)[table(heritage2$cyear)>1] # Dupe check
###############################################################

###############################################################
# WGI
WGIregQual2 <- WGIregQual
colnames(WGIregQual2)[1:2] <- c('Country.Name','Country.Code')
data <- WGIregQual2; variable <- 'regQual'
WGIregQual2Clean <- cleanWbData(WGIregQual2, 'regQual')

WGIregQual2Clean$Country.Name[WGIregQual2Clean$Country.Name=="S\355O TOM\304 AND PRINCIPE"] <- 'Sao Tome'
WGIregQual2Clean$Country.Name[WGIregQual2Clean$Country.Name=="KOREA, DEM. REP."] <- 'North Korea' 
WGIregQual2Clean$Country.Name[WGIregQual2Clean$Country.Name=="KOREA, REP."] <- 'South Korea' 

WGIregQual2Clean$cname <- countrycode(WGIregQual2Clean$Country.Name, 'country.name', 'country.name')

drop <- unique(WGIregQual2Clean[which(WGIregQual2Clean$cname %in% setdiff(WGIregQual2Clean$cname, panel$cname)), 'Country.Name'])
WGIregQual2Clean <- WGIregQual2Clean[which(!WGIregQual2Clean$Country.Name %in% drop),]

WGIregQual2Clean$cnameYear <- paste(WGIregQual2Clean$cname, WGIregQual2Clean$year, sep='')

table(WGIregQual2Clean$cnameYear)[table(WGIregQual2Clean$cnameYear)>1] # Dupe check

# Adding in codes from panel
WGIregQual2Clean$ccode <- panel$ccode[match(WGIregQual2Clean$cname,panel$cname)]
WGIregQual2Clean$cyear <- paste(WGIregQual2Clean$ccode, WGIregQual2Clean$year, sep='')
table(WGIregQual2Clean$cyear)[table(WGIregQual2Clean$cyear)>1] # Dupe check
###############################################################

###############################################################
# Fraser, starts annually at 2000
fraser2 <- fraser[7:length(fraser)]
allVars <- lapply(fraser2, function(x) FUN=colnames(x))
vars <- allVars[[1]]
for(ii in 2:length(allVars)){ vars <- intersect(vars, allVars[[ii]]) }
finVars <- vars[which(!vars %in% append('X', paste('X', 1:11, sep='.')))]

fraser3 <- NULL
for(ii in 1:length(fraser2)){
	slice <- fraser2[[ii]]
	slice <- slice[,finVars]
	slice <- cbind(slice, year=names(fraser2)[ii])
	fraser3 <- rbind(fraser3,slice) }

fraser3$Countries <- as.character(fraser3$Countries)
fraser3[fraser3$Countries=='Pap. New Guinea','Countries'] <- 'PAPUA NEW GUINEA'
fraser3[fraser3$Countries=='Unit. Arab Em.','Countries'] <- 'UNITED ARAB EMIRATES'

fraser3$cname <- countrycode(fraser3$Countries, 'country.name', 'country.name')

drop <- unique(fraser3[which(fraser3$cname %in% setdiff(fraser3$cname, panel$cname)), 'Countries'])
fraser3 <- fraser3[which(!fraser3$Countries %in% drop),]

fraser3$cnameYear <- paste(fraser3$cname, fraser3$year, sep='')

table(fraser3$cnameYear)[table(fraser3$cnameYear)>1] # Dupe check

# Adding in codes from panel
fraser3$ccode <- panel$ccode[match(fraser3$cname,panel$cname)]
fraser3$cyear <- paste(fraser3$ccode, fraser3$year, sep='')
table(fraser3$cyear)[table(fraser3$cyear)>1] # Dupe check
###############################################################

###############################################################
# Disputes
disputes2 <- disputes

disputes2$cname <- countrycode(disputes2$Country, 'country.name', 'country.name')
disputes2$cname[disputes2$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'

drop <- unique(disputes2[which(disputes2$cname %in% setdiff(disputes2$cname, panel$cname)), 'Country'])
disputes2 <- disputes2[which(!disputes2$Country %in% drop),]

disputes2$cnameYear <- paste(disputes2$cname, disputes2$Year, sep='')

table(disputes2$cnameYear)[table(disputes2$cnameYear)>1] # Dupe check

# Adding in codes from panel
disputes2$ccode <- panel$ccode[match(disputes2$cname,panel$cname)]
disputes2$cyear <- paste(disputes2$ccode, disputes2$Year, sep='')
table(disputes2$cyear)[table(disputes2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Reputation Dataset
karenReput2 <- karenReput

karenReput2$cname <- countrycode(karenReput2$NationSM, 'country.name', 'country.name')
karenReput2$cname[karenReput2$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
karenReput2$cname[karenReput2$cname=='Yugoslavia'] <- 'SERBIA'

karenReput2$cnameYear <- paste(karenReput2$cname, karenReput2$Year, sep='')

table(karenReput2$cnameYear)[table(karenReput2$cnameYear)>1] # Dupe check
karenReput2 <- karenReput2[!is.na(karenReput2$cname),]

# Adding in codes from panel
karenReput2$ccode <- panel$ccode[match(karenReput2$cname,panel$cname)]
karenReput2$cyear <- paste(karenReput2$ccode, karenReput2$Year, sep='')
table(karenReput2$cyear)[table(karenReput2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Privatization dataset
privatization2 <- privatization

privatization2$cname <- countrycode(privatization2$NationSM, 'country.name', 'country.name')
privatization2$cname[privatization2$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
privatization2$cname[privatization2$cname=='Yugoslavia'] <- 'SERBIA'

privatization2$cnameYear <- paste(privatization2$cname, privatization2$Year, sep='')

privatization2 <- unique(privatization2) # Angola 1980 is entered twice

names(table(privatization2$cnameYear)[table(privatization2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
privatization2$ccode <- panel$ccode[match(privatization2$cname,panel$cname)]
privatization2$cyear <- paste(privatization2$ccode, privatization2$Year, sep='')
table(privatization2$cyear)[table(privatization2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Wright Expropriation Dataset
wrightExprop2 <- wrightExprop

wrightExprop2$ctryname[wrightExprop2$ctryname=='Congo (Brazzaville)'] <- 'Congo, Republic of'
wrightExprop2$ctryname[wrightExprop2$ctryname=='Congo (Kinshasa)'] <- 'Congo, Democratic Republic of'
wrightExprop2$ctryname[wrightExprop2$ctryname=='Germany East (1945-1990)'] <- "Germany Democratic Republic"
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Belgium-Luxembourg',]

wrightExprop2$drop <- 0
wrightExprop2[wrightExprop2$ctryname=='Czechoslovakia' & wrightExprop2$year>=1993, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Czech Republic' & wrightExprop2$year<1993, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Ethiopia (-1992)' & wrightExprop2$year>=1993, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Ethiopia (1993+)' & wrightExprop2$year<1993, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Germany West (1945-1990)' & wrightExprop2$year>=1991, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Germany (-1945)' & wrightExprop2$year>=1945, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Pakistan (1972+)' & wrightExprop2$year<1972, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Yugoslavia (1918-1992)' & wrightExprop2$year>=1992, 'drop'] <- 1
wrightExprop2[wrightExprop2$ctryname=='Yemen Unified (1990+)' & wrightExprop2$year<1990, 'drop'] <- 1
wrightExprop2 <- wrightExprop2[wrightExprop2$drop==0,]; wrightExprop2 <- wrightExprop2[,1:(ncol(wrightExprop2)-1)]

wrightExprop2$cname <- countrycode(wrightExprop2$ctryname, 'country.name', 'country.name')
wrightExprop2$cname[wrightExprop2$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
wrightExprop2$cname[wrightExprop2$cname=='Yugoslavia'] <- 'SERBIA'

wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Danzig',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='French Equatorial Africa',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='French West Africa',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Newfoundland',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Saar',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Straits Settlements',]
wrightExprop2 <- wrightExprop2[wrightExprop2$ctryname!='Indochina',]

drop <- unique(wrightExprop2[which(wrightExprop2$cname %in% setdiff(wrightExprop2$cname, panel$cname)), 'ctryname'])
wrightExprop2 <- wrightExprop2[which(!wrightExprop2$ctryname %in% drop),]

wrightExprop2$cnameYear <- paste(wrightExprop2$cname, wrightExprop2$year, sep='')

names(table(wrightExprop2$cnameYear)[table(wrightExprop2$cnameYear)>1])

# Adding in codes from panel
wrightExprop2$ccode <- panel$ccode[match(wrightExprop2$cname,panel$cname)]
wrightExprop2$cyear <- paste(wrightExprop2$ccode, wrightExprop2$year, sep='')
table(wrightExprop2$cyear)[table(wrightExprop2$cyear)>1] # Dupe check
###############################################################

###############################################################
bitsReporter <- bits[,c('Reporter','ReporterClean','ccodeRep',
	'Year_Signature','Year_force','signedbitsSM', 'ratifiedbitsSM', 
	'PartnerClean', 'ccodePar')]
colnames(bitsReporter) <- c('Country', 'cname', 'ccode', 
	'yearSign', 'yearRat', 'signedbitsSM', 'ratifiedbitsSM','other','othercode')
bitsPartner <- bits[,c('Partner','PartnerClean','ccodePar',
	'Year_Signature','Year_force','signedbitsSM', 'ratifiedbitsSM', 
	'ReporterClean', 'ccodeRep')]
colnames(bitsPartner) <- c('Country', 'cname', 'ccode', 
	'yearSign', 'yearRat', 'signedbitsSM', 'ratifiedbitsSM','other','othercode')
bitsMelt <- data.frame(rbind(bitsReporter,bitsPartner))

bitsMelt$Country <- as.character(bitsMelt$Country)
bitsMelt$Country[bitsMelt$Country=='Congo, DR'] <- 'Congo, Democratic Republic of'
bitsMelt$Country[bitsMelt$Country=="Democratic People's Republic of Korea"] <- 'North Korea'
bitsMelt$Country[bitsMelt$Country=="S\355\243o Tom\355\251 and Principe"] <- 'Sao Tome'
bitsMelt$Country[bitsMelt$Country=="ghanistan"] <- 'Afghanistan'

bitsSigned <- unique(bitsMelt); bitsRatified <- unique(na.omit(bitsMelt))

bitsSigned$cname <- countrycode(bitsSigned$Country, 'country.name', 'country.name')
bitsSigned$cnameYear <- paste(bitsSigned$cname, bitsSigned$yearSign, sep='')
drop <- unique(bitsSigned[which(bitsSigned$cname %in% setdiff(bitsSigned$cname, panel$cname)), 'Country'])
bitsSigned <- bitsSigned[which(!bitsSigned$Country %in% drop),]
bitsSigned$ccode <- panel$ccode[match(bitsSigned$cname,panel$cname)]
bitsSigned$cyear <- paste(bitsSigned$ccode, bitsSigned$yearSign, sep='')
bitsSigned <- summaryBy(signedbitsSM ~ cyear, data=bitsSigned, FUN=(sum))
colnames(bitsSigned)[2] <- 'signedbitsSM'

bitsRatified$cname <- countrycode(bitsRatified$Country, 'country.name', 'country.name')
bitsRatified$cnameYear <- paste(bitsRatified$cname, bitsRatified$yearRat, sep='')
drop <- unique(bitsRatified[which(bitsRatified$cname %in% setdiff(bitsRatified$cname, panel$cname)), 'Country'])
bitsRatified <- bitsRatified[which(!bitsRatified$Country %in% drop),]
bitsRatified$ccode <- panel$ccode[match(bitsRatified$cname,panel$cname)]
bitsRatified$cyear <- paste(bitsRatified$ccode, bitsRatified$yearRat, sep='')
bitsRatified <- summaryBy(signedbitsSM ~ cyear, data=bitsRatified, FUN=(sum))
colnames(bitsRatified)[2] <- 'ratifiedbitsSM'	
###############################################################

###############################################################
# Combining data
setwd(pathData)
save(disputes2,fraser3, WGIregQual2Clean, heritage2,icrg2,
	polity2, wbData, kaopen2, karenReput2, wrightExprop2, kaopen2,
	bitsSigned, bitsRatified, constraints2, banks2, privatization2,
	file='cleanedData.rda')

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('cleanedData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

frame <- unique(panel[,c('ccode', 'cname')])
dframe <- NULL; frame$year <- NA; years <- seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year <- years[ii]; dframe <- rbind(dframe, frame) }
dframe$cyear <- paste(dframe$ccode, dframe$year, sep='')
dim(dframe)
combData <- merge(dframe, wbData[,c(4,8:ncol(wbData))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, WGIregQual2Clean[,c(4,ncol(WGIregQual2Clean))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, disputes2[,c(4:9,ncol(disputes2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, heritage2[,c(3:13,ncol(heritage2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, kaopen2[,c(3,ncol(kaopen2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, fraser3[,c(2:56,ncol(fraser3))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, polity2[,c(7:35,ncol(polity2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, icrg2[,c(5:16,ncol(icrg2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, karenReput2[,c(5:19,ncol(karenReput2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, wrightExprop2[,c(5:6,ncol(wrightExprop2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, banks2[,c(5:13,ncol(banks2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, privatization2[,c(5:16,ncol(privatization2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, constraints2[,c(8:10,ncol(constraints2))],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)

combData <- merge(combData, bitsSigned,by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData$signedbitsSM[is.na(combData$signedbitsSM)] <- 0

combData <- merge(combData, bitsRatified,by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData$ratifiedbitsSM[is.na(combData$ratifiedbitsSM)] <- 0


save(combData, file='combinedData.rda')
write.csv(combData, file='combinedData.csv')
###############################################################