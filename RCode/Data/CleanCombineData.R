### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### Load data
setwd(pathData)
load('allData.rda')

# countrycode
write.csv(na.omit(cbind(
	1:999,
	countrycode(1:999,'cown','country.name'))),
	file='cowcodes_countrynames.csv')

###############################################################
# Organizing WB data
### Fx for Melting/Cleaning WB Data for Merge
cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations
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
		'World')
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# WB abbreviations vary for some measures
	mdata$Country.Code <- as.character(mdata$Country.Code)
	mdata[mdata$Country.Name=='Andorra','Country.Code'] <- 'ADO'
	mdata[mdata$Country.Name=='Congo, Dem. Rep.','Country.Code'] <- 'ZAR'
	mdata[mdata$Country.Name=='Isle of Man','Country.Code'] <- 'IMY'
	mdata[mdata$Country.Name=='Romania','Country.Code'] <- 'ROM'
	mdata[mdata$Country.Name=='Timor-Leste','Country.Code'] <- 'TMP'
	mdata[mdata$Country.Name=='West Bank and Gaza','Country.Code'] <- 'WBG'

	# Incorp common IDs from countrycode
	mdata2 <- cbind(mdata[,1:2], 
		cname=countrycode(mdata$Country.Code, 'wb', 'country.name'),
		ccode=countrycode(mdata$Country.Code, 'wb', 'cown'),
		mdata[,3:4])

	# Manually adding ccodes for select countries
	mdata2[mdata2$Country.Name=='Korea, Dem. Rep.','ccode'] <- 731
	mdata2[mdata2$Country.Name=='American Samoa','ccode'] <- 990
	mdata2[mdata2$Country.Name=='Aruba','ccode'] <- 1000
	mdata2[mdata2$Country.Name=='Bermuda','ccode'] <- 1001
	mdata2[mdata2$Country.Name=='Cayman Islands','ccode'] <- 1002
	mdata2[mdata2$Country.Name=='Channel Islands','ccode'] <- 1003
	mdata2$cname <- as.character(mdata2$cname)
	mdata2[mdata2$Country.Name=='Channel Islands','cname'] <- 'CHANNEL ISLANDS'
	mdata2[mdata2$Country.Name=='Curacao','ccode'] <- 1004
	mdata2[mdata2$Country.Name=='Faeroe Islands','ccode'] <- 1005
	mdata2[mdata2$Country.Name=='French Polynesia','ccode'] <- 1006
	mdata2[mdata2$Country.Name=='Greenland','ccode'] <- 1007
	mdata2[mdata2$Country.Name=='Guam','ccode'] <- 1008
	mdata2[mdata2$Country.Name=='Hong Kong SAR, China','ccode'] <- 1009
	mdata2[mdata2$Country.Name=='Isle of Man','ccode'] <- 1010
	mdata2[mdata2$Country.Name=='Macao SAR, China','ccode'] <- 1011
	mdata2[mdata2$Country.Name=='New Caledonia','ccode'] <- 1012
	mdata2[mdata2$Country.Name=='Northern Mariana Islands','ccode'] <- 1013
	mdata2[mdata2$Country.Name=='Puerto Rico','ccode'] <- 1014
	mdata2[mdata2$Country.Name=='Serbia','ccode'] <- 1015
	mdata2[mdata2$Country.Name=='Sint Maarten (Dutch part)','ccode'] <- 1016
	mdata2[mdata2$Country.Name=='St. Martin (French part)','ccode'] <- 1017
	mdata2[mdata2$Country.Name=='Turks and Caicos Islands','ccode'] <- 1018
	mdata2[mdata2$Country.Name=='Virgin Islands (U.S.)','ccode'] <- 1019
	mdata2[mdata2$Country.Name=='West Bank and Gaza','ccode'] <- 1020
	mdata2 }

WBinflDeflatorClean <- cleanWbData(WBinflDeflator, 'inflDeflator')
WBinflDeflatorClean$cyear <- 
	as.numeric(as.character(
			paste(WBinflDeflatorClean$ccode, WBinflDeflatorClean$year, sep='')))

WBgdpDeflatorClean <- cleanWbData(WBgdpDeflator, 'gdpDeflator')
WBgdpDeflatorClean$cyear <- 
	as.numeric(as.character(
		paste(WBgdpDeflatorClean$ccode, WBgdpDeflatorClean$year, sep='')))

WBfdiClean <- cleanWbData(WBfdi, 'fdi')
WBfdiClean$cyear <- 
	as.numeric(as.character(
		paste(WBfdiClean$ccode, WBfdiClean$year, sep='')))

WBfdiGdpClean <- cleanWbData(WBfdiGdp, 'fdiGDP')
WBfdiGdpClean$cyear <- 
	as.numeric(as.character(
		paste(WBfdiGdpClean$ccode, WBfdiGdpClean$year, sep='')))

WBgdpClean <- cleanWbData(WBgdp, 'gdp')
WBgdpClean$cyear <- 
	as.numeric(as.character(
		paste(WBgdpClean$ccode, WBgdpClean$year, sep='')))

WBgdpCapClean <- cleanWbData(WBgdpCap, 'gdpCAP')
WBgdpCapClean$cyear <- 
	as.numeric(as.character(
		paste(WBgdpCapClean$ccode, WBgdpCapClean$year, sep='')))

# Make sure order matches
sum(WBinflDeflatorClean$cyear!=WBgdpDeflatorClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBfdiClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBfdiGdpClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBgdpClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBgdpCapClean$cyear)

# combine data
setwd(pathData)
wbData <- data.frame(cbind(WBinflDeflatorClean,
	gdpDeflator=WBgdpDeflatorClean[,6], fdi=WBfdiClean[,6],
	fdiGdp=WBfdiGdpClean[,6], gdp=WBgdpClean[,6],
	gdpCAP=WBgdpCapClean[,6]))
save(wbData, file='wbData.rda')
###############################################################

###############################################################
# kaopen (uses imf numeric codes, cn)
kaopen$stabb <- kaopen$ccode
kaopen$ccode <- countrycode(kaopen$cn, 'imf', 'cown')
kaopen[kaopen$cn==314,'ccode'] <- 1000 # Aruba
kaopen <- kaopen[which(!kaopen$cn %in% 353),] # Drop Netherland Antilles
kaopen[kaopen$cn==532,'ccode'] <- 1009 # Hong Kong
kaopen[kaopen$cn==728,'ccode'] <- 565 # Namibia
kaopen$cyear <- paste(kaopen$ccode, kaopen$year, sep='')
###############################################################

###############################################################
# Polity

###############################################################