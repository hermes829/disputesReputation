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
kaopen2 <- kaopen

kaopen2$country_name <- as.character(kaopen2$country_name)
kaopen2$country_name[kaopen2$country_name=="S? Tom\341and Principe"] <- 'Sao Tome'
kaopen2$country_name[kaopen2$country_name=="C?e d'Ivoire"] <- 'Ivory Coast'
drop <- c("Aruba", "Netherlands Antilles", "Hong Kong, China")
kaopen2 <- kaopen2[which(!kaopen2$country_name %in% drop),]
kaopen2$cname <- countrycode(kaopen2$country_name, 'country.name', 'country.name')

kaopen2$cnameYear <- paste(kaopen2$cname, kaopen2$year, sep='')

table(kaopen2$cnameYear)[kaopen2$cnameYear>1] # Dupe check

# Adding in codes from panel
kaopen2$ccode <- panel$ccode[match(kaopen2$cname,panel$cname)]
kaopen2$cyear <- paste(kaopen2$ccode, kaopen2$year, sep='')
table(kaopen2$cyear)[table(kaopen2$cyear)>1] # Dupe check
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
disputes$cname <- countrycode(disputes$Country, 'country.name', 'country.name')
disputes$ccode <- countrycode(disputes$cname, 'country.name', 'cown')

# Manual corrections
disputes[disputes$Country=='Korea, North','ccode'] <- 731
disputes[disputes$Country=='Serbia','ccode'] <- 345
disputes[disputes$Country=='Aruba','ccode'] <- 1000
disputes[disputes$Country=='Bermuda','ccode'] <- 1001
disputes[disputes$Country=='Cayman Islands','ccode'] <- 1002
disputes[disputes$Country=='Czechoslovachia','ccode'] <- 315
disputes[disputes$Country=='Faeroe Islands','ccode'] <- 1005
disputes[disputes$Country=='Greenland','ccode'] <- 1007
disputes[disputes$Country=='Hong Kong','ccode'] <- 1009
disputes[disputes$Country=='Macao, China','ccode'] <- 1011
disputes <- disputes[disputes$Country!='Netherlands Antilles',]
disputes[disputes$Country=='New Caledonia','ccode'] <- 1012
disputes[disputes$Country=='Congo-Brazzaville','ccode'] <- 484
disputes[disputes$Country=='Congo-Kinshasa','ccode'] <- 490
disputes <- disputes[disputes$Country!='Zaire',]
# unique(disputes[is.na(disputes$ccode),c(2,ncol(disputes))])

disputes$cyear <- 
	as.numeric(as.character(
		paste(disputes$ccode, disputes$Year, sep='')))

# Correcting duplicates
# multiples <- names(table(disputes$cyear)[table(disputes$cyear)>1])
###############################################################

###############################################################
# Reputation Dataset
# Correcting country names
temp <- unique(karenReput[,c('Nation', 'Refno')])
temp <- temp[order(temp$Nation,decreasing=F),]
temp2 <- temp[(which(temp$Nation %in% '.')+1):nrow(temp),]
multiples <- names(table(temp2$Refno)[table(temp2$Refno)>1])
temp2[which(temp2$Refno %in% multiples),]
temp2 <- temp2[temp2$Nation!='CAmeroon',]
temp2 <- temp2[temp2$Nation!='Czeckoslovakia',]
temp2 <- temp2[temp2$Nation!='EAst Timor',]
temp2 <- temp2[temp2$Nation!='East Timur',]
temp2 <- temp2[temp2$Nation!='Servia',]
temp2 <- temp2[temp2$Nation!='Yugoslavia',]
colnames(temp2) <- c('cname', 'Refno')
karenReput <- merge(karenReput, temp2, by='Refno', all.x=T, all.y=F)

karenReput$ccode <- countrycode(karenReput$cname, 'country.name', 'cown')

# unique(karenReput[is.na(karenReput$ccode),'cname'])

karenReput <- karenReput[!is.na(karenReput$Refno),]
karenReput[karenReput$cname=='Korea, North','ccode'] <- 731
karenReput[karenReput$cname=='Serbia','ccode'] <- 345

karenReput$cyear <- 
	as.numeric(as.character(
		paste(karenReput$ccode, karenReput$Year, sep='')))

# multiples <- names(table(karenReput$cyear)[table(karenReput$cyear)>1])
###############################################################

###############################################################
# Wright Expropriation Dataset
wrightExprop$ccode <- countrycode(wrightExprop$ctryname, 
	'country.name', 'cown')

wrightExprop[wrightExprop$ctryname=='Cook Islands','ccode'] <- 1023
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Danzig',]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='French Equatorial Africa',]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='French West Africa',]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Guadeloupe',]
wrightExprop[wrightExprop$ctryname=='Hong Kong','ccode'] <- 1009
wrightExprop[wrightExprop$ctryname=='Korea North (1949+)','ccode'] <- 731
wrightExprop[wrightExprop$ctryname=='Macao','ccode'] <- 1011
wrightExprop[wrightExprop$ctryname=='Martinique','ccode'] <- 1026
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Montserrat',]
wrightExprop[wrightExprop$ctryname=='New Caledonia','ccode'] <- 1012
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Newfoundland',]
wrightExprop[wrightExprop$ctryname=='Palestine (-1947)','ccode'] <- 1020
wrightExprop[wrightExprop$ctryname=='Puerto Rico','ccode'] <- 1014
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Rhodesia and Nyasaland Federation',]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Saar',]
wrightExprop[wrightExprop$ctryname=='Serbia and Montenegro (1992+)','ccode'] <- 345
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Straits Settlements',]

# unique(wrightExprop[is.na(wrightExprop$ccode), 'ctryname'])

wrightExprop <- wrightExprop[wrightExprop$ctryname!='Belgium-Luxembourg',]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Germany (-1945)',]
wrightExprop[wrightExprop$ctryname=='Germany East (1945-1990)','ccode'] <- 265
drop <- which(wrightExprop$ctryname=='Germany West (1945-1990)' & wrightExprop$year==1991)
wrightExprop <- wrightExprop[c(1:(drop-1), (drop+1):nrow(wrightExprop)),]
drop <- which(wrightExprop$ctryname=='Yugoslavia (1918-1992)' & wrightExprop$year==1992)
wrightExprop <- wrightExprop[c(1:(drop-1), (drop+1):nrow(wrightExprop)),]
wrightExprop[wrightExprop$ctryname=='Congo (Brazzaville)','ccode'] <- 484
wrightExprop[wrightExprop$ctryname=='Congo (Kinshasa)','ccode'] <- 490
drop <- which(wrightExprop$ctryname=='Ethiopia (1993+)' & wrightExprop$year<1993)
wrightExprop <- wrightExprop[c(1:(drop[1]-1), (drop[length(drop)]+1):nrow(wrightExprop)),]
drop <- which(wrightExprop$ctryname=='Yemen Unified (1990+)' & wrightExprop$year<1990)
wrightExprop <- wrightExprop[c(1:(drop[1]-1), (drop[length(drop)]+1):nrow(wrightExprop)),]
wrightExprop <- wrightExprop[wrightExprop$ctryname!='Indochina',]
drop <- which(wrightExprop$ctryname=='Pakistan (1972+)' & wrightExprop$year<1972)
wrightExprop <- wrightExprop[c(1:(drop[1]-1), (drop[length(drop)]+1):nrow(wrightExprop)),]

wrightExprop$cyear <- 
	as.numeric(as.character(
		paste(wrightExprop$ccode, wrightExprop$year, sep='')))
# multiples <- names(table(wrightExprop$cyear)[table(wrightExprop$cyear)>1])
# temp <- unique(wrightExprop[which(wrightExprop$cyear %in% multiples), c('ctryname', 'ccode')])
# temp[order(temp$ccode),]
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

bitsSigned <- unique(bitsMelt); bitsRatified <- unique(na.omit(bitsMelt))
bitsSigned$cyear <- 
	as.numeric(as.character(
		paste(bitsSigned$ccode, bitsSigned$yearSign, sep='')))
bitsSigned <- summaryBy(signedbitsSM ~ cyear, data=bitsSigned, FUN=(sum))
colnames(bitsSigned)[2] <- 'signedbitsSM'
bitsRatified$cyear <- 
	as.numeric(as.character(
		paste(bitsRatified$ccode, bitsRatified$yearRat, sep='')))
bitsRatified <- summaryBy(ratifiedbitsSM ~ cyear, data=bitsRatified, FUN=(sum))
colnames(bitsRatified)[2] <- 'ratifiedbitsSM'	
###############################################################

###############################################################
# Combining data
setwd(pathData)
save(disputes,fraser3, WGIregQualClean, heritage,icrg,
	polity2, wbData, kaopen, karenReput, wrightExprop, 
	bitsSigned, bitsRatified, file='cleanedData.rda')

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')
setwd(pathData)
load('cleanedData.rda')

frame <- na.omit(cbind(1:999, countrycode(1:999,'cown','country.name')))
frame <- rbind(frame, matrix(c('731', 'NORTH KOREA'),nrow=1,ncol=2,byrow=T))
frame <- data.frame(frame)
frame$X1 <- as.numeric(as.character(frame$X1))
colnames(frame) <- c('ccode', 'cname')

dframe <- NULL; frame$year <- NA; years <- seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year <- years[ii]; dframe <- rbind(dframe, frame) }
dframe$cyear <- paste(dframe$ccode, dframe$year, sep='')

combData <- merge(dframe, wbData[,c(7,6,8:12)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, WGIregQualClean[,6:7],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, disputes[,c(4:9,12)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, heritage[,c(3:13,15)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, kaopen[,c(3,7)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, fraser3[,c(2:56,59)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, polity2[,c(7:36)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, icrg[,c(5:16,18)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, karenReput[,c(4:14,16:23,26)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, wrightExprop[,c(5,12)],by='cyear',all.x=T,all.y=F)
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