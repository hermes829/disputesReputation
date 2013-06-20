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
	# mdata2[mdata2$Country.Name=='Serbia','ccode'] <- 1015
	mdata2[mdata2$Country.Name=='Serbia','ccode'] <- 345
	mdata2[mdata2$Country.Name=='Sint Maarten (Dutch part)','ccode'] <- 1016
	mdata2[mdata2$Country.Name=='St. Martin (French part)','ccode'] <- 1017
	mdata2[mdata2$Country.Name=='Turks and Caicos Islands','ccode'] <- 1018
	mdata2[mdata2$Country.Name=='Virgin Islands (U.S.)','ccode'] <- 1019
	mdata2[mdata2$Country.Name=='West Bank and Gaza','ccode'] <- 1020
	mdata2[mdata2$Country.Name=='American Samoa','ccode'] <- 1021
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
kaopen$cyear <- as.numeric(as.character(
	paste(kaopen$ccode, kaopen$year, sep='')))
###############################################################

###############################################################
# Polity
polity2 <- polity[polity$year>=1960,3:ncol(polity)]
polity2$ccode <- countrycode(polity2$country, 'country.name', 'cown') 

# Manual Corrections
# unique(polity2[which(is.na(polity2$ccode)),1:3])
polity2[polity2$country=="Korea North", 'ccode'] <- 731
polity2[polity2$country=="Serbia", 'ccode'] <- 1015
polity2[polity2$country=="UAE", 'ccode'] <- 696
polity2[polity2$country=="Serbia and Montenegro", 'ccode'] <- 345
polity2[polity2$country=="Germany East", 'ccode'] <- 265
polity2[polity2$country=="Congo Kinshasa", 'ccode'] <- 490

polity2$cyear <- as.numeric(as.character(
	paste(polity2$ccode, polity2$year, sep='')))

# Need to remove duplicate cases
polity2$temp <- 1:nrow(polity2)
# N. Vietnam 1976 and Vietnam repeat 1976
# Ethiopia repeat 1993
# W Germany and Germany repeat 1990
# Drop S. Vietnam between 1960 to 1975
# Drop Sudan repeat 2011
# Drop South Yemen
multiples <- names(table(polity2$cyear)[table(polity2$cyear)>1])
temp <- unique(polity2[which(polity2$cyear %in% multiples), c(1:4,10,ncol(polity2))])
rownames(temp) <- 1:nrow(temp)
# temp
# Choose cases to drop
drop <- temp[c(17,18,20,22:37,38,64,66,67:90),'temp']
polity2 <- polity2[which(!polity2$temp %in% drop),]
polity2 <- polity2[,1:36]
###############################################################

###############################################################
# ICRG data from PRS group
icrg$ccode <- countrycode(icrg$Country, 'country.name', 'cown')

# Manual corrections
# unique(icrg[is.na(icrg$ccode),1:3])
icrg[icrg$Country=='Hong Kong', 'ccode'] <- 1009
icrg[icrg$Country=='Korea, North', 'ccode'] <- 731
icrg[icrg$Country=='New Caledonia', 'ccode'] <- 1012
icrg[icrg$Country=='Serbia and Montenegro', 'ccode'] <- 345
icrg[icrg$Country=='Congo-Brazzaville', 'ccode'] <- 484
icrg[icrg$Country=='Congo-Kinshasa', 'ccode'] <- 490
icrg <- icrg[which(!icrg$Country %in% 'Serbia'),] # Drop Serbia
icrg <- icrg[which(!is.na(icrg$ccode)),] # Dropping extra NA cases

icrg$cyear <- as.numeric(as.character(
	paste(icrg$ccode, icrg$Year, sep='')))

# Remove duplicate cases
# icrg$temp <- 1:nrow(icrg)
# multiples <- names(table(icrg$cyear)[table(icrg$cyear)>1])
# temp <- unique(icrg[which(icrg$cyear %in% multiples), c(2,4,17,18,19)])
# rownames(temp) <- 1:nrow(temp)
# temp[order(temp$ccode),]
###############################################################

###############################################################
# heritage
heritage$ccode <- countrycode(heritage$name, 'country.name', 'cown')

# Manual corrections
# temp <- unique(heritage[,c(1,ncol(heritage))]); temp[is.na(temp$ccode),]
heritage[heritage$name=='Hong Kong','ccode'] <- 1009
heritage[heritage$name=='Macau','ccode'] <- 1011
heritage[heritage$name=='North Korea','ccode'] <- 731
heritage[heritage$name=='Serbia ','ccode'] <- 345
heritage[heritage$name=='Serbia','ccode'] <- 345

heritage$cyear <- as.numeric(as.character(
	paste(heritage$ccode, heritage$index.year, sep='')))
###############################################################

###############################################################
# WGI
colnames(WGIregQual)[1:2] <- c('Country.Name','Country.Code')
data <- WGIregQual; variable <- 'regQual'
WGIregQualClean <- cleanWbData(WGIregQual, 'regQual')

# Manual corrections
# temp <- WGIregQualClean[is.na(WGIregQualClean$ccode),3:4];unique(temp)
WGIregQualClean[WGIregQualClean$cname=='AMERICAN SAMOA','ccode'] <- 1021
WGIregQualClean[WGIregQualClean$cname=='ANGUILLA','ccode'] <- 1022
WGIregQualClean[WGIregQualClean$cname=='ARUBA','ccode'] <- 1000
WGIregQualClean[WGIregQualClean$cname=='BERMUDA','ccode'] <- 1001
WGIregQualClean[WGIregQualClean$cname=='CAYMAN ISLANDS','ccode'] <- 1002
WGIregQualClean[WGIregQualClean$cname=='COOK ISLANDS','ccode'] <- 1023
WGIregQualClean[WGIregQualClean$cname=='FRENCH GUIANA','ccode'] <- 1024
WGIregQualClean[WGIregQualClean$cname=='GREENLAND','ccode'] <- 1007
WGIregQualClean[WGIregQualClean$cname=='GUAM','ccode'] <- 1008
WGIregQualClean[WGIregQualClean$cname=='HONG KONG','ccode'] <- 1009
WGIregQualClean[WGIregQualClean$cname=='JERSEY','ccode'] <- 1025
WGIregQualClean[WGIregQualClean$cname=="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",'ccode'] <- 731
WGIregQualClean[WGIregQualClean$cname=='MACAO','ccode'] <- 1011
WGIregQualClean[WGIregQualClean$cname=='MARTINIQUE','ccode'] <- 1026
WGIregQualClean <- WGIregQualClean[which(WGIregQualClean$cname!='NETHERLANDS ANTILLES'),]
WGIregQualClean[WGIregQualClean$cname=='NEW CALEDONIA','ccode'] <- 1012
WGIregQualClean <- WGIregQualClean[which(WGIregQualClean$cname!='NIUE'),]
WGIregQualClean[WGIregQualClean$cname=='PUERTO RICO','ccode'] <- 1014
WGIregQualClean <- WGIregQualClean[which(WGIregQualClean$cname!='REUNION'),]
WGIregQualClean[WGIregQualClean$cname=='SERBIA','ccode'] <- 345
WGIregQualClean[WGIregQualClean$cname=='VIRGIN ISLANDS, U.S.','ccode'] <- 1019
WGIregQualClean[WGIregQualClean$cname=='PALESTINIAN TERRITORY, OCCUPIED','ccode'] <- 1020
# temp <- WGIregQualClean[is.na(WGIregQualClean$ccode),3:4];unique(temp)

WGIregQualClean$cyear <- 
	as.numeric(as.character(
		paste(WGIregQualClean$ccode, WGIregQualClean$year, sep='')))
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
	fraser3 <- rbind(fraser3,slice) }

fraser3$cname <- countrycode(fraser3$Countries, 'country.name', 'country.name')
fraser3$ccode <- countrycode(fraser3$cname, 'country.name', 'cown')

# Manual corrections
fraser3[fraser3$Countries=='Pap. New Guinea','cname'] <- 'PAPUA NEW GUINEA'
fraser3[fraser3$Countries=='Pap. New Guinea','ccode'] <- 910
fraser3[fraser3$Countries=='Unit. Arab Em.','cname'] <- 'UNITED ARAB EMIRATES'
fraser3[fraser3$Countries=='Unit. Arab Em.','ccode'] <- 696
fraser3[fraser3$Countries=='Hong Kong','ccode'] <- 1009
fraser3[fraser3$Countries=='Serbia','ccode'] <- 345
fraser3[fraser3$cname=='SERBIA','ccode'] <- 345
# unique(fraser3[is.na(fraser3$ccode), c('Countries', 'cname', 'ccode')])

fraser3$cyear <- 
	as.numeric(as.character(
		paste(fraser3$ccode, fraser3$cyear, sep='')))
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