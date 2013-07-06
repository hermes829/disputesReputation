### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

### Load data
setwd(pathData)
list.files()
setwd(paste(pathData, '/Components', sep=''))
list.files()
setwd(paste(pathData, '/Components', '/Controls', sep=''))
list.files()

WBinflDeflator <- read.csv('NY.GDP.DEFL.KD.ZG_Indicator_MetaData_en_EXCEL.csv')
WBgdpDeflator <- read.csv('NY.GDP.DEFL.ZS_Indicator_MetaData_en_EXCEL.csv')
WBgdp <- read.csv('NY.GDP.MKTP.CD_Indicator_MetaData_en_EXCEL.csv')
WBgdpCap <- read.csv('NY.GDP.PCAP.CD_Indicator_MetaData_en_EXCEL.csv')
WBpop <- read.csv('SP.POP.TOTL_Indicator_MetaData_en_EXCEL.csv')
WBdebt <- read.csv('GC.DOD.TOTL.GD.ZS_Indicator_MetaData_en_EXCEL.csv')
kaopen <- read.csv('kaopen_2011.csv')
polity <- read.csv('p4v2011.csv')
constraints <- read.dta('polcon2012.dta')
privatization <- read.csv('privatizationData.csv')
banks <- read.csv('CNTSDATA.csv')

setwd(paste(pathData, '/Components', '/Disputes', sep=''))
list.files()
disputes <- read.csv('Dispute_Data.csv')
disputes$Country <- as.character(disputes$Country)
disputes$Country[disputes$Country=='Czechoslovachia'] <- 'Czechoslovakia'
disputes$Country[disputes$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
disputes$Country[disputes$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'
# Czech Rep not in existence until 1993 and Czecho out of existence after 1992
disputes$drop <- 0
disputes[disputes$Country=='Czech Republic' & disputes$Year<1993, 'drop'] <- 1
disputes[disputes$Country=='Czechoslovakia' & disputes$Year>1992, 'drop'] <- 1
# Dropping Zaire observations
disputes[disputes$Country=='Zaire', 'drop'] <- 1
# Dropping
disputes <- disputes[disputes$drop!=1,]

setwd(paste(pathData, '/Components', '/FDI', sep=''))
list.files()
WBfdi <- read.csv('BX.KLT.DINV.CD.WD_Indicator_MetaData_en_EXCEL.csv')
WBfdiGdp <- read.csv('BX.KLT.DINV.WD.GD.ZS_Indicator_MetaData_en_EXCEL.csv')

setwd(paste(pathData, '/Components', '/ReputationalMeasures/Fraser', sep=''))
list.files()
# snum <- sheetCount(list.files()[2])
# snames <- sheetNames(list.files()[2])
# fraser <- list()
# for(ii in 1:17){ fraser[[ii]] <- read.xls(list.files()[2],ii) }
# names(fraser) <- snames[1:17]
# save(fraser, file='fraser.rda')
load('fraser.rda')

setwd(paste(pathData, '/Components', '/ReputationalMeasures/Heritage', sep=''))
list.files()
heritage <- read.csv('data.csv')

setwd(paste(pathData, '/Components', '/ReputationalMeasures/ICRG', sep=''))
list.files()
icrg <- read.csv('PRS_Melted_Format.csv')

setwd(paste(pathData, '/Components', '/ReputationalMeasures/WGI', sep=''))
list.files()
WGIregQual <- read.csv('wgi_regQual.csv')

setwd(paste(pathData, '/Components', sep=''))
list.files()
# karenReput <- read.dta('ICSID_Reputation.dta')
karenReput <- read.dta('icsid_13.dta') # Updated dataset from Karen
karenReput <- karenReput[!is.na(karenReput$Refno),] 
karenReput$key <- paste(karenReput$Refno, karenReput$Nation, karenReput$Year, sep='')
temp <- read.dta("revised icsid.dta")
temp <- temp[!is.na(temp$Refno),] 
temp$key <- paste(temp$Refno, temp$Nation, temp$Year, sep='')
dim(karenReput)
karenReput <- merge(karenReput[,c(1:3,7:ncol(karenReput))], temp[,c(4:ncol(temp))], by='key', all.x=T)
dim(karenReput)
# Fixing mislabeling of Philippines 1971 case
karenReput$Nation <- trim(karenReput$Nation)
karenReput[karenReput$Refno==137 & karenReput$Nation=='Philippines' &
 karenReput$Year==1971,'Nation'] <- 'Poland'
# Removed one case where cunctad value was -6
karenReput[karenReput$cunctadcase==-6,] <- 0
# Czechoslovakia was misspelled
karenReput$Nation[karenReput$Nation=='Czeckoslovakia'] <- 'Czechoslovakia'
# Fixing missing country names in karenReput
temp <- na.omit(unique(karenReput[karenReput$Nation!="",c('Refno', 'Nation')]))
colnames(temp) <- c('Refno', 'NationSM')
mults <- names(table(temp$Refno)[table(temp$Refno)>1])
temp[which(temp$Refno %in% mults),]
temp <- temp[temp$NationSM!='CAmeroon' & temp$NationSM!='Servia' &
 temp$NationSM!='East Timur' & temp$NationSM!='EAst Timor',] 
# Merge cleaned nation variable back into original dataset
karenReput$NationSM <- temp$NationSM[match(karenReput$Refno,temp$Refno)]
# Fixing Czech Republic/Czechoslovakia (46) & Yugoslavia/Serbia rename (189)
karenReput[karenReput$Refno==46 & karenReput$Year>1992, 'NationSM'] <- 'Czech Republic'
karenReput[karenReput$Refno==189 & karenReput$Year>1989, 'NationSM'] <- 'Serbia'

wrightExprop <- read.dta('TomzWright2010.dta')
load('bits.rda')
bits$signedbitsSM <- 1
bits$ratifiedbitsSM <- ifelse(is.na(bits$Year_force), 0, 1)

setwd(pathData)
save(WBgdp, WBgdpCap, WBinflDeflator, WBgdpDeflator, WBpop, WBdebt,
	kaopen, privatization, constraints, banks,
	polity, disputes, WBfdi, WBfdiGdp, fraser, heritage, icrg, 
	WGIregQual, karenReput, wrightExprop, bits,
	 file='allData.rda')

# Comparing my disputes ™to karen's (disputes v karenReput)
temp <- karenReput[,c('Refno', 'NationSM', 'Year', 'icsidcase', 'settle', 'cunctadcase')]
temp$cname <- countrycode(temp$NationSM, 'country.name', 'country.name')
temp$cname[temp$cname=='Yugoslavia'] <- 'SERBIA'
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp[is.na(temp$cname),]
temp <- na.omit(temp)
temp$cnameYear <- paste(temp$cname, temp$Year, sep='')
temp$ccodeYear <- panel$ccodeYear[match(temp$cnameYear,panel$cnameYear)]
apply(temp[is.na(temp$ccodeYear),4:6],2,sum)
temp <- na.omit(temp)

disputes$cname <- countrycode(disputes$Country, 'country.name', 'country.name')
disputes$cname[disputes$cname=='Yugoslavia'] <- 'SERBIA'
disputes$cname[disputes$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
disputes[is.na(disputes$cname),]
disputes <- na.omit(disputes)
disputes$cnameYear <- paste(disputes$cname, disputes$Year, sep='')
disputes$ccodeYear <- panel$ccodeYear[match(disputes$cnameYear,panel$cnameYear)]
apply(disputes[is.na(disputes$ccodeYear),4:9],2,sum)
disputes <- na.omit(disputes)

dim(temp); dim(disputes)
masterDisp <- merge(
	disputes[,c('ccodeYear','Country','cname','Year','conc_disputes','pend_disputes')], 
	temp[,c('ccodeYear','NationSM','icsidcase','settle','cunctadcase')],
	by='ccodeYear', all.x=T, all.y=T)
dim(masterDisp)

masterDisp <- masterDisp[masterDisp$Year!=2012,]
masterDisp <- masterDisp[!is.na(masterDisp$Year),]
masterDisp <- na.omit(masterDisp)

mults <- names(table(masterDisp$ccodeYear)[table(masterDisp$ccodeYear)>1])
masterDisp[which(masterDisp$ccodeYear %in% mults),
 c('ccodeYear','Country', 'Year', 'conc_disputes','pend_disputes', 
 	'icsidcase', 'settle', 'cunctadcase', 'cp_disputes')]


# cumul icsidcase variable
temp <- masterDisp
temp$cicsidcase <- 0
countries <- unique(temp$cname)
years <- 1972:2011
fullData <- NULL
for(ii in 1:length(countries)){
	slice <- temp[temp$cname==countries[ii],
		c('ccodeYear','Year','icsidcase','cicsidcase')]
	years <- seq(min(slice$Year), max(slice$Year), 1)
	for(jj in 2:length(years)){
		slice[slice$Year==years[jj],'cicsidcase'] <- 
			slice[slice$Year==years[jj],'icsidcase'] + 
				slice[slice$Year==(years[jj]-1),'cicsidcase'] }
	fullData <- rbind(fullData, slice) 
	print(ii)}
dim(masterDisp); dim(fullData)
masterDisp <- merge(masterDisp, fullData[,c(1,4)], by='ccodeYear', all.x=T, all.y=F)
dim(masterDisp); dim(fullData)

# Comparing my disputes against karen
masterDisp$cp_disputes <- masterDisp$conc_disputes + masterDisp$pend_disputes
write.csv(masterDisp[,c('NationSM', 'Year', 'conc_disputes', 'pend_disputes', 'cp_disputes', 'icsidcase', 'cicsidcase')],file='temp2.csv')