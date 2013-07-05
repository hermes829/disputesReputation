### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

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
kaopen <- read.csv('kaopen_2011.csv')
polity <- read.csv('p4v2011.csv')

setwd(paste(pathData, '/Components', '/Disputes', sep=''))
list.files()
disputes <- read.csv('Dispute_Data.csv')
disputes$Country <- as.character(disputes$Country)
disputes$Country[disputes$Country=='Czechoslovachia'] <- 'Czechoslovakia'
disputes$Country[disputes$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
disputes$Country[disputes$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'

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
# Fixing mislabeling of Philippines 1971 case
karenReput$Nation <- trim(karenReput$Nation)
karenReput[karenReput$Refno==137 & karenReput$Nation=='Philippines' &
 karenReput$Year==1971,'Nation'] <- 'Poland'
# Czechoslovakia was misspelled
karenReput$Nation[karenReput$Nation=='Czeckoslovakia'] <- 'Czechoslovakia'
# Fixing missing country names in karenReput
temp <- na.omit(unique(karenReput[karenReput$Nation!="",c('Refno', 'Nation')]))
colnames(temp) <- c('Refno', 'NationSM')
mults <- names(table(temp$Refno)[table(temp$Refno)>1])
temp[which(temp$Refno %in% mults),]
temp <- temp[temp$NationSM!='CAmeroon' & temp$NationSM!='Servia' &
 temp$NationSM!='East Timur' & temp$NationSM!='EAst Timor',] 
dim(karenReput); karenReput <- merge(karenReput, temp, by='Refno', all.x=T); dim(karenReput)

wrightExprop <- read.dta('TomzWright2010.dta')
load('bits.rda')
bits$signedbitsSM <- 1
bits$ratifiedbitsSM <- ifelse(is.na(bits$Year_force), 0, 1)

setwd(pathData)
save(WBgdp, WBgdpCap, WBinflDeflator, WBgdpDeflator, kaopen, 
	polity, disputes, WBfdi, WBfdiGdp, fraser, heritage, icrg, 
	WGIregQual, karenReput, wrightExprop, bits,
	 file='allData.rda')

# Comparing my disputes to karen's (disputes v karenReput)
temp <- karenReput[,c('Refno', 'NationSM', 'Year', 'icsidcase', 'settle', 'cunctadcase')]
temp$cname <- countrycode(temp$NationSM, 'country.name', 'country.name')
temp[is.na(temp$cname),]
temp <- na.omit(temp)
temp$cnameYear <- paste(temp$cname, temp$Year, sep='')

disputes$cname <- countrycode(disputes$Country, 'country.name', 'country.name')
disputes[is.na(disputes$cname),]
disputes <- na.omit(disputes)
disputes$cnameYear <- paste(disputes$cname, disputes$Year, sep='')

dim(temp); dim(disputes)
masterDisp <- merge(disputes, temp, by='cname', all.x=T, all.y=T)
dim(masterDisp)