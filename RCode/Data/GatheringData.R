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
karenReput <- read.dta('ICSID_Reputation.dta')
wrightExprop <- read.dta('TomzWright2010.dta')

setwd(pathData)
save(WBgdp, WBgdpCap, WBinflDeflator, WBgdpDeflator, kaopen, 
	polity, disputes, WBfdi, WBfdiGdp, fraser, heritage, icrg, 
	WGIregQual, karenReput, wrightExprop,
	 file='allData.rda')