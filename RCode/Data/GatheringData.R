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

WBgdp <- read.csv(list.files()[1])
WBgdpCap <- read.csv(list.files()[3])
WBdeflator <- read.csv(list.files()[5])
kaopen <- read.csv(list.files()[7])
polity <- read.csv(list.files()[9])

setwd(paste(pathData, '/Components', '/Disputes', sep=''))
list.files()
disputes <- read.csv(list.files()[1])

setwd(paste(pathData, '/Components', '/FDI', sep=''))
list.files()
WBfdi <- read.csv(list.files()[1])
WBfdiGdp <- read.csv(list.files()[3])

setwd(paste(pathData, '/Components', '/ReputationalMeasures/Fraser', sep=''))
list.files()
# snum <- sheetCount(list.files()[2])
snames <- sheetNames(list.files()[2])
fraser <- list()
for(ii in 1:17){ fraser[[ii]] <- read.xls(list.files()[2],ii) }
names(fraser) <- snames[1:17]

setwd(paste(pathData, '/Components', '/ReputationalMeasures/Heritage', sep=''))
list.files()
heritage <- read.csv(list.files()[1])

setwd(paste(pathData, '/Components', '/ReputationalMeasures/ICRG', sep=''))
list.files()
icrg <- read.csv(list.files()[2])

setwd(paste(pathData, '/Components', '/ReputationalMeasures/WGI', sep=''))
list.files()
WGIregQual <- read.csv(list.files()[1])

setwd(pathData)
save(WBgdp, WBgdpCap, WBdeflator, kaopen, polity,
	disputes, WBfdi, WBfdiGdp, fraser, heritage, icrg, 
	WGIregQual, file='allData.rda')