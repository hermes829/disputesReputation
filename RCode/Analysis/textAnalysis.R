### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###################################################################
# Load in Data
setwd(pathData)
# load('textData.rda')
load('textDatav2.rda')
load('caseTextData.rda')
load('modelData.rda')

# ICSID dispute variables
icsidVars=c('kicsidcase', 'icsidtreaty_case', 'icsidmember', 
	'cum_kicsidcase', 'cum_icsidtreaty_case')
icsidData=modelData[,c('ccode', 'cname', 'year', icsidVars)]

# Aggregate to yearly level
aggForm=formula(paste0(paste(icsidVars, collapse=' + ' ), ' ~ year'))
icsidYrData=summaryBy(aggForm, data=icsidData, FUN=sum, keep.names=TRUE)
###################################################################

###################################################################
# Convert dates to year month format
textData$date=as.Date(as.yearmon(textData$date))
textData$year=format(textData$date, "%Y")
textData=na.omit(textData)

# data
caseTextData$date=as.Date(as.yearmon(caseTextData$date))
caseTextData$year=format(caseTextData$date, "%Y")
caseTextData=na.omit(caseTextData)
caseTextData$count=1
cyear=summaryBy(count ~ icsid, data=caseTextData, FUN=sum)
# merge(cyear, )
###################################################################

###################################################################
# Country and year
cntryYrMention=summaryBy(count ~ year + cname, data=caseTextData, FUN=sum, keep.names=TRUE)

cntryYrMentionTime=cntryYrMention[which(cntryYrMention$year>=2007),]
byCntry=summaryBy(count ~ cname, data=cntryYrMention, FUN=sum)
byCntry[order(byCntry$count.sum, decreasing=TRUE),]
###################################################################

###################################################################
# yearly level
textData = textData[textData$year<=2011,]
tmp=ggplot(textData, aes(x=year)) + geom_histogram(binwidth=30)
tmp=tmp + ylab("Frequency") + xlab("Year")
tmp=tmp + theme(axis.text.x=element_text(angle=45, hjust=1))
tmp

ggplot(icsidYrData, aes(x=year, y=cum_kicsidcase))+geom_line()
ggplot(icsidYrData, aes(x=year, y=kicsidcase))+geom_line()
###################################################################

###################################################################
# yearly level
tmp=ggplot(textData, aes(x=date)) + geom_density()
tmp=tmp + ylab("Frequency") + xlab("Year")
tmp=tmp + theme(axis.text.x=element_text(angle=45, hjust=1))
tmp
###################################################################

###################################################################
# Monthly level
tmp=ggplot(textData, aes(x=date)) + geom_histogram(binwidth=30)
tmp=tmp + scale_x_date(labels = date_format("%Y-%b"),
    breaks = seq(min(textData$date)-5, max(textData$date)+5, 480),
    limits = c(as.Date("1975-10-01"), as.Date("2014-11-01")))
tmp=tmp + ylab("Frequency") + xlab("Year and Month")
tmp=tmp + theme(axis.text.x=element_text(angle=45, hjust=1))
tmp
setwd(pathGraphics)
pdf(file='histICSID.pdf', width=12, height=8)
tmp
dev.off()
###################################################################

###################################################################
# Monthly level
tmp=ggplot(textData, aes(x=date)) + geom_density()
tmp=tmp + scale_x_date(labels = date_format("%Y-%b"),
    breaks = seq(min(textData$date)-5, max(textData$date)+5, 480),
    limits = c(as.Date("1975-10-01"), as.Date("2014-11-01")))
tmp=tmp + ylab("Frequency") + xlab("Year and Month")
tmp=tmp + theme(axis.text.x=element_text(angle=45, hjust=1))
tmp
setwd(pathGraphics)
pdf(file='densityICSID.pdf', width=12, height=8)
tmp
dev.off()
###################################################################

###################################################################
# Number of media mentions for a case change by year?

###################################################################