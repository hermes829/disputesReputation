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
textData$year=numSM(format(textData$date, "%Y"))
textData=na.omit(textData)
###################################################################

###################################################################
# yearly level
textData = textData[textData$year<=2014,]
cbind(table(textData$year))

tmp=ggplot(textData, aes(x=year))
tmp=tmp + geom_histogram(stat='bin', binwidth=1, fill='grey', color='darkgrey')
tmp=tmp + scale_y_continuous('Frequency', breaks=seq(0, 200, 50), limits=c(0,200), expand=c(0,0))
tmp=tmp + scale_x_continuous('', breaks=seq(1977, 2014, 6), expand=c(0,0))
tmp=tmp + geom_line(data=icsidYrData, aes(x=year, y=kicsidcase), lwd=2)
tmp=tmp + theme(
	axis.text.x=element_text(angle=45, hjust=1),
	axis.title.y=element_text(vjust=1),
	panel.grid=element_blank(),
	axis.ticks=element_blank(),
	panel.border = element_blank(),
	axis.line = element_line(color = 'black')
	)
tmp
setwd(pathGraphics)
tikz(file='histICSID', width=8, height=4, standAlone=F)
tmp
dev.off()
###################################################################

# Histogram of ICSID stuff
tmp=ggplot(icsidYrData, aes(x=year, y=kicsidcase))
tmp=tmp + geom_line()
tmp

###################################################################
# Case level data
caseTextData$date=as.Date(as.yearmon(caseTextData$date))
caseTextData$year=numSM(format(caseTextData$date, "%Y"))
caseTextData=na.omit(caseTextData)
caseTextData$count=1
cyear=summaryBy(count ~ icsid, data=caseTextData, FUN=sum)

# Country and year
cntryYrMention=summaryBy(count ~ year + cname, data=caseTextData, FUN=sum, keep.names=TRUE)

cntryYrMentionTime=cntryYrMention[which(cntryYrMention$year>=2007),]
byCntry=summaryBy(count ~ cname, data=cntryYrMention, FUN=sum)
# byCntry[order(byCntry$count.sum, decreasing=TRUE),]
###################################################################