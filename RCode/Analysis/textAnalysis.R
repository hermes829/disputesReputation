### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###################################################################
# Load in Data
setwd(pathData)
load('textData.rda')
load('modelData.rda')

# ICSID dispute variables
icsidVars=c('kicsidcase', 'icsidtreaty_case', 'icsidmember', 
	'cum_kicsidcase', 'cum_icsidtreaty_case')
icsidData=modelData[,c('ccode', 'cname', 'year', icsidVars)]

# Aggregate to yearly level
aggForm=formula(paste0(paste(icsidVars, collapse=' + ' ), ' ~ year'))
icsidYrData=summaryBy(aggForm, data=icsidData, FUN=sum, keep.names=TRUE)
# Add in values to icsidYrData for 2012:2014
add=matrix(c(2012:2014,c(50,40,38),rep(NA, (ncol(icsidYrData)-2)*3)),
	byrow=FALSE,nrow=3, dimnames=list(NULL, names(icsidYrData)))
icsidYrData=rbind(icsidYrData, add)
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
tmp=tmp + scale_x_continuous('', breaks=seq(1987, 2014, 3), expand=c(0,0), limits=c(1987,2014))
tmp=tmp + geom_line(data=icsidYrData, aes(x=year+.5, y=kicsidcase), lwd=2)
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