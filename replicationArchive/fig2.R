#######################################################################################
### source in the setup.R script using the path on your computer
source('~/Research/disputesReputation/replicationArchive/setup.R')

### Load data
load('textData.rda')
load('analysisData.rda')
#######################################################################################

###################################################################
# ICSID dispute variables
icsidVars=c('iDisp', 'iDispB', 'iDispC', 'iDispBC')
icsidData=aData[,c('ccode', 'cname', 'year', icsidVars)]

# Aggregate to yearly level
aggForm=formula(paste0(paste(icsidVars, collapse=' + ' ), ' ~ year'))
icsidYrData=summaryBy(aggForm, data=icsidData, FUN=sum, keep.names=TRUE)
###################################################################

###################################################################
# Convert dates to year month format
textData$date=as.Date(as.yearmon(textData$date))
textData$year=num(format(textData$date, "%Y"))
textData=na.omit(textData)
###################################################################

###################################################################
# yearly level
textData = textData[textData$year<=2014,]

tmp=ggplot(textData, aes(x=year)) + 
	geom_histogram(stat='bin', binwidth=1, fill='grey', color='darkgrey') + 
	scale_y_continuous('Occurrences', expand=c(0,0)) + 
	scale_x_continuous('', expand=c(0,0), labels=seq(1970, 2014, 4), breaks=seq(1970, 2014, 4)) + 
	geom_line(data=icsidYrData, aes(x=year+.5, y=iDisp), lwd=2) + 
	theme(
		axis.text.x=element_text(angle=45, hjust=1),
		axis.title.y=element_text(vjust=1),
		# panel.grid=element_blank(),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		# axis.line = element_line(color = 'black')
	)
ggsave(tmp, file='fig2.pdf', width=8, height=4)
###################################################################