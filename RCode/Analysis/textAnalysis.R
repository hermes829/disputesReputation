####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

###################################################################
# Load in Data
load(paste0(pathBin, 'textData.rda'))
load(paste0(pathBin, 'analysisData.rda'))

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
cbind(table(textData$year))

tmp=ggplot(textData, aes(x=year))
tmp=tmp + geom_histogram(stat='bin', binwidth=1, fill='grey', color='darkgrey')
tmp=tmp + scale_y_continuous('Frequency', expand=c(0,0))
tmp=tmp + scale_x_continuous('', expand=c(0,0), labels=seq(1970, 2014, 4), breaks=seq(1970, 2014, 4))
tmp=tmp + geom_line(data=icsidYrData, aes(x=year+.5, y=iDisp), lwd=2)
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

###################################################################
# Save data
textYr = table(textData$year) %>% cbind(year=names(.),count=.) %>% apply(., 2, num) %>% data.frame(.,stringsAsFactors=FALSE,row.names=NULL) 
textYr = setdiff(1986:2014, textYr$year) %>% cbind(year=., count=0) %>% rbind(textYr, .) %>% .[order(.$year),]
textYr = textYr[textYr$year>=1986,]
save(textYr, file=paste0(pathBin, 'textYr.rda'))
###################################################################