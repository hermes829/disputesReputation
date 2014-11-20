### Load setup
if(Sys.info()['user']=='janus829'){
	source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')
}

if(Sys.info()['user']=='s7m'){
	source('/Users/s7m/Research/RemmerProjects/disputesReputation/RCode/setup.R')
}

###################################################################
# Load in Data
setwd(pathData)
load('textData.rda')
###################################################################

###################################################################
library(zoo)
library(ggplot2)
library(scales)

textData$date=as.Date(as.yearmon(textData$date))
textData=na.omit(textData)

###################################################################
tmp=ggplot(textData, aes(x=date)) + geom_histogram(binwidth=30)
tmp=tmp + scale_x_date(labels = date_format("%Y-%b"),
                    breaks = seq(min(textData$date)-5, 
                    	max(textData$date)+5, 480),
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
tmp=ggplot(textData, aes(x=date)) + geom_density()
tmp=tmp + scale_x_date(labels = date_format("%Y-%b"),
                    breaks = seq(min(textData$date)-5, 
                    	max(textData$date)+5, 480),
                    limits = c(as.Date("1975-10-01"), as.Date("2014-11-01")))
tmp=tmp + ylab("Frequency") + xlab("Year and Month")
tmp=tmp + theme(axis.text.x=element_text(angle=45, hjust=1))
tmp
setwd(pathGraphics)
pdf(file='densityICSID.pdf', width=12, height=8)
tmp
dev.off()
###################################################################