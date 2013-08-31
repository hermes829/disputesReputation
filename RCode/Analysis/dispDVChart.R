### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis0.rda')

### Slicing to relevant vars
vars <- c('Investment.Profile', 'Property.Rights', 
	'cicsidcase', 'icsidcase',
	'conc_disputes', 'pend_disputes', 'cp_disputes', 
	'oecd', 'upperincome',
	'year', 'ccode', 'cname')
data <- allData0[, vars]

### Creating moving sum for disputes
dispVar <- c('icsidcase')
wdow <- 3
mvalab <- function(x){paste('mva',wdow,'_',x,sep='')}
data <- moveAvgPanel(data, 'ccode', 'year', dispVar, wdow)
data[,mvalab(dispVar)] <- data[,mvalab(dispVar)]*wdow

### Dropping OECD and upperincome countries subbing to 1984-2011
data <- data[data$year>=1984 & data$year<=2011,]
data <- data[data$oecd!=1,]
data <- data[data$upperincome!=1,]

### Creating charts
ggplotSM <- function(x='cicsidcase', xlabel='Cum. ICSID Cases by',
	y='Investment.Profile', ylabel='Investment Profile Rating in', 
	time=1985, dataframe=data) {
		slice <- dataframe[dataframe$year==time,]
		ggData <- na.omit(slice[,c(x, y)])
		ggData[,x] <- as.factor(ggData[,x])

		form <- formula(paste(y, x, sep=' ~ '))
		means <- summaryBy(form, data=ggData, FUN=mean)
		q25 <- summaryBy(form, data=ggData, FUN=function(x){quantile(x, 0.25)})
		q75 <- summaryBy(form, data=ggData, FUN=function(x){quantile(x, 0.75)})
		ggSumm <- cbind(means, q25[,2], q75[,2])
		colnames(ggSumm)[2:4] <- c('mean', 'q25', 'q75')
		ggData <- merge(ggData, ggSumm, by=x, all.x=T)
		colnames(ggData)[1:2] <- c('x', 'y')

		ggplot(ggData, aes(x=x, y=y)) + 
		 geom_jitter(position=position_jitter(width=.25), size=2.5, col='grey') +
		  geom_errorbar(aes(ymin=q25, ymax=q75), width=0.2) +
		   geom_point(aes(y=mean), shape=18, size=2.5) +
		    xlab(paste(xlabel, time)) + ylab(paste(ylabel, time)) +
		     theme(legend.position='none', legend.title=element_blank(),
			    axis.ticks=element_blank(), panel.grid.major=element_blank(),
			    panel.grid.minor=element_blank(), text=element_text(size=10))
	}

years <- c(1985, 1990, 1995, 2000, 2005, 2010)
ggInvProf <- lapply(years, function(zz) 
	FUN=ggplotSM(x='cicsidcase', xlabel='Cum. ICSID Cases by',
		y='Investment.Profile', ylabel='Investment Profile Rating in', 
		time=zz, dataframe=data))
ggPropRight <- lapply(years, function(zz) 
	FUN=ggplotSM(x='cicsidcase', xlabel='Cum. ICSID Cases by',
		y='Property.Rights', ylabel='Property Rights Rating in', 
		time=zz, dataframe=data))

setwd(pathGraphics)
pdf(file='dvCDisp.pdf', height=7, width=10)
multiplot(ggInvProf, cols=3); multiplot(ggPropRight, cols=3)
dev.off()


years <- c(1985, 1990, 1995, 2000, 2005, 2010)
ggInvProf <- lapply(years, function(zz) 
	FUN=ggplotSM(x=mvalab(dispVar), xlabel='ICSID Cases 3 years previous',
		y='Investment.Profile', ylabel='Investment Profile Rating in', 
		time=zz, dataframe=data))
ggPropRight <- lapply(years, function(zz) 
	FUN=ggplotSM(x=mvalab(dispVar), xlabel='ICSID Cases 3 years previous',
		y='Property.Rights', ylabel='Property Rights Rating in', 
		time=zz, dataframe=data))

setwd(pathGraphics)
pdf(file='dvMVA3Disp.pdf', height=7, width=10)
multiplot(ggInvProf, cols=3); multiplot(ggPropRight, cols=3)
dev.off()