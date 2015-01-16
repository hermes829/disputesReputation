### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis0.rda')

### Slicing to relevant vars
repVars <- c('Property.Rights', 'Investment.Profile', 
	'property.rights', 
	'investment.freedom',
	'X2C..Protection.of.property.rights',
	'X2..Legal.System...Property.Rights')
repdchVars <- paste('dch_', repVars, sep='')
reppchVars <- paste('pch_', repVars, sep='')
vars <- c(repVars, repdchVars, reppchVars,
	c('cicsidcase', 'icsidcase',
		'conc_disputes', 'pend_disputes', 'cp_disputes', 
		'oecd', 'upperincome',
		'year', 'ccode', 'cname') )
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
		n <- summaryBy(form, data=ggData, FUN=function(x){length(x)})
		ggSumm <- cbind(means, q25[,2], q75[,2], n[,2])
		colnames(ggSumm)[2:5] <- c('mean', 'q25', 'q75', 'n')
		ggData <- merge(ggData, ggSumm, by=x, all.x=T)
		colnames(ggData)[1:2] <- c('x', 'y')
		# ggData <- ggData[ggData$n>=3,]

		ggplot(ggData, aes(x=x, y=y)) + 
		 # geom_jitter(position=position_jitter(width=.25), size=2.5, col='grey') +
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
# pdf(file='dvCDisp.pdf', height=7, width=10)
# multiplot(ggInvProf, cols=3); multiplot(ggPropRight, cols=3)
# dev.off()


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
# pdf(file='dvMVA3Disp.pdf', height=7, width=10)
# multiplot(ggInvProf, cols=3); multiplot(ggPropRight, cols=3)
# dev.off()

### Subsetting to countries with high disputes
temp <- na.omit(data[data$year==2011, c('cname', 'year', 'cicsidcase')])
temp <- temp[order(temp$cicsidcase, decreasing=T),]
hdcntries <- temp[temp$cicsidcase>=2,'cname']

# setwd(pathGraphics)
# pdf(file='highDispCntries.pdf')
# par(mar=c(5,4,4,5)+.1, mfrow=c(3,2))
# for(ii in 1:length(hdcntries)){
# 	pdata <- data[data$cname==hdcntries[ii],]
# 	plot(pdata$year, pdata$Investment.Profile, las=1,
# 		type='l', col='blue', ylab='Reputation', xlab='year')
# 	par(new=T)
# 	plot(pdata$year, pdata$cicsidcase,
# 		type='h', col="red", xaxt="n", yaxt="n", xlab="", ylab="")
# 	axis(4,las=1,at=pdata$cicsidcase,labels=round(pdata$cicsidcase,digits=0))
# 	mtext("Disputes",side=4,line=3,cex=.8)
# 	legend("topleft",col=c("blue","red"), bty='n',
# 		lty=1,legend=c("Reputation","Disputes"))
# 	title(hdcntries[ii])
# }
# dev.off()

### 2010 plots
mapData <- cshp(date = as.Date("2010-6-30"))
coords<-coordinates(mapData)
dst<-as.matrix(dist(coords, upper=TRUE, diag=TRUE))
xy<-cmdscale(dst, k=3)
r<-rank(xy[,1])/dim(xy)[1]
g<-rank(xy[,2])/dim(xy)[1]
b<-rank(xy[,3])/dim(xy)[1]

farben<-rgb(g,r,0)
#add colors back to the map
mapData$mapcolors<-farben
# rearrange order of farben to align with 
# latent space data
latColors <- data.frame(cbind(gwcodes=mapData$GWCODE,farben))
latColors$gwcodes <- as.numeric(as.character(latColors$gwcodes))
latColors$ccode <- panel$ccode[match(latColors$gwcodes,panel$GWCODE)]

data10 <- data[data$year==2010,]
data10 <- merge(data10, latColors, by='ccode', all.x=T)
data10$CNTRY_NAME <- panel$CNTRY_NAME[match(data10$ccode, panel$ccode)]
data10$sabb <- countrycode(data10$CNTRY_NAME, 'country.name', 'iso3c')
ylabs <- c(rep('ICRG Rep. Var',2), rep('Herit Rep. Var', 2), 
	rep('Fraser Rep. Var', 2))
# data10 <- data10[data10$cicsidcase>1,]
data10 <- data10[data10$cicsidcase<40,]

# pdf(file='rep2010ICSID_noArg.pdf', width=10, height=8)
# par(mfrow=c(3,2))
# for(ii in 1:length(repVars)){
# 	plot(data10$cicsidcase, data10[,repVars[ii]], 
# 		ylab=ylabs[ii], xlab='Disputes', xaxt='n',
# 		las=1
# 		, pch=''
# 		# , pch=18, col=as.character(data10$farben)
# 		)
# 		text(jitter(data10$cicsidcase, .25), data10[,repVars[ii]],
# 		 labels = data10$sabb, cex = 0.8, col=as.character(data10$farben))
# 	axis(1, at = seq(0, 18, by = 1), las=1)
# 	title(paste(repVars[ii], " & \n Cum. Disputes in 2010", sep=''))
# 	}
# plot(mapData, col=farben, lwd=1e-200)
# dev.off()

### Change in ratings after n disputes in t periods
dataD <- data; dataD$cyear <- paste(dataD$ccode, dataD$year, sep='')
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), -1)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), -2)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), -3)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), -4)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), 1)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), 2)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), 3)
dataD <- lagDataSM(dataD, 'cyear', 'ccode', c('cicsidcase','icsidcase'), 4)

for(kk in 1:length(repdchVars)){
	chVar <- repdchVars[kk]
	chData <- NULL
	for(ii in 1:5){
		for(jj in c(-4:-1,1:4)){
			lVar <- paste('lag',jj,'_icsidcase',sep='')		
			clVar <- paste('lag',jj,'_cicsidcase',sep='')
			slice <- cbind(
				dataD[which(dataD[,lVar]==1 & dataD[,clVar]==ii), 
					c(chVar,c('ccode', 'year'))], 
				ii, jj)
			chData <- rbind(chData, slice)
		}
	}

	chData <- data.frame(chData)
	colnames(chData) <- c('change', 'ccode', 'year', 'dispute', 'time')
	chData <- na.omit(chData)
	chData$dispute <- factor(chData$dispute, 
		levels=1:5, labels=paste(1:5, 'Dispute(s)'))
	chData$farben <- as.character(latColors$farben[match(chData$ccode, latColors$ccode)])
	chData$ccode <- as.factor(chData$ccode)
	chData$CNTRY_NAME <- panel$cname[match(chData$ccode, panel$ccode)]
	chData$sabb <- countrycode(chData$CNTRY_NAME, 'country.name', 'iso3c')
	chData$sabb <- paste(chData$sabb, substrRight(chData$year,2), sep='_')
	ggColors <- chData$farben
	names(ggColors) <- chData$ccode

	pdf(file=paste(chVar, 'chByDispute.pdf', sep=''), width=8, height=5)
	ggCh <- ggplot(chData, aes(x=as.factor(time), y=change, 
		label=sabb, group=dispute, color=ccode ) )
	ggCh <- ggCh + geom_text(size=1, position=position_jitter(width=.24, height=0))
	ggCh <- ggCh + scale_colour_manual(values=ggColors)
	ggCh <- ggCh + geom_hline(yintercept=0, color='red', lty=2, size=0.25)
	ggCh <- ggCh + facet_wrap(~dispute,ncol=5)
	ggCh <- ggCh + xlab('Time Since Dispute (Years)') + 
		ylab(paste('Change in', chVar))
	ggCh <- ggCh + theme(legend.position='none', legend.title=element_blank(),
				    axis.ticks=element_blank(), 
				    panel.grid.major=element_blank(),
				    # panel.grid.minor=element_blank(), 
				    text=element_text(size=10))
	print(ggCh)
	dev.off()
}