### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')

### prelim analyses

# Plotting reputations against disputes
# cdata <- combData[combData$cname=='ARGENTINA',]
cdata <- combData
cdata <- cdata[cdata$year>=1984,]
vars <- c('year', 'cname', 'fdi',
	'conc_disputes', 'conc_disputes_three', 'conc_disputes_five',
	'pend_disputes', 'pend_disputes_three', 'pend_disputes_five',
	'kaopen', 'polity2', 'Investment.Profile')
cdata <- cdata[,vars]
cdata <- na.omit(cdata)
countries <- as.character(unique(cdata$cname))
ii=128
setwd(pathGraphics)
pdf(file='concRep.pdf')
par(mar=c(5,4,4,5)+.1, mfrow=c(2,2))
for(ii in 1:length(countries)){
	pdata <- cdata[cdata$cname==countries[ii],]
	plot(pdata$year, pdata$Investment.Profile, las=1,
		type='l', col='blue', ylab='Reputation', xlab='year')
	par(new=T)
	plot(pdata$year, pdata$conc_disputes,
		type='s', col="red", xaxt="n", yaxt="n", xlab="", ylab="")
	axis(4,las=1)
	mtext("Disputes",side=4,line=3,cex=.8)
	legend("topleft",col=c("blue","red"), bty='n',
		lty=1,legend=c("Reputation","Disputes"))
	title(countries[ii]) }
dev.off()

### basic prelim models
cdata <- ddply(cdata, .(cname), transform, conc_disputesLag1 = 
            c(NA, conc_disputes[-length(conc_disputes)] ) )
cdata <- ddply(cdata, .(cname), transform, conc_disputesLag2 = 
            c(NA, conc_disputesLag1[-length(conc_disputesLag1)] ) )
cdata <- ddply(cdata, .(cname), transform, conc_disputesLag3 = 
            c(NA, conc_disputesLag2[-length(conc_disputesLag2)] ) )

model <- lmer(Investment.Profile ~ (1|cname) + conc_disputes +
	conc_disputesLag1 + conc_disputesLag2 + 
	polity2 + kaopen, data=cdata)
summary(model)