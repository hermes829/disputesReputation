### Run ECM analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)

# # Running model
setwd(pathData)
modelData <- na.omit(modelData)
forKaren <- merge(modelData, 
	combData[,c('cyear', setdiff(names(combData), names(modelData)))],
	by='cyear',all.x=T,all.y=F)
write.csv(forKaren, file='forKaren.csv')

# ### Descriptive statistics
summary(modelData)
modelSumm <- modelData[,5:ncol(modelData)]
means <- apply(modelSumm, 2, mean)
sds <- apply(modelSumm, 2, sd)
quants <- t(apply(modelSumm, 2, function(x) FUN=quantile(x,seq(0,1,.25))))
xtable(cbind(means, sds, quants),digits=3)

# Form of ECM
# ∆Yt = α + β0*∆Xt - β1(Yt-1 - β2Xt-1) + ε
# formula = Investment.Profile ~ LNgdp.y + chgdp + polity + LNtradebalance + signedbits + 
			# settle + conc_disputes

# formula = ch_Investment.Profile ~ 
			# Investment.ProfileLag + 
formula = ch_Property.Rights ~ 
			Property.RightsLag + 			
			ch_LNgdp.y + ch_LNtradebalance + ch_polity + ch_signedbits + ch_disputesNoSettle +
			LNgdp.yLag + LNtradebalanceLag + polityLag + signedbitsLag + disputesNoSettleLag +
			as.factor(ccode)-1

modelResults <- lm(formula, data=modelData)
modelLatex <- xtable(modelResults)[1:11,]
# rownames(modelLatex) <- c('Investment Profile$_{t-1}$',
# 	'$\\Delta$GDP$_{t}$',
# 	'$\\Delta$GDP Growth$_{t}$',
# 	'$\\Delta$Polity$_{t}$',
# 	'$\\Delta$BITs$_{t}$',
# 	'$\\Delta$ICSID Settlements$_{t}$',
# 	'$\\Delta$ICSID Disputes$_{t}$',
# 	'$\\Delta$Expropriation$_{t}$',
# 	'GDP$_{t-1}$',
# 	'GDP Growth$_{t-1}$',
# 	'Polity$_{t-1}$',
# 	'BITs$_{t-1}$',
# 	'ICSID Settlement$_{t-1}$',
# 	'ICSID Dispute$_{t-1}$',
# 	'Expropriation$_{t-1}$')
# print(xtable(modelLatex, sanitize.text.function=function(x){x}))
modelLatex

# Prelim graphics
setwd(pathGraphics)
pdf(file='ch_inv_hist.pdf')
hist(modelData$ch_Investment.Profile)
dev.off()