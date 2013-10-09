### Run ECM analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')
allVars=names(allData)
dirt=c('pch_','lag_','distC_','distD_','distD2_')
for(ii in 1:length(dirt)){allVars=gsub(dirt[ii],'',allVars)}
unique(allVars)

# Adding democracy binary
allData$democ=as.numeric(allData$polity>=16)

# dep='Investment.Profile'
dep='Property.Rights'
dv=paste('pch_', dep, sep='')

covs <- c(
	'cicsidcase', 'signedbits',
	'LNgdpCAP', 'LNpopulation',
	'domSUM', 
	'kaopen',
	# 'polity',
	'xconst',
	'polconiii')
covs=unlist(lapply(covs, function(x) FUN=paste(c('pch_', 'lag_'),x,sep='')))

# Adding other covariates
# Lagged DV
covs=c(paste('lag_',dep,sep=''),covs)
# Developed Country Binary
covs=c('oecd',covs)
# Democracy Binary
# covs=c('democ',covs)

ids=c('ccode','year')
vars=c(dv, covs, ids)

dim(allData)
dim(na.omit(allData[,vars]))

mform=paste(
	paste(dv, paste(covs,collapse=' + '), sep=' ~ ') )

# RE Models
reform=formula(paste(mform,
			' + (1 | ccode)', sep=''))
remodel <- lmer(reform, data=allData )
attributes(summary(remodel))$coefs
sqrt(mean((resid(remodel))^2))

# FE Models
feform=formula(paste(mform,
		' + as.factor(ccode) - 1', sep=''))
femodel <- lm(feform, data=allData )
summary(femodel)$coefficients[1:(length(covs)+1),]
sqrt(mean(femodel$residuals^2))
summary(femodel)$r.squared
summary(femodel)$adj.r.squared

# trade sig for inv profile not prop rights
# inv sig for proprights but not inv prof