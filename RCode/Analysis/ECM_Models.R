### Run ECM analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')
load('forAnalysisFull.rda')

# ### Descriptive statistics
vars <- c(
	# DVs
	'pch_Property.Rights',
	'pch_Investment.Profile',
	# PCH controls
	'pch_LNgdp.y', 'pch_LNtradebalance', 'pch_polity', 
	# PCH key covariates
	'pch_cratifiedbitsSM',
	'pch_csignedbitsSM',
	'pch_csettle',
	'pch_conc_disputes',
	'pch_pend_disputes',
	'pch_cp_disputes',
	'pch_disputesNoSettle',
	# Lagged variables
	'Property.RightsLag',
	'Investment.ProfileLag',
	'LNgdp.yLag', 'LNtradebalanceLag', 'polityLag', 
	'cratifiedbitsSMLag',
	'csignedbitsSMLag',
	'csettleLag',
	'conc_disputesLag',
	'pend_disputesLag',
	'cp_disputesLag',
	'disputesNoSettleLag',
	# country effects
	'ccode', 'cname', 'year')

modelData <- modelData[,c(1:16,29:52)]
modelSumm <- modelData[,5:ncol(modelData)]
means <- apply(modelSumm, 2, mean)
sds <- apply(modelSumm, 2, sd)
quants <- t(apply(modelSumm, 2, function(x) FUN=quantile(x,seq(0,1,.25))))
summResults <- xtable(cbind(means, sds, quants),digits=2)
setwd(paste(pathLatex,'/PrelimResults3',sep=''))
print(summResults, file='summResults.tex')

# Form of ECM
# ∆Yt = α + β0*∆Xt - β1(Yt-1 - β2Xt-1) + ε
# dv = paste(vars[2], ' ~ ', vars[14], ' +') # Inv Profile
dv = paste(vars[1], ' ~ ', vars[13], ' +') # Prop Rights

covsV1 = paste(vars[3], vars[4], vars[5],   
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits
	vars[7], vars[19], # sig bits
	vars[9], vars[21], # conc disp
	paste('(1|',vars[25],')',sep=''), sep=' + ') 

covsV1.5 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits
	vars[7], vars[19], # sig bits
	vars[10], vars[22], # pend disp
	paste('(1|',vars[25],')',sep=''), sep=' + ') 

covsV2 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits 
	vars[7], vars[19], # sig bits
	vars[11], vars[23], # cp disp
	paste('(1|',vars[25],')',sep=''), sep=' + ') 

covsV3 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits 
	vars[7], vars[19], # sig bits
	vars[12], vars[24], # disp-settl
	paste('(1|',vars[25],')',sep=''), sep=' + ') 

formulaV1=paste(dv,covsV1);formulaV1.5=paste(dv,covsV1.5);
	formulaV2=paste(dv,covsV2);formulaV3=paste(dv,covsV3)

# Running models
models <- list(formulaV1, formulaV1.5, formulaV2, formulaV3)
modelResults <- lapply(models, function(x) FUN=lmer(x, data=modelData))
modelSumm <- lapply(modelResults, function(x) FUN=attributes(summary(x))[['coefs']])

# Saving results
varsTable <- c(
	# Lagged versions of DV
	'Investment.ProfileLag',
	# 'Property.RightsLag',
	# Percent change covariates
	'pch_LNgdp.y', 'pch_LNtradebalance', 'pch_polity', 
	# Include one of BIT measures
	'pch_cratifiedbitsSM',
	# 'pch_csignedbitsSM',
	# Four models in a table, include all dispute measures
	'pch_conc_disputes',
	'pch_pend_disputes',
	'pch_cp_disputes',
	'pch_disputesNoSettle',
	# Lagged variables
	'LNgdp.yLag', 'LNtradebalanceLag', 'polityLag', 
	# Include one of BIT measures
	'cratifiedbitsSMLag',
	# 'csignedbitsSMLag',
	# Four models in a table, include all dispute measures
	'conc_disputesLag',
	'pend_disputesLag',
	'cp_disputesLag',
	'disputesNoSettleLag',
	)

# Old results using pooled models and apsrtable
# model1<-lm(formulaV1,modelData);model1.5<-lm(formulaV1.5,modelData);
# 	model2<-lm(formulaV2,modelData);model3<-lm(formulaV3,modelData)

# modelLatex <- apsrtable(model1, model1.5, model2, model3, 
# 	order='longest',
# 	digits=3, caption.position='below', Sweave=T,
# 	# caption='Here we show the results of a fixed effects regression on investment profile (DV=pch\\_Investment.Profile) using ratified BITs as a covariate, a number of dispute measures, and typical control variables.')
# 	# caption='Here we show the results of a fixed effects regression on the protection of property rights (DV=pch\\_Property.Rights) using ratified BITs as a covariate, a number of dispute measures, and typical control variables.')
# 	# caption='Here we show the results of a fixed effects regression on investment profile (DV=pch\\_Investment.Profile) using signed BITs as a covariate, a number of dispute measures, and typical control variables.')
# 	caption='Here we show the results of a fixed effects regression on the protection of property rights (DV=pch\\_Property.Rights) using signed BITs as a covariate, a number of dispute measures, and typical control variables.')

# setwd(paste(pathLatex,'/PrelimResults3',sep=''))

# # writeLines(modelLatex,file("modelResultsInvProfileV1.tex"))
# # writeLines(modelLatex,file("modelResultsPropRightsV1.tex"))

# # writeLines(modelLatex,file("modelResultsInvProfileV2.tex"))
# writeLines(modelLatex,file("modelResultsPropRightsV2.tex"))

# rownames(modelLatex) <- c(
# 	'Property Rights$_{t-1}$',
# 	'$\\Delta$GDP$_{t}$',
# 	'$\\Delta$Trade Balance$_{t}$',
# 	'$\\Delta$Polity$_{t}$',
# 	'$\\Delta$Ratified BITs$_{t}$',
# 	'$\\Delta$Concl. Disputes$_{t}$',
# 	'GDP$_{t-1}$',
# 	'Trade Balance$_{t-1}$',
# 	'Polity$_{t-1}$',
# 	'Ratified BITs$_{t-1}$',
# 	'Conc. Dispute$_{t-1}$'
# 	)

# # Prelim graphics
# setwd(pathGraphics)
# pdf(file='ch_inv_hist.pdf')
# hist(modelData$ch_Investment.Profile)
# dev.off()