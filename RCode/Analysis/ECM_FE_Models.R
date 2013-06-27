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
setwd(paste(pathLatex,'/feModel_V1',sep=''))
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
	paste('as.factor(',vars[25],')-1',sep=''), sep=' + ') 

covsV1.5 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits
	vars[7], vars[19], # sig bits
	vars[10], vars[22], # pend disp
	paste('as.factor(',vars[25],')-1',sep=''), sep=' + ') 

covsV2 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits 
	vars[7], vars[19], # sig bits
	vars[11], vars[23], # cp disp
	paste('as.factor(',vars[25],')-1',sep=''), sep=' + ') 

covsV3 = paste(vars[3], vars[4], vars[5], 
	vars[15], vars[16], vars[17], 
	# vars[6], vars[18], # rat bits 
	vars[7], vars[19], # sig bits
	vars[12], vars[24], # disp-settl
	paste('as.factor(',vars[25],')-1',sep=''), sep=' + ') 

formulaV1=paste(dv,covsV1);formulaV1.5=paste(dv,covsV1.5);
	formulaV2=paste(dv,covsV2);formulaV3=paste(dv,covsV3)

# Running models
models <- list(formulaV1, formulaV1.5, formulaV2, formulaV3)
modelResults <- lapply(models, function(x) FUN=lm(x, data=modelData))
modelSumm <- lapply(modelResults, function(x) FUN=robust.se(x, modelData$ccode)[[2]][1:11,])

# Saving results
varsTable <- c(
	# Lagged versions of DV
	# 'Investment.ProfileLag',
	'Property.RightsLag',
	# Percent change covariates
	'pch_LNgdp.y', 'pch_LNtradebalance', 'pch_polity', 
	# Include one of BIT measures
	# 'pch_cratifiedbitsSM',
	'pch_csignedbitsSM',
	# Four models in a table, include all dispute measures
	'pch_conc_disputes',
	'pch_pend_disputes',
	'pch_cp_disputes',
	'pch_disputesNoSettle',
	# Lagged variables
	'LNgdp.yLag', 'LNtradebalanceLag', 'polityLag', 
	# Include one of BIT measures
	# 'cratifiedbitsSMLag',
	'csignedbitsSMLag',
	# Four models in a table, include all dispute measures
	'conc_disputesLag',
	'pend_disputesLag',
	'cp_disputesLag',
	'disputesNoSettleLag'
	)

# Creating APSR like tables
# tabEst <- paste(varsTable, 'Estimate', sep="__")
# tabSE <- paste(varsTable, 'Std. Error', sep="__")
digs=3; noModels=4
tableResults <- matrix('', nrow=2*length(varsTable), ncol=1+noModels)

tableResults[,1] <- rep(varsTable,2)
colnames(tableResults) <- c('Variable',paste('Model',1:noModels))
for(ii in 2:ncol(tableResults)){
	temp <- modelSumm[[ii-1]]
	df <- summary(modelResults[[ii-1]])[['df']][2]
	temp <- temp[match(tableResults[,'Variable'], rownames(temp)),]
	estims <- temp[1:length(varsTable),'Estimate']
	estims <- round(as.numeric(as.character(estims)),digs)
	tvals <- abs(temp[1:length(varsTable),'t value'])
	tvals <- round(as.numeric(as.character(tvals)),digs)
	estims <- ifelse(tvals>=qt(0.95,df) & !is.na(tvals) & tvals<qt(0.975,df), 
		paste('$', estims,'^{\\ast}$',sep=''), estims)
	estims <- ifelse(tvals>=qt(0.975,df) & !is.na(tvals), 
		paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
	tableResults[1:length(varsTable),ii] <- estims
	serrors <- temp[(length(varsTable)+1):nrow(tableResults),'Std. Error']
	serrors <- round(as.numeric(as.character(serrors)),digs)
	serrors <- paste('(',serrors,')',sep='')
	serrors <- ifelse(serrors=='(NA)','',serrors)
	tableResults[(length(varsTable)+1):nrow(tableResults),ii] <- serrors
}

# Reorganizing rows and variable labels
tableFinal <- NULL
for(ii in 1:length(varsTable)){
	temp <- cbind('', t(tableResults[ii+length(varsTable),2:ncol(tableResults)]))
	tableFinal <- rbind(tableFinal, tableResults[ii,], temp) }

# Adding other info
sSize <- cbind('n', t(as.vector(mapply(x=modelResults,
		function(x) FUN=summary(x)[['df']][2] + summary(x)[['df']][1] ))))
gSize <- cbind('N', t(as.vector(mapply(x=modelResults, function(x)
	FUN=summary(x)[['df']][1]-length(attr(summary(x)[['terms']],'term.labels'))+1 ))))
rSQ <- cbind('$R^{2}$', t(as.vector(mapply(x=modelResults,
		function(x) FUN=round(summary(x)[['r.squared']],2) ))))
arSQ <- cbind('Adj. $R^{2}$', t(as.vector(mapply(x=modelResults,
		function(x) FUN=round(summary(x)[['adj.r.squared']],2) ))))
rmse <- round(mapply(x=modelResults, function(x) 
	FUN=sqrt(mean(summary(x)[['residuals']]^2))),2)
frmse <- cbind('RMSE', t(rmse))

tableFinal <- rbind(tableFinal, sSize, gSize, rSQ, arSQ, frmse)
# colnames(tableFinal) <- paste('\\multicolumn{1}{c}{',colnames(tableFinal),'}',sep='')
# \multicolumn{ 1 }{ c }{ Model 1 }

setwd(paste(pathLatex,'/feModel_V1',sep=''))
print.xtable(xtable(tableFinal, align='llcccc',
	# caption='Fixed effects regression on investment profile (DV=pch\\_Investment.Profile) with standard errors in parentheses. $^**$ and $^*$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	caption='Fixed effects regression on the protection of property rights (DV=pch\\_Property.Rights) with standard errors in parentheses. $^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
	), include.rownames=FALSE,
	# sanitize.text.function = function(x) x,
	sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
	hline.after=c(0,0,2,18,34,39,39), 
	# file='modelResultsInvProfileV1.tex'
	# file='modelResultsPropRightsV1.tex'
	# file='modelResultsInvProfileV2.tex'
	file='modelResultsPropRightsV2.tex'
	)

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

# mat <- matrix(0, nrow=2, ncol=2)
# diag(mat) <- c('$\\sigma_1^2$', '$\\sigma_2^2$')
# tbl <- xtable(mat) 
# print.xtable(tbl, sanitize.text.function = function(x) x)