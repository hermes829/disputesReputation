### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('forAnalysis.rda')
karenData=read.dta('Dispute Data.dta')
karenData$cyear=paste(karenData$ccode, karenData$year, sep='')

dim(karenData); dim(allData)
length(unique(karenData$ccode)); length(unique(allData$ccode))
setdiff(allData$cyear, karenData$cyear)

modelData=allData[allData$upperincome!=1,]
modelForm=formula(pch_Investment.Profile ~ pch_Internal.Conflict + factor(ccode)-1)

karenData2=karenData[karenData$upperincome!=1,]
gKdata=na.omit( karenData2[,c('pch_Investment_Profile', 'pch_Internal_Conflict', 'ccode', 'year')] )
kmodelForm=formula(pch_Investment_Profile ~ pch_Internal_Conflict + factor(ccode)-1)

require(panelAR)
pcseAR1=panelAR(modelForm, modelData, 'ccode', 'year', 
	autoCorr = c("psar1"), panelCorrMethod="pcse",
	rhotype='breg', complete.case=TRUE  )
pcseAR1$coefficients[1]
sqrt(diag(pcseAR1$vcov)[1])
pcseAR1$r2
nrow(pcseAR1$panelStructure$Sigma)
length(pcseAR1$residual)
