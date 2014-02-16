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
temp=panelAR(modelForm, modelData, 'ccode', 'year', 
	autoCorr = c("psar1"), panelCorrMethod="pcse",
	rhotype='breg', complete.case=TRUE  )
temp$coefficients[1]
sqrt(diag(temp$vcov)[1])