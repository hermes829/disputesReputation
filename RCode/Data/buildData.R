### Load setup
if(Sys.info()['user']=='janus829'){
	source('~/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R') }

if(Sys.info()['user']=='s7m'){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }	

#######################################################################################
# Directly loading in Karen's data
setwd(paste(pathData, '/Components', sep=''))
modelData=read.dta('Investment Profile Data.11.dta')

# Use cumulative number of disputes 
colnames(modelData)[colnames(modelData)=='lagcumcunctadcase']='lag_cumcunctadcase'
colnames(modelData)[colnames(modelData)=='lagcum_icsidtreaty_case']='lag_cum_icsidtreaty_case'
colnames(modelData)[colnames(modelData)=='lagcum_kicsidcase']='lag_cum_kicsidcase'
colnames(modelData)[colnames(modelData)=='lagpch_gdp']='lag_pch_gdp'

lagVars=c('cumunsettled_icsid_treaty','cum_alltreaty')
modelData$cyear=numSM(modelData$cyear)
modelData=lagDataSM(modelData, 'cyear', 'ccode', lagVars, 1)
colnames(modelData)[(ncol(modelData)-1):ncol(modelData)]=paste0('lag_',lagVars)

# For modeling
ivDisp=c('cum_kicsidcase','cum_icsidtreaty_case',
	'cumunsettled_icsid_treaty','cumcunctadcase','cum_alltreaty' )

# Create two year moving sum of dispute variables
dispVars=c('kicsidcase', 'icsidtreaty_case', 
	'unsettled_icsid_treaty', 'cunctadcase', 'alltreaty')

modelData=movePanel(modelData, 'ccode', 'year', dispVars, 2, sum=TRUE)
modelData=lagDataSM(modelData, 'cyear', 'ccode', paste0('mvs2_',dispVars), 1)
colnames(modelData)[(ncol(modelData)-4):ncol(modelData)]=paste0('lag_',paste0('mvs2_',dispVars))

# For modeling
ivDisp=paste0('mvs2_',dispVars)

# Data for lagged estimate analysis
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 1)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 2)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 3)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 4)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 5)

# Create five year moving sum of dispute variables
modelData=movePanel(modelData, 'ccode', 'year', dispVars, 5, sum=TRUE)
modelData=lagDataSM(modelData, 'cyear', 'ccode', paste0('mvs5_',dispVars), 1)
colnames(modelData)[(ncol(modelData)-4):ncol(modelData)]=paste0('lag_',paste0('mvs5_',dispVars))

# For modeling
ivDisp=paste0('mvs5_',dispVars)

# Data for lagged estimate analysis
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 1)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 2)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 3)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 4)
modelData=lagDataSM(modelData, 'cyear', 'ccode', ivDisp, 5)

# Narrow sample to relevant range
modelData = modelData[modelData$upperincome==0,]
modelData = modelData[modelData$year>1986,]
#######################################################################################

#######################################################################################
# Save
setwd(pathData)
save(modelData, file='modelData.rda')
#######################################################################################