####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

###############################################################
# Load in datasets
dvData='icrg.rda'
monData=c('disputes', 'kaopen', 'constraints', 'polity', 'worldBank', 'bitPanel') %>% paste0('.rda')
dyData=c('distMats') %>% paste0('.rda')
###############################################################

###############################################################
# Merge monadic variables from icrg, worldBank, polity, constraints into kaopen
for(pkg in pathBin %>% paste0(c(dvData, monData)) ) { load( pkg ) }; rm(list='pkg')

# Merge disputes
disputes = disputes[,c('cyear', names(disputes)[9:ncol(disputes)])]
aData = merge(icrg, disputes, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge kaopen
kaopen = kaopen[,c('cyear', 'kaopen', 'ka_open')]
aData = merge(aData, kaopen, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge worldbank
aData = merge(aData, worldBank, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge polity
polity = polity[,c('cyear', names(polity)[8:21])]
aData = merge(aData, polity, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge constraints
constraints = constraints[,c('cyear', names(constraints)[8:10])]
aData = merge(aData, constraints, by='cyear', all.x=TRUE, all.y=FALSE)

# Merge BITs
bitPanel$cyear = paste0(bitPanel$ccode, bitPanel$year)
bitPanel = bitPanel[,c('cyear', names(bitPanel)[7:18])]
aData = merge(aData, bitPanel, by='cyear', all.x=TRUE, all.y=FALSE)

# Remove leftover datasets
rm(list=c(substr(dvData, 1, nchar(dvData)-4),
	substr(monData, 1, nchar(monData)-4)) )
###############################################################

###############################################################
# Create lags

# Select vars to lag
noLag = c( 'cyear','Country', 'year', 'cname', 'ccode', 'cnameYear' )
toLag = setdiff(names(aData), noLag)

# Adjustments to id variables
aData$cyear = num(aData$cyear)
aData$ccode = num(aData$ccode)

# Make sure all variables to be lagged are numeric
sum(apply(aData[,toLag],2,class)=='numeric')/length(toLag)

# Lag selected variables 1 year
aData = lagData(aData, 'cyear', 'ccode', toLag, lag=1)
aData = lagData(aData, 'cyear', 'ccode', toLag, lag=2)

# Calculate difference variables
for(var in toLag){
	# Calculate difference from prev year
	aData$tmp = aData[,var] - aData[,paste0('lag1_', var)]
	names(aData)[ncol(aData)] = paste0('diff_', var)		
	# Calculate percent change from prev year
	aData$tmp = aData[,paste0('diff_',var)] / aData[,paste0('lag1_',var)]
	names(aData)[ncol(aData)] = paste0('pch_', var)		

	# Calculate lagged difference
	aData$tmp = aData[,paste0('lag1_', var)] - aData[,paste0('lag2_', var)]
	names(aData)[ncol(aData)] = paste0('lag1_diff_', var)
	# Calculate percent change lagged
	aData$tmp = aData[,paste0('lag1_diff_',var)] / aData[,paste0('lag2_',var)]
	names(aData)[ncol(aData)] = paste0('lag1_pch_', var)
}

# Drop 2 lag diff vars
aData = aData[,-which( names(aData) %in% paste0('lag2_', toLag) )]
###############################################################

###############################################################
# Bring in upperincome data
load(paste0(pathData,'/Old/modelData.rda'))
ui = modelData[,c('cname', 'upperincome', 'oecd', 'lacus')] %>% unique()
toDrop = setdiff(aData$cname, modelData$cname)
aData = aData[which(!aData$cname %in% toDrop),]

# Subset to post 1987
aData = aData[aData$year>=1987,]
###############################################################

###############################################################
# Save
save(aData, file=paste0(pathBin,'analysisData.rda'))
write.dta(aData, file=paste0(pathData, '/analysisData.dta'))
###############################################################