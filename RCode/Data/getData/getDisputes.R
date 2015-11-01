if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }

###############################################################
# Load in data
disp = read.dta(paste0(pathRaw, 'UNCTAD_ISDS_LAC_2.dta'))
###############################################################	

###############################################################	
# Add cleaned country identifiers to disp using respondent var

# Drop three cases in which no respondent identified/extra rows
disp = disp[!is.na(disp$respondent),]

# Drop one case that was brought against two countries
disp = disp[disp$respondent!='France; United Kingdom',]

# Add cname and ccode to disp
disp$cname = countrycode(disp$respondent, 'country.name', 'country.name')

# Fix one country labeling error
disp$cname[disp$respondent=='Kyrgystan'] = 'KYRGYZSTAN'

# Add ccodes
disp$ccode = panel$ccode[match(disp$cname, panel$cname)]
###############################################################	

###############################################################	
# Create disputes country-year dataset

# disputes filed under a BIT?
disp$underBIT = as.numeric( grepl("BIT", disp$Legal_Instrument) )

# Disputes brought through any legal instrument
# arbitration tribunal specific variables
## ICSID // UNCITRAL // ICSID-UNCITRAL // All
disp$iDisp = 0 ; disp$iDisp[disp$Arbitration_Rules=='ICSID' | disp$Arbitration_Rules=='ICSID AF'] = 1
disp$uDisp = 0 ; disp$uDisp[disp$Arbitration_Rules=='UNCITRAL'] = 1
disp$iuDisp = 0 ; disp$iuDisp[disp$iDisp==1 | disp$uDisp==1] = 1
disp$disp = 1

# Disputes brought only through BITs
disp$iDispB = 0 ; disp$iDispB[(disp$Arbitration_Rules=='ICSID' | disp$Arbitration_Rules=='ICSID AF') & disp$underBIT==1] = 1
disp$uDispB = 0 ; disp$uDispB[disp$Arbitration_Rules=='UNCITRAL' & disp$underBIT==1] = 1
disp$iuDispB = 0 ; disp$iuDispB[(disp$iDispB==1 | disp$uDispB==1) & disp$underBIT==1] = 1
disp$dispB = 1

# Break out disputes by sector
relSectors = c('oil, gas & mining', 'electricity & power')
disp$oil = 0; disp$oil[disp$sector==relSectors[1]] = 1
disp$elec = 0; disp$elec[disp$sector==relSectors[2]] = 1
disp$oilElec = 0; disp$oilElec[disp$sector %in% relSectors] = 1
# Split by arbitration tribunal
disp$iOil = 0; disp$iOil[disp$oil==1 & disp$iDisp==1] = 1
disp$uOil = 0; disp$uOil[disp$oil==1 & disp$uDisp==1] = 1
disp$iuOil = 0; disp$iuOil[disp$oil==1 & disp$iuDisp==1] = 1
#
disp$iElec = 0; disp$iElec[disp$elec==1 & disp$iDisp==1] = 1
disp$uElec = 0; disp$uElec[disp$elec==1 & disp$uDisp==1] = 1
disp$iuElec = 0; disp$iuElec[disp$elec==1 & disp$iuDisp==1] = 1
#
disp$iOilElec = 0; disp$iOilElec[disp$oilElec==1 & disp$iDisp==1] = 1
disp$uOilElec = 0; disp$uOilElec[disp$oilElec==1 & disp$uDisp==1] = 1
disp$iuOilElec = 0; disp$iuOilElec[disp$oilElec==1 & disp$iuDisp==1] = 1
# Split by whether dispute was filed under a bit
disp$iOilB = 0 ; disp$iOilB[disp$iOil==1 & disp$underBIT==1] = 1
disp$uOilB = 0 ; disp$uOilB[disp$uOil==1 & disp$underBIT==1] = 1
disp$iuOilB = 0 ; disp$iuOilB[disp$iuOil==1 & disp$underBIT==1] = 1
#
disp$iElecB = 0 ; disp$iElecB[disp$iElec==1 & disp$underBIT==1] = 1
disp$uElecB = 0 ; disp$uElecB[disp$uElec==1 & disp$underBIT==1] = 1
disp$iuElecB = 0 ; disp$iuElecB[disp$iuElec==1 & disp$underBIT==1] = 1
#
disp$iOilElecB = 0 ; disp$iOilElecB[disp$iOilElec==1 & disp$underBIT==1] = 1
disp$uOilElecB = 0 ; disp$uOilElecB[disp$uOilElec==1 & disp$underBIT==1] = 1
disp$iuOilElecB = 0 ; disp$iuOilElecB[disp$iuOilElec==1 & disp$underBIT==1] = 1

# Subset to relevant variables
dispVars = apply(
	expand.grid(
		c('iDisp', 'uDisp', 'iuDisp', 'disp', 
			'iOil', 'uOil', 'iuOil', 
			'iElec', 'uElec', 'iuElec',
			'iOilElec', 'uOilElec', 'iuOilElec'),
		c('','B')
	), 1, function(x){ paste0(x[1],x[2]) })
relVars = c( 'ccode', 'startyear', dispVars)
disp = disp[,relVars]

# Aggregate to country year level
dispCnt = summaryBy(.~ccode+startyear, data=disp, FUN=sum, keep.names=TRUE)
dispCnt$cyear = paste0(dispCnt$ccode, dispCnt$startyear)

# Drop one observation in 2015
dispCnt = dispCnt[dispCnt$startyear<=2014,]

# Merge with full coutnry year panel
panel = panel[panel$year>=1970,] # use 1970 for now so moving sums dont create NAs
# Relabel one variable in panel
names(panel)[7] = 'cyear'
# Add 2013 and 2014 to panel (assumes '13-14 cntries = '12 cntries)
p13 = panel[panel$year==2012,]; p13$year = 2013
p13$cyear = paste0(p13$ccode, p13$year); p13$cnameYear = paste0(p13$cname, p13$year)
panel = rbind(panel, p13); rm(list='p13')
p14 = panel[panel$year==2012,]; p14$year = 2014
p14$cyear = paste0(p14$ccode, p14$year); p14$cnameYear = paste0(p14$cname, p14$year)
panel = rbind(panel, p14); rm(list='p14')

dispF = merge(panel, dispCnt[,3:ncol(dispCnt)], by='cyear', all.x=TRUE, all.y=FALSE)
dispF[is.na(dispF)] = 0
###############################################################

###############################################################
# Create cumulative variables
dispF = cumulTS(dispF, 'ccode', 'year', dispVars)
# Create two year moving sum
dispF = movePanel(dispF, 'ccode', 'year', dispVars, 2, sum=TRUE)
# Create five year moving sum
dispF = movePanel(dispF, 'ccode', 'year', dispVars, 5, sum=TRUE)

# Subset to 1984 and beyond
dispF = dispF[dispF$year>=1984,]
###############################################################

###############################################################
# Save
disputes = dispF
save(disputes, file=paste0(pathBin, 'disputes.rda'))
###############################################################