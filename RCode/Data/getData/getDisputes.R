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

# Subset to relevant variables
dispVars = apply(expand.grid(c('iDisp', 'uDisp', 'iuDisp', 'disp'),c('','B')), 1, function(x){ paste0(x[1],x[2]) })
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