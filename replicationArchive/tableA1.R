###
# REPLICATION OF ALLEE AND PEINHARDT
# RERUNNING MODEL 4.2 IN TABLE 4 AND CONDUCTING ROBUSTNESS CHECKS
###

#######################################################################################
### source in the setup.R script using the path on your computer
source('~/Research/disputesReputation/replicationArchive/setup.R')

### Load data
load('analysisData.rda')
#######################################################################################


####
#Loading Allee Peinhardt Data
####
fdi = read.dta("Allee and Peinhardt IO 2011 data.dta")
setwd(pathData)
load('forAnalysis.rda')

fdi$cname=countrycode(fdi$country, 'country.name', 'country.name')
# fdi$rbitcount=allData$ratifiedbits[match(fdi$cname,allData$cname)]

###
# CORRECTING MISTAKE IN HOW fdi_inflows WAS LOGGED
###
# fdi$lnfdiSM = logNeg(fdi$fdi_inflows) # Log abs value of neg and then mult neg 1 before recombining
fdi$lnfdiSM = log( fdi$fdi_inflows+abs(min(fdi$fdi_inflows,na.rm=T))+.0001 ) # add constant to make neg >0 and then log
# fdi$lnfdiSM = fdi$fdi_inflows # No log

# Lead DV forward one year
fdi$cyear = numSM(paste0(fdi$ifscode, fdi$year))
tmp=fdi[,c('ifscode', 'year','lnfdiSM', 'lnfdi', 'worldfdi')]
names(tmp)[3:5]=paste0('lead1_', names(tmp)[3:5])
tmp$year=tmp$year-1
tmp$cyear = numSM(paste0(tmp$ifscode, tmp$year))
fdi=merge(fdi, tmp[,3:ncol(tmp)], by='cyear', all.x=TRUE)

# Specifying DV
# fdi$DV = fdi[,'lead1_lnfdi'] # Allee and Peinhardt Approach
fdi$DV = fdi[,'lead1_lnfdiSM'] # Corrected
fdi$Fworldfdi=fdi[,'lead1_worldfdi']

### 
# Checking on footnote 72
###
f72=fdi[which(fdi$cname %in% c('Gambia', 'Peru')),]
setwd(pathGraphics)
pdf(file='gambiaPeruFDI.pdf', width=12, height=10)
ggplot(f72, aes(x=year, y=fdi_inflows))+geom_line()+facet_wrap(~cname, ncol=1,scales='free_y')
dev.off()

pdf(file='gambiaPeruLnFDI.pdf', width=12, height=10)
ggplot(f72, aes(x=year, y=lnfdi))+geom_line()+facet_wrap(~cname, ncol=1,scales='free_y')
dev.off()

####
# RUNNING MODELS
#####
mNames=c('Lose/Settle (2 years)', 'Lose/Settle (5 years)', 
  'Lost (2 years)', 'Lost (5 years)' )
# kivs=c('lose_settle2', 'lose_settle5', 'losticsid2', 'losticsid5')
kivs=c('losticsid2')
coefsRobustList=list()
resultsList=list()
fdi=fdi[fdi$year>1983,]
for(ii in 1:length(kivs)){
  #Note: variables for analysis
  regVars = c( 'DV',
    'bitcount', 
    kivs[ii], 
    'cbdcrisis', 
    'domestic1_8', 'external_conflict', 'polity2', 'proprights',
    'lnpop_total', 'lngdppc', 'gdp_grow', 'offxrate_lcudif', 'kaopen',
    'Fworldfdi',
    'oecd', 'ifscode', 'country', 'year')
  regData = na.omit(fdi[,c(regVars, 'fdi_inflows')])
  regData = regData[regData$oecd!=1,]

# xtreg F.lnfdi bitcount losticsid2 cbdcrisis domestic1_8 external_conflict polity2
# proprights lnpop_total lngdppc gdp_grow offxrate_lcudif kaopen F.worldfdi if oecd~=1,
# fe robust cluster (ifscode)

  ####
  # Setting up model
  #####
  ap_model = formula(paste('DV ~ bitcount +', kivs[ii],
      ' + cbdcrisis + domestic1_8 + 
            external_conflict + polity2 + proprights + 
            lnpop_total + lngdppc + gdp_grow + offxrate_lcudif +
            kaopen + Fworldfdi +
            factor(ifscode)-1'))
  ap_modelVars = 13

  regModel = lm(ap_model, data = regData)
  robustRegModel = clx(regModel, regData$date, regData$country)
  regModelSE = robustRegModel[[2]]
  vcovCL = robustRegModel[[1]]
  error = summary(regModel)$sigma

  resultsList[[ii]]=regModel
	coefsRobustList[[ii]]=regModelSE
}

Acoefs=c(kivs, regVars[c(2,4:14)])
apTables = lmtableSM(coefs=Acoefs, vnames=Acoefs,
 modelResults=resultsList, modelSumm=coefsRobustList, modelNames=mNames)
apTables