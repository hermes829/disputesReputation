###
# REPLICATION OF ALLEE AND PEINHARDT
# RERUNNING MODEL 4.2 IN TABLE 4 AND CONDUCTING ROBUSTNESS CHECKS
###

# Setting workspace
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

####
#Loading Allee Peinhardt Data
####
setwd(paste(pathData, '/allee-peinhardt2011io.zip Folder',sep=''))
fdi = read.dta("Allee and Peinhardt IO 2011 data.dta")

###
# CORRECTING MISTAKE IN HOW fdi_inflows WAS LOGGED
###
fdi$lnfdiSM = log( fdi$fdi_inflows+abs(min(fdi$fdi_inflows,na.rm=T))+.0001 ) # add constant to make neg >0 and then log

# Lead DV forward one year
fdi$cyear = numSM(paste0(fdi$ifscode, fdi$year))
tmp=fdi[,c('ifscode', 'year','lnfdiSM', 'lnfdi', 'worldfdi')]
names(tmp)[3:5]=paste0('lead1_', names(tmp)[3:5])
tmp$year=tmp$year-1
tmp$cyear = numSM(paste0(tmp$ifscode, tmp$year))
fdi=merge(fdi, tmp[,3:ncol(tmp)], by='cyear', all.x=TRUE)

# Specifying DV
fdi$Fworldfdi=fdi[,'lead1_worldfdi']
fdi$DV = fdi[,'lead1_lnfdi'] # Allee and Peinhardt Approach
# fdi$DV = fdi[,'lead1_lnfdiSM'] # Corrected

# Drop pre 1983
fdi=fdi[fdi$year>1983,]

####
# RUNNING MODELS
#####
# Original stata code from A & P
# xtreg F.lnfdi bitcount losticsid2 cbdcrisis domestic1_8 external_conflict polity2
# proprights lnpop_total lngdppc gdp_grow offxrate_lcudif kaopen F.worldfdi if oecd~=1,
# fe robust cluster (ifscode)
mNames=c('Pending', 'Filed (2 years)', 'Filed (5 years)')
kivs=c('pending', 'numreg2', 'numreg5')
plmList=list()

for(ii in 1:length(kivs)){
  # Data
  regVars = c( 'DV', 'bitcount', kivs[ii], 
    'cbdcrisis', 'domestic1_8', 'external_conflict', 'polity2',
    'proprights', 'lnpop_total', 'lngdppc', 'gdp_grow', 'offxrate_lcudif',
    'kaopen', 'Fworldfdi', 'oecd', 'ifscode', 'country', 'year')
  regData = na.omit(fdi[,c(regVars, 'fdi_inflows')])
  regData = regData[regData$oecd!=1,]

  # Model
  plmData=pdata.frame(regData, index=c('ifscode', 'year'))
  plmForm = formula(paste('DV ~ bitcount +', kivs[ii],
    ' + cbdcrisis + domestic1_8 + 
    external_conflict + polity2 + proprights + 
    lnpop_total + lngdppc + gdp_grow + offxrate_lcudif +
    kaopen + Fworldfdi'))
  plmList[[ii]]=plm(plmForm, data=plmData, model='within')
}

lapply(plmList, function(x) round(coeftest(x, vcov=vcovHC(x, type='HC1')),3) )