if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/Ruthenium/R/setup.R') }

###############################################################
# ICRG data from PRS group
# Manually downloaded through Duke website
icrg=read.csv(paste0(pathDataRaw, 'PRS_Melted_Format.csv'))

# Match to country names in panel
icrg$Country=char(icrg$Country)
icrg$Country[icrg$Country=='Congo-Brazzaville']='Congo, Republic of'
icrg$Country[icrg$Country=='Congo-Kinshasa']='Congo, Democratic Republic of'

# Drop small countries
drop=c("Hong Kong", "New Caledonia")
icrg=icrg[which(!icrg$Country %in% drop),]
icrg$cname=cname(icrg$Country)

# Drop repeat country observations
icrg$drop=0
icrg[icrg$Country=='Serbia and Montenegro' & icrg$Year>=2006, 'drop']=1
icrg[icrg$Country=='Serbia' & icrg$Year<2006, 'drop']=1
icrg[icrg$Country=='Czechoslovakia' & icrg$Year>=1993, 'drop']=1
icrg[icrg$Country=='Czech Republic' & icrg$Year<1993, 'drop']=1
icrg=icrg[icrg$drop==0,]; icrg=icrg[,1:(ncol(icrg)-1)]
icrg[icrg$Country=='Czechoslovakia', 'cname']='CZECH REPUBLIC'

# Create country + year id
icrg$cnameYear=paste(icrg$cname, icrg$Year, sep='')
 
# Check for duplicates
table(icrg$cnameYear)[table(icrg$cnameYear)>1]

# Adding in codes from panel
icrg$ccode=panel$ccode[match(icrg$cname,panel$cname)]
icrg$cyear=paste(icrg$ccode, icrg$Year, sep='')
table(icrg$cyear)[table(icrg$cyear)>1] # Dupe check
###############################################################

###############################################################
# Save
save(icrg, file=paste0(pathDataBin, 'icrg.rda'))
###############################################################