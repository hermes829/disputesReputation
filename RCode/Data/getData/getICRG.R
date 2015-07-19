####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

###############################################################
# Function to clean data data
icrgCleaner = function(data,sheetNum){
	# Match to country names in panel
	data$Country=char(data$Country)
	data$Country[data$Country=='Congo-Brazzaville']='Congo, Republic of'
	data$Country[data$Country=='Congo']='Congo, Republic of'
	data$Country[data$Country=='Congo-Kinshasa']='Congo, Democratic Republic of'
	data$Country[data$Country=='Congo, DR']='Congo, Democratic Republic of'
	data$Country[data$Country=='UAE'] = 'United Arab Emirates'

	# Drop small countries
	drop=c("Hong Kong", "New Caledonia")
	data=data[which(!data$Country %in% drop),]
	data$cname=cname(data$Country)

	# Corrections
	data$cname[data$Country=='East Germany'] = 'German Democratic Republic'

	# Drop repeat country observations
	data$drop=0
	data[data$Country=='Russia' & data$year<1992,'drop'] = 1
	data[data$Country=='USSR' & data$year>=1992,'drop'] = 1
	data[data$Country=='Germany' & data$year<1990,'drop'] = 1
	data[data$Country=='West Germany' & data$year>=1990,'drop'] = 1
	data[data$Country=='East Germany' & data$year>=1990,'drop'] = 1
	data[data$Country=='Serbia and Montenegro' & data$year>=2006, 'drop']=1
	data[data$Country=='Serbia' & data$year<2006, 'drop']=1
	data[data$Country=='Serbia & Montenegro *' & data$year>=2006, 'drop']=1
	data[data$Country=='Serbia *' & data$year<2006, 'drop']=1	
	data[data$Country=='Czechoslovakia' & data$year>=1993, 'drop']=1
	data[data$Country=='Czech Republic' & data$year<1993, 'drop']=1
	data=data[data$drop==0,]; data=data[,1:(ncol(data)-1)]
	data[data$Country=='Czechoslovakia', 'cname']='CZECH REPUBLIC'

	# Create country + year id
	data$cnameYear=paste(data$cname, data$year, sep='')
	 
	# Check for duplicates
	cat(paste0('cyearDupe ',sheetNum, ': ')); cat( table(data$cnameYear)[table(data$cnameYear)>1] ) ; cat('\n')

	# Adding in codes from panel
	data$ccode=panel$ccode[match(data$cname,panel$cname)]
	data$cyear=paste(data$ccode, data$year, sep='')
	cat(paste0('cyearDupe ',sheetNum, ': ')); cat( table(data$cyear)[table(data$cyear)>1] ) ; cat('\n')

	# return cleaned data
	if(sheetNum<3){ return(data) } else { return(data[,c(3,7)]) }
}
###############################################################

###############################################################
# ICRG data from PRS group
# Manually downloaded through Duke website
icrgName = paste0(pathRaw, "3BResearchersDataset2015.xlsx")
icrgL = lapply(2:13, function(ii){
	var = read.xlsx(icrgName, sheet=ii)[3,1]
	dat = read.xlsx(icrgName, sheet=ii, startRow=8)
	mdat = melt(dat, id='Country')
	for(v in c('variable','value')){ mdat[,v] = num(mdat[,v]) }
	names(mdat)[2:3] = c('year', var)
	mdat = icrgCleaner(data=mdat, sheetNum=ii)
	return(mdat)
})

# Reduce to dataframe
icrg = icrgL[[1]]
for(ii in icrgL[2:length(icrgL)]){
	icrg$tmp = ii[,1][match(icrg$cyear, ii[,2])]
	names(icrg)[ncol(icrg)] = names(ii)[1] }
###############################################################

###############################################################
# Save
save(icrg, file=paste0(pathBin, 'icrg.rda'))
###############################################################