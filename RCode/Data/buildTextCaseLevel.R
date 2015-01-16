### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###################################################################
# Function to run on all files
cleanBunch=function(fileName){
	text=loadText(fileName)
	textData=pullDate(text)
	textData=convertDate(textData)
	textData$cntry=strsplit(fileName, '_')[[1]][1]
	textData$cname=countrycode(textData$cntry, 'country.name','country.name')
	textData$icsid=gsub('.TXT','',strsplit(fileName, '_')[[1]][2])
	textData$icsidYr=gsub(':','',
		regmatches(textData$icsid, 
			regexpr(':[0-9]+:',textData$icsid) ) )
	pre2000=which(numSM(textData$icsidYr)>15)
	post2000=which(numSM(textData$icsidYr)<15)
	textData$icsidYr[pre2000] = numSM(paste0('19',textData$icsidYr[pre2000]))
	textData$icsidYr[post2000] = numSM(paste0('20',textData$icsidYr[post2000]))
	unique(textData)
}

pathTxts='~/Dropbox/Research/RemmerProjects/disputesMedia/data/cntry_case'
setwd(pathTxts)
caseTextData=lapply(list.files(), cleanBunch)
caseTextData=do.call('rbind', caseTextData)
###################################################################

###################################################################
# Save
setwd(pathData)
save(caseTextData, file='caseTextData.rda')
###################################################################