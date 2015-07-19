####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R') }
####

###################################################################
# Directly loading in data
text=loadText(paste0(pathRaw, 'Newspaper_Stories,_Combined_Papers2015-07-18_20-16.TXT'))
text=text[-1]
###################################################################

###################################################################
textData=pullDate(text)
# textData[557,'storiesDate']='OCTOBER 1, 1997'

# Converting to date format
textData=convertDate(textData)
###################################################################

###################################################################
# Save for analysis
setwd(pathData)
save(textData, file='textData.rda')
###################################################################