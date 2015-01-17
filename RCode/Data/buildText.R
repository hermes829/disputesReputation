### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###################################################################
# Directly loading in data
setwd(paste(pathData, '/Components', sep=''))
text=loadText('icsidMentions.TXT')
###################################################################

###################################################################
textData=pullDate(text)

# Manual cleanup of rest
index=which(textData[,2]=='')
textData[index,2][1]='February 2012'
textData[index,2][2]='April 2013'
textData[index,2][3:4]=NA
textData[index,2][5:6]='November 2014'
textData[index,2][7:9]='May 2009'

# Converting to date format
textData=convertDate(textData)
###################################################################

###################################################################
# Save for analysis
setwd(pathData)
save(textData, file='textData.rda')
###################################################################

###################################################################
### Load setup
source('~/Research/RemmerProjects/disputesReputation/RCode/setup.R')
###################################################################

###################################################################
# Directly loading in data
setwd(paste(pathData, '/Components', sep=''))
text=loadText('icsidGenMentions.TXT')
text=text[-1]
###################################################################

###################################################################
textData=pullDate(text)
textData[557,'storiesDate']='OCTOBER 1, 1997'

# Converting to date format
textData=convertDate(textData)
###################################################################

###################################################################
# Save for analysis
setwd(pathData)
save(textData, file='textDatav2.rda')
###################################################################