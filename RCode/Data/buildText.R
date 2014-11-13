### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

###################################################################
# Directly loading in data
setwd(paste(pathData, '/Components', sep=''))
con = file('icsidMentions.TXT', 'r')
lines=readLines(con)
close(con)
lines=lines[lines!='']

length(lines)

text=NULL
stCnt=0
for(ii in 1:length(lines)){
	if(substr(lines[ii], 1, 4)!='    ' & substr(lines[ii], 1, 4)!='.   '){
		text=append(text, lines[ii])
		stCnt=stCnt+1
	} else {
		text[stCnt]=paste(text[stCnt], lines[ii])
	}
}
text=text[text!='.']
###################################################################

###################################################################
months='(January|February|March|April|May|June|July|August|September|October|November|December)'
regDate=paste0(months,'( +)[0-9]+,( +)[0-9][0-9][0-9][0-9]')
regDate2=paste0(months,'( +)[0-9][0-9][0-9][0-9]')
regDate3=paste0(months,',( +)[0-9][0-9][0-9][0-9]')

textData=matrix(NA, nrow=length(text), ncol=2, dimnames=list(NULL, c('text', 'textDate')))
for(ii in 1:length(text)){
	date=regmatches(text[ii], regexpr(regDate, text[ii]))
	if(length(date)==0L){
		date=regmatches(text[ii], regexpr(regDate2, text[ii]))
	}
	if(length(date)==0L){
		date=regmatches(text[ii], regexpr(regDate3, text[ii]))
	}	
	if(length(date)==0L){date=''}
	textData[ii,]=cbind(text[ii], date)
}

# Manual cleanup of rest
index=which(textData[,2]=='')
textData[index,2][1]='February 2012'
textData[index,2][2]='April 2013'
textData[index,2][3:4]=NA
textData[index,2][5:6]='November 2014'
textData[index,2][7:9]='May 2009'
###################################################################

###################################################################
# Converting to date format
textData=data.frame(textData)
textData$date=NA
for(ii in 1:nrow(textData)){
	date=as.Date(textData[ii,2], '%B %d, %Y')
	if(is.na(date)){
		txt=gsub('[[:punct:]]','',textData[ii,2])
		txt=gsub(' ', ' 1, ', txt)
		date=as.Date(txt, '%B %d, %Y')
	}
	textData$date[ii]=as.character(date)
}
###################################################################

###################################################################
# Save for analysis
setwd(pathData)
save(textData, file='textData.rda')
###################################################################