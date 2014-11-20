loadText=function(fileName){
	con = file(fileName, 'r')
	lines=readLines(con)
	close(con)
	lines=lines[lines!='']

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
	text	
}

pullDate=function(stories){
	months='(January|February|March|April|May|June|July|August|September|October|November|December)'
	regDate=paste0(months,'( +)[0-9]+,( +)[0-9][0-9][0-9][0-9]')
	regDate2=paste0(months,'( +)[0-9][0-9][0-9][0-9]')
	regDate3=paste0(months,',( +)[0-9][0-9][0-9][0-9]')

	storiesData=matrix(NA, nrow=length(stories), ncol=2, dimnames=list(NULL, c('stories', 'storiesDate')))
	for(ii in 1:length(stories)){
		date=regmatches(stories[ii], regexpr(regDate, stories[ii]))
		if(length(date)==0L){
			date=regmatches(stories[ii], regexpr(regDate2, stories[ii]))
		}
		if(length(date)==0L){
			date=regmatches(stories[ii], regexpr(regDate3, stories[ii]))
		}	
		if(length(date)==0L){date=''}
		storiesData[ii,]=cbind(stories[ii], date)
	}
	storiesData
}

convertDate=function(stories_date_matrix){
	stories_date_matrix=data.frame(stories_date_matrix)
	stories_date_matrix$date=NA
	for(ii in 1:nrow(stories_date_matrix)){
		date=as.Date(stories_date_matrix[ii,2], '%B %d, %Y')
		if(is.na(date)){
			txt=gsub('[[:punct:]]','',stories_date_matrix[ii,2])
			txt=gsub(' ', ' 1, ', txt)
			date=as.Date(txt, '%B %d, %Y')
		}
		stories_date_matrix$date[ii]=as.character(date)
	}
	stories_date_matrix	
}