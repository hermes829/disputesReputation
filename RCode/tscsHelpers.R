# The function to use is lagDataSM
# This takes a dataset, a variable country year which is 
# a concatenation of the country identifier and year
# Then a country variable
# The varsTOlag should be inputted as a vector
# And lag is just a numeric specifying how many years to lag the variable
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagDataSM <- function(data, country_year, country, varsTOlag, lag)
{
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2, 
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) ) 
    } )
  colnames(lagData) <- paste('lag', lag, '_', varsTOlag, sep='')
  cbind(data, lagData)
}

# Order panel dataset
orderTS=function(x, unit, time){ x=x[order(x[,time]),]; x=x[order(x[,unit]),]; x }

# calculate moving averages/sums for panel datasets
movePanel <- function(data, ccode, time, vars, wdow, sum=FALSE){
	data=orderTS(data, ccode, time)
	vars2 <- paste('mva',wdow,'_',vars,sep='')
	vars3 <- paste('mvs',wdow,'_',vars,sep='')

	# Countries with fewer observed periods than wdow
	toDrop = table(data$ccode) %>% cbind() %>% data.frame() %>% cbind(rownames(.), .) %>% .[order(.[,2]),] %>% .[.[,2]<wdow,] %>% .[,1] %>% num()	
	d2 = data[which(!data$ccode %in% toDrop),]

	# Create moving sums
	rollmeanSM <- function(x){ as.vector(filter(x, rep(1/wdow, wdow), sides=1)) }
	temp <- ddply(d2[,c(ccode, vars)], .(ccode), numcolwise(rollmeanSM))

	# Cleanup 
	if(sum==FALSE){
		temp <- temp[,vars]; colnames(temp) <- vars2 }else{
		temp <- temp[,vars]*wdow; colnames(temp) <- vars3 }
	
	# Return if no data to be added back in
	if(length(toDrop)==0){ return( cbind(d2, temp) ) }

	# Add back in dropped data and return result
	if(length(toDrop)!=0){
		d2 = cbind(d2, temp)
		dAdd = data[which(data$ccode %in% toDrop),]
		missVals = matrix(NA, nrow=nrow(dAdd), ncol=ncol(temp), dimnames=list(NULL, colnames(temp)))
		dAdd = cbind(dAdd, missVals)
		return(rbind(d2, dAdd))
	}
}

# Calculate cumulative sum of var
cumulTS=function(data, ccode, time, vars){
	data=orderTS(data, ccode, time)	
	vars2=paste0(vars,'C')
	cumsumSM=function(x){x[is.na(x)]=0; cumsum(x)}
	temp=ddply(data[,c(ccode,vars)], .(ccode), cumsumSM)
	temp=temp[,vars]; colnames(temp)=vars2
	temp[which(is.na(data[,vars]),arr.ind=TRUE)]=NA
	cbind(data, temp) 
}

# Check balance of a panel
panelBalance=function(ivs, dv, group, time, regData){
	Avars=c(ivs, dv, group, time)
	data=na.omit(regData[,Avars])[,c(group, time)]
	obsGroup=by(data, data[,group], nrow)
	obsGroup=data.frame(cbind(ccode=names(obsGroup), obs=as.vector(obsGroup)))
	obsGroup$obs=numSM(obsGroup$obs)
	obsGroup[order(obsGroup[,2], decreasing=F),]	
}