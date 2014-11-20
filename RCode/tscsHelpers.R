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
	rollmeanSM <- function(x){ as.vector(filter(x, rep(1/wdow, wdow), sides=1)) }
	temp <- ddply(data[,c(ccode, vars)], .(ccode), numcolwise(rollmeanSM))
	if(sum==FALSE){
		temp <- temp[,vars]; colnames(temp) <- vars2 }else{
		temp <- temp[,vars]*wdow; colnames(temp) <- vars3 }
	cbind(data, temp)
}

# Calculate cumulative sum of var
cumulTS=function(data, ccode, time, vars){
	data=orderTS(data, ccode, time)	
	vars2=paste('C',vars,sep='')
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