# Creating coefficient plots

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

##########################################################################################
# Loading model results
setwd(pathResults)
load('invProfRE.rda')
# load('propRightsRE.rda')
##########################################################################################

##########################################################################################
# Extracting coef estims and serrors from lmer object
tabResults=lapply(modResults, function(x){FUN=
		estims=x@fixef; serrors=sqrt(diag(vcov(x)))
		cbind('Estimate'=estims, 'Std. Error'=serrors) } )
##########################################################################################

##########################################################################################
# Creating coefficient plots
ggcoefplot(coefData=tabResults[[1]], vars=ivAll[[1]], varNames=ivAllNames[[1]], 
	Noylabel=FALSE, coordFlip=TRUE,
    specY=TRUE, ggylims=c(-4,4), ggybreaks=seq(-4,4,1),
    colorGrey=FALSE, grSTA=0.5, grEND=0.1)
##########################################################################################