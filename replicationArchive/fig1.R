#######################################################################################
### source in the setup.R script using the path on your computer
source('setup.R')

### Load data
load('analysisData.rda')
#######################################################################################

#######################################################################################
# Setting up models
aData$fdiLog2 = logNeg(aData$fdi) ; dv='fdiLog2'

# disputes
dispVars =  c( 'iDispB')
dispLabs = c('ICSID' )
ivDisp=c( paste0('mvs2_',dispVars), paste0('mvs5_',dispVars), paste0(dispVars, 'C') )
ivDispName = c( dName(dispLabs,2), dName(dispLabs,5), dName(dispLabs) )

# Other covariates
ivOther=c(
	'gdpGr', 'gdpCapLog', 'popLog','inflLog',
	'intConf', 'extConf',
	'rbitNoDuplC', 'kaopen', 'polity', 'propRights'
	)
ivs=c(ivDisp, ivOther)
ivAll=lapply(ivDisp, function(x) FUN= c( lagLab(x,1), lagLab(ivOther,1), 'globSumRFDI' ) )

# Setting up variables names for display
ivOtherName=c(
	'\\%$\\Delta$ GDP', 'Ln(GDP per capita)', 'Ln(Pop.)', 'Ln(Inflation)', 
	'Internal Stability','External Stability',
	'Ratified BITs','Capital Openness','Polity', 'Property Rights'
	)
ivsName=lapply(1:length(ivDispName), function(x){ c(ivDispName[x], lagLabName(ivOtherName), 'World FDI') })
#######################################################################################

#######################################################################################
# Simple mod analysis
# By country
getInts = function(res){ 
	c(mu=res[1], hi95=res[1]+qnorm(.975)*res[2], lo95=res[1]-qnorm(.975)*res[2],
		hi90=res[1]+qnorm(.95)*res[2], lo90=res[1]-qnorm(.95)*res[2] )  }
cntries = char( unique(aData$ccode) )
mat = matrix(NA, nrow=length(cntries), ncol=length(ivDisp), dimnames=list(cntries, ivDisp))
csMods = list(rho=mat, mu=mat, hi95=mat, lo95=mat, hi90=mat, lo90=mat)
for(c in cntries){
	slice = aData[aData$ccode==c,]
	for(d in ivDisp){
		if( sum(slice[,d], na.rm=TRUE)>1 ){
			dForm = paste(dv, lagLab(d, 1), sep='~') %>% formula()
			cdCoef = lm(dForm, data=slice) %>% coeftest() %>% .[2,1:2] ; names(cdCoef) = NULL; cdCoef = getInts(cdCoef)
			rho = cor(slice[,c(dv, d)], method='pearson')[1,2]
			csMods[['rho']][c,d] = rho ;csMods[['mu']][c,d] = cdCoef['mu'] ; csMods[['hi95']][c,d] = cdCoef['hi95'] 
			csMods[['lo95']][c,d] = cdCoef['lo95']; csMods[['hi90']][c,d] = cdCoef['hi90'] ; csMods[['lo90']][c,d] = cdCoef['lo90']
		}
	}
}
tmp = lapply(csMods, function(x){ melt(x) %>% mutate(., id=paste0(X1, X2)) %>% na.omit() %>% return(.) })
ggCoef = tmp[['mu']]
for(v in c('rho', as.vector(outer(c('hi','lo'), c('95','90'), paste0)))){
	ggCoef$tmp = tmp[[v]][,3][match(ggCoef$id, tmp[[v]][,4] )] ; names(ggCoef)[ncol(ggCoef)] = v }

# Calculate for full sample using fixed effects & rbind
fullID='All'
fullCoef = lapply(ivDisp, function(d){
	dForm = paste0(paste(dv, lagLab(d,1), sep='~'), ' + factor(ccode)-1') %>% formula()
	dCoef = lm(dForm, data=aData) %>% coeftest() %>% .[1,1:2] ; names(dCoef) = NULL; dCoef = getInts(dCoef) ; names(dCoef)[1] = ''
	rho = cor(aData[,c(dv,d)], method='pearson', use='complete.obs')[1,2]
	c(X1=fullID, X2=d, value=dCoef[1], id=paste0(fullID,d), rho=rho, dCoef[2:length(dCoef)])
	}) %>% do.call('rbind', .) %>% data.frame()
for(v in names(fullCoef)[!names(fullCoef) %in% c('X1', 'X2', 'id')]){ fullCoef[,v] = num(fullCoef[,v]) }
ggCoef$fixefValue = fullCoef$value[match(ggCoef$X2, fullCoef$X2)]

# Adjust var names
ggCoef$X2 = mapVar(ggCoef$X2, ivDisp, ivDispName)
ggCoef$cntry = panel$CNTRY_NAME[match(ggCoef$X1, panel$ccode)] ; ggCoef$cntry[ggCoef$X1==fullID] = fullID
ggCoef$cAbb = countrycode(ggCoef$cntry, 'country.name', 'cowc') ; ggCoef$cAbb[ggCoef$X1==fullID] = fullID
ggCoef$cAbb = factor(ggCoef$cAbb, levels=c(char(sort(unique(ggCoef$cAbb[ggCoef$cAbb!=fullID]))), fullID))

# viz
tmp=ggplot(ggCoef, aes(x=value)) +
	geom_histogram(bins=80, fill='#bdbdbd', color='#969696') +
	geom_vline(xintercept=0, color='red', linetype='solid') +
	scale_x_continuous('$\\beta$ for Dispute Variables', expand = c(0, 0)) +
	scale_y_continuous('Count',expand = c(0, 0), labels=seq(0,20,5), limits=c(0,20)) +
	facet_wrap(~X2, nrow=1, scales='free_x') +
	theme(
			legend.position='none',
			legend.title=element_blank(),
		    axis.ticks=element_blank(),
		    panel.border=element_blank()
    )
ggsave(tmp, file='fig1.pdf', width=8, height=3.5)