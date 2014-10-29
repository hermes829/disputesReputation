###
# REPLICATION OF ALLEE AND PEINHARDT
# RERUNNING MODEL 4.2 IN TABLE 4 AND CONDUCTING ROBUSTNESS CHECKS
###

# Setting workspace
if(Sys.info()['user']=='janus829'){
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')}
if(Sys.info()['user']=='s7m'){
source('/Users/s7m/Research/RemmerProjects/disputesReputation/RCode/setup.R')}


####
#Loading Allee Peinhardt Data
####
setwd(paste(pathData, '/allee-peinhardt2011io.zip Folder',sep=''))
fdi = read.dta("Allee and Peinhardt IO 2011 data.dta")
setwd(pathData)
load('forAnalysis.rda')

fdi$cname=countrycode(fdi$country, 'country.name', 'country.name')
# fdi$rbitcount=allData$ratifiedbits[match(fdi$cname,allData$cname)]

###
# CORRECTING MISTAKE IN HOW fdi_inflows WAS LOGGED
###
# fdi$lnfdiSM = logNeg(fdi$fdi_inflows) # Log abs value of neg and then mult neg 1 before recombining
fdi$lnfdiSM = log( fdi$fdi_inflows+abs(min(fdi$fdi_inflows,na.rm=T))+.0001 ) # add constant to make neg >0 and then log
# fdi$lnfdiSM = fdi$fdi_inflows # No log

# Lead DV forward one year
fdi$cyear = numSM(paste0(fdi$ifscode, fdi$year))
tmp=fdi[,c('ifscode', 'year','lnfdiSM', 'lnfdi', 'worldfdi')]
names(tmp)[3:5]=paste0('lead1_', names(tmp)[3:5])
tmp$year=tmp$year-1
tmp$cyear = numSM(paste0(tmp$ifscode, tmp$year))
fdi=merge(fdi, tmp[,3:ncol(tmp)], by='cyear', all.x=TRUE)

# Specifying DV
# fdi$DV = fdi[,'lead1_lnfdi'] # Allee and Peinhardt Approach
fdi$DV = fdi[,'lead1_lnfdiSM'] # Corrected
fdi$Fworldfdi=fdi[,'lead1_worldfdi']

### 
# Checking on footnote 72
###
f72=fdi[which(fdi$cname %in% c('Gambia', 'Peru')),]
ggplot(f72, aes(x=year, y=fdi_inflows, color=cname))+geom_line()

####
# RUNNING MODELS
#####
mNames=c('Lose/Settle (2 years)', 'Lose/Settle (5 years)', 
  'Lost (2 years)', 'Lost (5 years)' )
# kivs=c('lose_settle2', 'lose_settle5', 'losticsid2', 'losticsid5')
kivs=c('losticsid2')
coefsRobustList=list()
resultsList=list()
fdi=fdi[fdi$year>1983,]
for(ii in 1:length(kivs)){
  #Note: variables for analysis
  regVars = c( 'DV',
    'bitcount', 
    kivs[ii], 
    'cbdcrisis', 
    'domestic1_8', 'external_conflict', 'polity2', 'proprights',
    'lnpop_total', 'lngdppc', 'gdp_grow', 'offxrate_lcudif', 'kaopen',
    'Fworldfdi',
    'oecd', 'ifscode', 'country', 'year')
  regData = na.omit(fdi[,c(regVars, 'fdi_inflows')])
  regData = regData[regData$oecd!=1,]

# xtreg F.lnfdi bitcount losticsid2 cbdcrisis domestic1_8 external_conflict polity2
# proprights lnpop_total lngdppc gdp_grow offxrate_lcudif kaopen F.worldfdi if oecd~=1,
# fe robust cluster (ifscode)

  ####
  # Setting up model
  #####
  ap_model = formula(paste('DV ~ bitcount +', kivs[ii],
      ' + cbdcrisis + domestic1_8 + 
            external_conflict + polity2 + proprights + 
            lnpop_total + lngdppc + gdp_grow + offxrate_lcudif +
            kaopen + Fworldfdi +
            factor(ifscode)-1'))
  ap_modelVars = 13

  regModel = lm(ap_model, data = regData)
  robustRegModel = clx(regModel, regData$date, regData$country)
  regModelSE = robustRegModel[[2]]
  vcovCL = robustRegModel[[1]]
  error = summary(regModel)$sigma

  resultsList[[ii]]=regModel
	coefsRobustList[[ii]]=regModelSE
}

Acoefs=c(kivs, regVars[c(2,4:14)])
apTables = lmtableSM(coefs=Acoefs, vnames=Acoefs,
 modelResults=resultsList, modelSumm=coefsRobustList, modelNames=mNames)
apTables

setwd(pathLatex)
print.xtable(xtable(apTables, align='llcccc',
  caption='Fixed effects regression on Logged FDI inflows from Allee and Peinhardt Table 4. Robust standard errors in parentheses, $^**$ and $^*$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
  ), include.rownames=FALSE,
  sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE),
  hline.after=c(0,0,8,32,32), 
  file='apTables.tex')

####
#COEFFICIENT PLOT FOR MODEL 4.2
#####
setwd(pathGraphics)
varNames = c("Cumul. BITs Signed$_{t-1}$", "ICSID Rulings Lost\n(past 2 years)$_{t-1}$",
 "Domestic\nEcon. Shocks$_{t-1}$", "Domestic\n Pol. Shocks$_{t-1}$",
 "Lack of\nExternal Threat$_{t-1}$", "Democracy$_{t-1}$",
 "Property Rights\nProtection$_{t-1}$", "Ln(Population)$_{t-1}$",
 "Ln(GDP per Capita)$_{t-1}$", "Economic\nGrowth$_{t-1}$",
 "Financial\nOpenness$_{t-1}$", "Exchange Rate\nVolatility$_{t-1}$",
 "Total World FDI$_{t-1}$")

results = regModelSE[1:ap_modelVars,]
rownames(results) = varNames
results
# xtable(results)

temp = ggcoefplot(coefData=regModelSE[1:ap_modelVars,], 
  vars=regVars[1:(ap_modelVars+1)], varNames=varNames, Noylabel=FALSE, coordFlip=TRUE,
  specY=TRUE, ggylims=c(-4.5,3), ggybreaks=seq(-4.5, 3, 1.5),    
  colorGrey=FALSE, grSTA=0.5, grEND=0.1)
temp
# tikz("AlleePeinhardtModel4.tex",width=7, height=7,standAlone=F)
# tikz("Correct_AlleePeinhardtModel4.tex",width=7, height=7,standAlone=F)
temp
# dev.off()

####
#RUNNING SIMULATIONS ON MODEL 4.2
#####
setwd(pathGraphics)
temp = ggsimplot(sims=10000, simData=regData, vars=regVars[2:ap_modelVars], 
  vi='bitcount', vRange=0:105, ostat=median,
  betas=regModelSE[, 1], vcov=vcovCL, sigma=error, intercept=FALSE,
  ylabel="Ln(Net FDI Inflows)$_{t}$", xlabel="BITs Signed$_{t-1}$",
  specX=TRUE, ggxbreaks=seq(0,105,10))
temp
# tikz("BITsSimMod4vAP.tex",width=4, height=3,standAlone=F)
temp
# dev.off()

temp = ggsimplot(sims=10000, simData=regData, vars=regVars[2:ap_modelVars], 
  vi='losticsid2', vRange=0:3, ostat=median,
  betas=regModelSE[, 1], vcov=vcovCL, sigma=0, intercept=FALSE,
  ylabel="Ln(Net FDI Inflows)$_{t}$", xlabel="ICSID Rulings Lost (past 2 years)",
  specX=TRUE, ggxbreaks=seq(0,3,1))
temp
# tikz("DisputesSimMod4vAP.tex",width=4, height=3,standAlone=F)
temp
# dev.off()

# Plotting density distributions
vi='losticsid2'
sims=10000
simData=regData
vars=regVars[2:ap_modelVars]
vRange=c(min(regData[,vi]),max(regData[,vi]))
ostat=median
betas=regModelSE[, 1]
vcov=vcovCL
sigma=error
intercept=FALSE
ylabel="Ln(Net FDI Inflows)$_{t}$"
xlabel="ICSID Rulings Lost (past 2 years)"
specX=TRUE
ggxbreaks=seq(0,3,1)

# Set up scenario
scenCol = length(vars); scenRow = length(vRange)
scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
colnames(scenario) = c(vars)
scenario[,vi] = vRange

viPos = which(vi==vars)
ovals = apply(simData[,vars[-viPos]], 2, ostat)
scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
vars2 = colnames(scenario)

draws = mvrnorm(n = sims, betas[vars2], vcov[vars2,vars2])
modelPreds = draws %*% t(scenario)
# modelPreds = regModelSE[vars2,1] %*% t(scenario)
# modelExp = apply(modelPreds, 2, function(x) FUN=rnorm(sims, x, sigma))
modelExp = exp( 10 + modelPreds + error^2/2  )

colnames(modelExp)=1:ncol(modelExp)
modelExp2=melt(modelExp)[,-1]
ggMeans = ddply(modelExp2, .(X2), summarise, sMean=mean(value))
ggDensity = ddply(modelExp2, .(X2), .fun=function(x){
  tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
  q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
  q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
  data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

ggMeans$X2 = as.factor(ggMeans$X2)
ggDensity$X2 = as.factor(ggDensity$X2)

temp = ggplot()
temp = temp + geom_line(data=ggDensity, aes(x=x,y=y, linetype=X2), color='black')
# temp = temp + geom_vline(data=ggMeans,
#   aes(xintercept=sMean, color=X2),linetype='solid',size=1)
# temp = temp + geom_ribbon(data=subset(ggDensity,q95),
#   aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.5)
# temp = temp + geom_ribbon(data=subset(ggDensity,q90),
#   aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.9)
temp = temp + xlab("Net FDI Inflows -- Outflows$_{t}$") + ylab('Density')
temp = temp + theme(legend.position='none', legend.title=element_blank(),
  axis.ticks = element_blank(), 
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
  axis.title.x = element_text(vjust=-0.2), 
  axis.title.y = element_text(vjust=0.2),
  panel.border = element_blank(), axis.line = element_line(color='black'))
# temp=temp+scale_color_grey(start=0.2,end=0.8)+scale_fill_grey(start=0.2,end=0.8)
temp
# tikz("BITsSimMod4vAP.tex",width=5, height=3,standAlone=F)
# tikz("DisputesSimMod4vAP.tex",width=5, height=3,standAlone=F)
temp
# dev.off()