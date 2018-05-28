#######################################################################################
### source in the setup.R script using the path on your computer
source('setup.R')

### Load data
fdi = read.dta("Allee and Peinhardt IO 2011 data.dta")
sink('tableA1.txt', append=TRUE)
#######################################################################################

# stata like robust clustered standard errors
# code from:
# https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
cl   <- function(dat,fm, cluster){
  attach(dat, warn.conflicts = F)
  loadPkg('sandwich')
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

# remove OECD countries
fdi = fdi[fdi$oecd!=1,]

# push forward fdi measure and add correctly logged fdi measure
tmp = fdi[,c('ifscode','year','lnfdi', 'worldfdi', 'fdi_inflows')]
tmp$lnfdi_correct = log( tmp$fdi_inflows+abs(min(tmp$fdi_inflows,na.rm=T))+1 )
tmp$year = tmp$year - 1
tmp$id = with(tmp, paste(ifscode, year, sep='_'))
fdi$id = with(fdi, paste(ifscode, year, sep='_'))
for(v in names(tmp)[c(3:ncol(tmp))]){
  fdi$tmp = tmp[match(fdi$id, tmp$id),v]
  names(fdi)[ncol(fdi)] = paste0('F.', v) }


# A & P: (1)
ivs = c(
  'bitcount', 'pending','cbdcrisis', 'domestic1_8', 'external_conflict', 'polity2',
  'proprights', 'lnpop_total', 'lngdppc', 'gdp_grow', 'offxrate_lcudif',
  'kaopen', 'F.worldfdi' )
apForm1 = formula(paste0(
  'F.lnfdi ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi',ivs)])
apMod1 = lm(apForm1, data=regData)
apRobustMod1 = cl(regData, apMod1, regData$ifscode)
print('A & P Original Model 1')
print(round(apRobustMod1, 3)[1:length(ivs),])

# A & P: (2)
ivs[2] = 'numreg2'
apForm2 = formula(paste0(
  'F.lnfdi ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi',ivs)])
apMod2 = lm(apForm2, data=regData)
apRobustMod2 = cl(regData, apMod2, regData$ifscode)
print('A & P Original Model 2')
print(round(apRobustMod2, 3)[1:length(ivs),])

# A & P: (3)
ivs[2] = 'numreg5'
apForm3 = formula(paste0(
  'F.lnfdi ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi',ivs)])
apMod3 = lm(apForm3, data=regData)
apRobustMod3 = cl(regData, apMod3, regData$ifscode)
print('A & P Original Model 3')
print(round(apRobustMod3, 3)[1:length(ivs),])

# Corrected: (1)
ivs = c(
  'bitcount', 'pending','cbdcrisis', 'domestic1_8', 'external_conflict', 'polity2',
  'proprights', 'lnpop_total', 'lngdppc', 'gdp_grow', 'offxrate_lcudif',
  'kaopen', 'F.worldfdi' )
corrForm1 = formula(paste0(
  'F.lnfdi_correct ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi_correct',ivs)])
corrMod1 = lm(corrForm1, data=regData)
corrRobustMod1 = cl(regData, corrMod1, regData$ifscode)
print('Corrected Model 1')
print(round(corrRobustMod1, 3)[1:length(ivs),])

# Corrected: (2)
ivs[2] = 'numreg2'
corrForm2 = formula(paste0(
  'F.lnfdi_correct ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi_correct',ivs)])
corrMod2 = lm(corrForm2, data=regData)
corrRobustMod2 = cl(regData, corrMod2, regData$ifscode)
print('Corrected Original Model 2')
print(round(corrRobustMod2, 3)[1:length(ivs),])

# Corrected: (3)
ivs[2] = 'numreg5'
corrForm3 = formula(paste0(
  'F.lnfdi_correct ~', paste(ivs, collapse = ' + '), '+ factor(ifscode) - 1' ))
regData = na.omit(fdi[,c('ifscode','year','F.lnfdi_correct',ivs)])
corrMod3 = lm(corrForm3, data=regData)
corrRobustMod3 = cl(regData, corrMod3, regData$ifscode)
print('Corrected Original Model 3')
print(round(corrRobustMod3, 3)[1:length(ivs),])
sink()