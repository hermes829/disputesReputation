### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/RemmerProjects/disputesReputation/RCode/setup.R')

### load data
setwd(pathData)
load('combinedData.rda')

# DV 
# Investment Profile from PRS Group: Investment.Profile

# Model covariates
# + GDP: gdp.y
# + GDP growth: chgdp
# + Polity / Polity^2 / Change in polity / Years in democracy: polity
# + Banks conflict index
# + Trade balance: tradebalance
# + BITs: signedbits
# + Settlement of concluded cases [[ would be fun if there was a positive relationship ]]: settle
# + Inflation: 
# + Dummy for upper income (World Bank Classification): 
# + ICSID disputes: conc_disputes
# + Spatial disputes [ distance ]: cshapes
# + FDI
# + Natural resource
#     - Possible interaction between this and ICSID disputes

# Variable manipulation

# polity correction
combData[combData$polity==-88,'polity'] <- -10
combData[combData$polity==-77,'polity'] <- -10
combData[combData$polity==-66,'polity'] <- -10

# conc_disputes spatial

# Running model
# Form of ECM
# ∆Yt = α + β0*∆Xt - β1(Yt-1 - β2Xt-1) + ε
formula = Investment.Profile ~ gdp.y + chgdp + polity + tradebalance + signedbits + 
			settle + conc_disputes
