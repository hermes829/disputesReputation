//Change path to match where .dta is stored
use "/Users/s7m/Research/disputesReputation/replicationArchive/analysisData.dta"

//set unit and time vars
xtset ccode year 

//ICSID 1987 - 2006
xtpcse pch_invProf L.pch_iDispBC L.iDispBC lag1_pch_gdpLog lag1_gdpLog lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity lag1_invProf i.year i.ccode if year>1986 & year<2007, pairwise corr (psar1)

//ICSID 1987 - 2014
xtpcse pch_invProf L.pch_iDispBC L.iDispBC lag1_pch_gdpLog lag1_gdpLog lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity lag1_invProf i.year i.ccode if year>1986, pairwise corr (psar1)

//All treaty 1987 - 2006
xtpcse pch_invProf L.pch_dispBC L.dispBC lag1_pch_gdpLog lag1_gdpLog lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity lag1_invProf i.year i.ccode if year>1986 & year<2007, pairwise corr (psar1)

//All treaty 1987 - 2014
xtpcse pch_invProf L.pch_dispBC L.dispBC lag1_pch_gdpLog lag1_gdpLog lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity lag1_invProf i.year i.ccode if year>1986, pairwise corr (psar1)
