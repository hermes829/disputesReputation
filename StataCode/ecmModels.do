// Code to run ECM models

// load data
clear
use "/Users/janus829/Dropbox/Research/RemmerProjects/disputesReputation/Data/analysisData.dta"

// set panel params
tsset ccode year

// clear up
eststo clear

// All disputes across full sample
qui eststo: xtpcse pch_invProf L.pch_dispBC  L.dispBC lag1_pch_gdpLog ///
	lag1_gdpLog	lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>1986, pairwise corr (psar1)
estimates store m1, title(All Disputes)

// ICSID disputes across full sample
qui eststo: xtpcse pch_invProf L.pch_iDispBC  L.iDispBC lag1_pch_gdpLog ///
	lag1_gdpLog	lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>1986, pairwise corr (psar1)
estimates store m2, title(ICSID Disputes)

// Non-ICSID disputes across full sample
qui eststo: xtpcse pch_invProf L.pch_niDispBC  L.niDispBC lag1_pch_gdpLog ///
	lag1_gdpLog	lag1_pch_inflLog lag1_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>1986, pairwise corr (psar1)
estimates store m3, title(Non-ICSID Disptues)

// All disputes >2006
qui eststo: xtpcse pch_invProf L.pch_dispBC  L.dispBC lag1_pch_gdpLog ///
	lag1_gdpLog lag1_inflLog lag1_pch_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>2006, pairwise corr (psar1)
estimates store m4, title(All >2006)

// ICSID disputes >2006
qui eststo: xtpcse pch_invProf L.pch_iDispBC  L.iDispBC lag1_pch_gdpLog ///
	lag1_gdpLog lag1_inflLog lag1_pch_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>2006, pairwise corr (psar1)
estimates store m5, title(ICSID >2006)

// Non-ICSID disputes >2006
qui eststo: xtpcse pch_invProf L.pch_niDispBC  L.niDispBC lag1_pch_gdpLog ///
	lag1_gdpLog lag1_inflLog lag1_pch_inflLog lag1_pch_popLog lag1_popLog ///
	lag1_pch_intConf lag1_intConf lag1_pch_extConf lag1_extConf lag1_pch_rbitNoDuplC lag1_rbitNoDuplC ///
	lag1_pch_kaopen lag1_kaopen lag1_pch_polity lag1_polity ///
	lag1_invProf i.year i.ccode if year>2006, pairwise corr (psar1)
estimates store m6, title(Non-ICSID >2006)

// create table
estout m1 m2 m3 m4 m5 m6, drop(*.year *.ccode) cells(b(star fmt(4)) ///
   se(par fmt(3))) stats(r2, fmt(3) label(R-sqr))
   
