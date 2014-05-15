* Setting up time series object
tsset ccode year

* All ICSID disputes
xtreg Investment_Profile lagcum_kicsidcase lagpch_gdp lag_LNpopulation lag_lncinflation lag_Internal_Conflict lag_ratifiedbits lag_kaopen lag_polity if upperincome ~=1 & year>1986, fe i(ccode) cluster(ccode)

* ICSID treaty based disputes
xtreg Investment_Profile lagcum_icsidtreaty_case lagpch_gdp lag_LNpopulation lag_lncinflation lag_Internal_Conflict lag_ratifiedbits lag_kaopen lag_polity if upperincome ~=1 & year>1986, fe i(ccode) cluster(ccode)
	
* Unsettled disputes
xtreg Investment_Profile L.cumunsettled_icsid_treaty lagpch_gdp lag_LNpopulation lag_lncinflation lag_Internal_Conflict lag_ratifiedbits lag_kaopen lag_polity if upperincome ~=1 & year>1986, fe i(ccode) cluster(ccode)

* UNCTAD disputes
xtreg Investment_Profile lagcumcunctadcase lagpch_gdp lag_LNpopulation lag_lncinflation lag_Internal_Conflict lag_ratifiedbits lag_kaopen lag_polity if upperincome ~=1 & year>1986, fe i(ccode) cluster(ccode)

* UNCTAD + ICSID disputes
xtreg Investment_Profile L.cum_alltreaty lagpch_gdp lag_LNpopulation lag_lncinflation lag_Internal_Conflict lag_ratifiedbits lag_kaopen lag_polity if upperincome ~=1 & year>1986, fe i(ccode) cluster(ccode)
