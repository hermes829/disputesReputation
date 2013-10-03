June 11 2013
===============

### To Dos
	+ How does ICSID define concluded cases as a separate check [ wins minus losses ]
	+ Also make sure you're not counting settlements...these should be an actual positive 
	+ Karen will look into whether or not countries refuse to pay based upon settlements
	+ Magnitude of case
	+ Time of case
	+ Sector of economy
	+ We can also use UNCTAD cases as a separate check
	+ Also check out what is going on with net FDI inflows
	+ Add in sovereign ratings
	+ Control for whether you're a member of the ICSID
	+ IMF programs cancelled
		- Across different domains this so called reputational anchor is not working, cancelled your IMF program
		- Other international commitments...international transparency literature, guidelines on standardization of national accounts

### Model Covariates (long and short term, so lagged variable and change)
	+ GDP
	+ GDP growth
	+ Polity / Polity^2 / Change in polity / Years in democracy
	+ Banks conflict index
	+ Trade balance
	+ BITs
	+ Settlement of concluded cases [[ would be fun if there was a positive relationship ]]
	+ Inflation
	+ Dummy for upper income (World Bank Classification)
	+ ICSID disputes
	+ Spatial disputes [ distance ]
	+ FDI
	+ Natural resource
		- Possible interaction between this and ICSID disputes

### Backup Robustness Checks
	+ Banking, currency, or economic crisis

### Types of modeling strategies
	+ One stage error correction model with prop rights as DV
	+ Two stage error correction model
		- first stage look at what predicts disputes
		- second stage those that have disputes what is effect on proprights
	+ Split population approach

June 16 2013
===============

### To dos
	+ Additional data needs
		- Dummy for upper income
		- Sector of economy where dispute is concentrated
		- Member of ICSID
		- violating other international commitments
			+ expropriation
		- sovereign ratings
	+ Dataset modifications
		- I need to create a spatial version of the BITs variable
		- Instead of cumulative disputes measure need to put in an indicator of whether or not there was a dispute in a year
	+ Modeling strategy
		- Need to figure out how to incorporate ECM code into R
	+ Things to email Karen about
		- sector of economy for dispute
		- concluded cases includes settlements

June 27 2013
===============

### Add more variables
	+ Financial flows
	+ FDI as a proportion of GDP
	+ Veto Players
	+ Magnitude of debt overhand - debt services as a percentage of exports and as percentage of GDP
	+ Coding disputes by firm -- energy
	+ privatization data, countries with high numbers of privatized disputes --- enhance reputation

### Two stage model
	+ First stage: Country Fundamentals
		- Debt as percentage from GDP
		- Privatization data, cumulative number of firms privatized
		- Veto Players
		- Net flows on the financial side [ check coverage ]
		- GDP per capita
		- Population

Sept 18 2013
===============
### Add additional measures

* take components of heritage econ rating...and try their open market components...less political risk...what other measures can we insert here to explain ratings on property rights

	* will try several additional measures:

		* kaopen

		* polconiii

		* Doing Biz measures: elecTime, elecCost, invDisc, invDirecLiab, invSuits, invProtect, traDocExp, traTimeExp, traCostExp, traDocImp, traTimeImp, traCostImp, enfTime, enfCost, enfProc

		* heritage open markets: trade.freedom, investment.freedom, financial.freedom

		* fraser: X4Bii..Compliance.costs.of.importing.and.exporting, X4Dii..Capital.Controls, X2F.Legal.enforcement.of.contracts

* add in inflation 

	* Added to dataset referred to as inflation

* Try charts again with varying + and - lags for disputes

* Allee-Peinhardt Replication - redo correcting for logging of negative FDI inflow values

	* Completed redo and sent results to karen