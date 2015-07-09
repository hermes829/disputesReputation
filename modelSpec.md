DisputesReputation
===

Building new data
---

* Big updates
    - Create new dispute variables
    - Run BIT getter script

* Inputs:
    - ICRG
    - World Bank
        + FDI, Population, GDP, GDP per capita, GDP growth, Exchange rate volatility
    - kaopen
    - polity


FDI model
---

* DV
    - FDI Net Inflows: World Bank
        + Also create yearly average for IV
    - Convert to real dollars
* IVs
    - BITs
        + Invesmtent policy hub
    - Disputes
        + New ICSID-UNCTAD data
        + from Remmer, check Dropbox
        + Create cumulative
        + Create two and five year moving sums
    - Domestic political shocks
        + Banks measure of domestic instability
    - ICRG
        + External threat
        + Property rights protection
    - World Bank
        + Population
        + GDP per capita, log
        + Economic growth
        + Exchange rate volatility
    - kaopen
        + kaopen, measure of capital openness

Reputation/ECM model
---

* DV
    - ICRG
        + Investment profile
* IVs
    - Disputes
        + see above
    - BITs
        + see above
    - World Bank
        + Population
        + Inflation
    - ICRG
        + Internal stablityi
    - kaopen
        + see above
    - polity
        + see above

* For the ECM we use the same variables but need to include the first differenced versions of each
