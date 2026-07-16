# Dictionary of raw data files

Data for historical default rates by rating category is taken from Standard & Poor's. Specifically Table 18 in this [link](https://www.spglobal.com/ratings/en/research/articles/220504-default-transition-and-recovery-2021-annual-global-sovereign-default-and-rating-transition-study-12350530#ID18536)
- `10_year_default_rate.csv`

Data on emissions, included for the analysis in Table 4 is taken from [Our World in Data](https://ourworldindata.org/greenhouse-gas-emissions).
- `carbon-intensity-electricity.csv`
- `per-capita-ghg-emissions.csv`
- `total-ghg-emissions.csv`

Data on the option-adjusted spread, calculated for approximating the increase cost of debt resulting from a downgrade is taken from the [St Louis Federal Reserve](https://fred.stlouisfed.org/series/BAMLC0A4CBBB)
- `fredgraph.csv`

Macroeconomic data resulting from the NGFS scenarios is taken from the [NGFS portal](https://data.ece.iiasa.ac.at/ngfs/#/downloads)
- `NiGEM_data_v4.csv`

Data from Table 3 in "Storm Alert: Natural Disasters Can Damage Sovereign Creditworthiness" by Standard & Poor's
- `T3.csv`

Fundemental economic data
- `economic.csv`
Data on the economic fundementals of the sovereigns under study. The variables of interest in this dataset are the following;
* **CountryName**: List of country names
* **Year**: Year of observation
* **scale20**: 1-20 scale corresponding to S&P Sovereign rating scale
* **S_GDPpercapitaUS**: GDP per capita in US$ as defined by S&P SRI
* **S_RealGDPgrowth**: Real GDP growth as defined by S&P SRI
* **S_NetGGdebtGDP**: Net general government debt to GDP as defined by S&P SRI
* **S_GGbalanceGDP**: General government balance to GDP as defined by S&P SRI
* **S_NarrownetextdebtCARs**: Narrow net external debt to CARs as defined by S&P SRI
* **S_CurrentaccountbalanceGDP**: Current account balance to GDP as defined by S&P SRI

