# AirToxicsMap
This repo contains code to create an interactive map of OR-specific tract-level air toxics cancer risk, based on NATA 2014 data.


The code to generate the Air Toxcis Cancer Risk map is contained in three files:
* risk_calc.R - this cscript reads in the NATA ambient concentrations and the state-specific chronic cancer risk TRVs (ure_trv.csv) to calculate cancer risk. 
It outputs 3 files in the "results" folder:
    + or_air_toxics_ambient_cancer_risk_all.csv (contains cancer risk for state, counties and tracts)
    + or_air_toxics_ambient_cancer_risk_tracts.csv (contains cancer risk for tracts only)
    + or_air_toxics_ambient_cancer_risk_counties.csv (contains cancer risk averages for the coutnies and state)
* at_sector_graphs.R - this script takes the tract-level cancer risk file and creates graphs showing the top five sectors/top five air toxins for the layers to be displayed in the interactive map. 
It also uses census data to create a demographic profile for each census tract.
The output from this file are a bunch (~3000) graphs, which are placed in the images folder.
* at_risk_map.Rmd - this creates the interactive leaflet map displaying cancer risk from air toxics.

To run the code as-is, please do the following to get the NATA ambient concentrations:
* Download the NATA 2014 data for Oregon from (this will be an Access database) https://www.epa.gov/national-air-toxics-assessment/2014-nata-assessment-results#state
* Export the "Ambient concentration (ug/m3)" layer as a csv form the Access database. Make sure that the option "export data with formatting and layout" is checked.
* rename the file "nata2014_ambient_concentrations.csv"
* place it in a folder named "data" in the working directory.

You will also need to get and install a census api key to access census data:
* Request a census api key from : https://api.census.gov/data/key_signup.html
* In an RStudio session, install the census api key : census_api_key("your key", install = TRUE)
* You only need to install the key once, it is subsequently available in future sessions without having to run this command.
