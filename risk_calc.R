library(tidyverse)

## The NATA 2014 data can be downloaded as an Access database from:
## https://www.epa.gov/national-air-toxics-assessment/2014-nata-assessment-results#state
## NOte that it is very important to make sure that when exporting the data from Access to Excel,
## the preserve formatting option is selected. If not, the numbers will be rounded to two decimal places, 
## messing up all results.
## You will need to export all three files - the ambient concentrations, the exposure concentrations, and the
## the risk file.
## Note that the script - as writtern - reads in csv files

## read in the three data files:
##    The NATA exposure concentrations
##    The NATA risk
##    The file (epa_trv_risks_for_non_zero_nata2014_haps_working.csv) that has the NATA UREs, as well as the OR TRVs


ris <- read.csv("data/nata2014_health_risk.csv", stringsAsFactors = FALSE)
ure_trv <- read.csv("data/epa_trv_risks_for_non_zero_nata2014_haps_working.csv", stringsAsFactors = FALSE)
amb <- read.csv("data/nata2014_ambient_concentrations.csv", stringsAsFactors = FALSE)

len <- length(ure_trv$nata.name)
or_ris <- ris[FALSE, 1:46]
for ( i in 1:len) {
  if (!is.na(ure_trv$trv_can1_ugm.3[i])) {
    at_amb <- amb[, 1:46] %>% filter(Pollutant.Name == ure_trv$nata.name[i])
    at_ris <- ris[, 1:46] %>% filter(Pollutant.Name == ure_trv$nata.name[i]) 
    at_ris[, 8:46] <- 0
    #    at_check <- at_exp[, 8:46]/ure_trv$trv_can1_ugm.3[i]  
    at_ris[, 8:46] <- at_amb[, 8:46]/ure_trv$trv_can1_ugm.3[i]
    or_ris <- rbind(or_ris, at_ris)

    print(paste(as.character(i), ure_trv$nata.name[i]))
                
  }
  
}




## add 'pahpom' to the or_ris table
## using columns 1-46, as we are calculating cancer risk
## the column numbers will need to be checked in future NATA/AT/EJScreen updates

pah_ris <- ris[, 1:46] %>% filter(Pollutant.Name == "PAHPOM") 

## see documentation from Sue MacMillan for halving the EPA's risk estimate for PAHPOM, in the data folder
pah_ris[, 8:46] <- pah_ris[, 8:46]/2
or_ris <- rbind(or_ris, pah_ris)

or_ris_tracts <- or_ris %>% filter(FIPS*10^6 != Tract)
or_ris_counties <- or_ris %>% filter(FIPS*10^6 == Tract)


write.csv(or_ris, "results/or_air_toxics_ambient_cancer_risk_all.csv", row.names = FALSE)
write.csv(or_ris_tracts, "results/or_air_toxics_ambient_cancer_risk_tracts.csv", row.names = FALSE)
write.csv(or_ris_counties, "results/or_air_toxics_ambient_cancer_risk_counties.csv", row.names = FALSE)



rm(i, len, amb, at_amb, ris, or_ris, or_ris_tracts, or_ris_counties, at_ris,ure_trv, pah_ris)



