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


exp <- read.csv("data/nata2014_exposure_concentrations.csv", stringsAsFactors = FALSE)
ris <- read.csv("data/nata2014_health_risk.csv", stringsAsFactors = FALSE)
ure_trv <- read.csv("data/epa_trv_risks_for_non_zero_nata2014_haps_working.csv", stringsAsFactors = FALSE)

## forward check - dividing exp by inv of the cancer URE recovers the estimated cancer risk
## this checkwas just to verify that this is indeed how the NATA risk was calculated
len <- length(ure_trv$nata.name)
outf <- data.frame(matrix(ncol = 6, nrow = len), stringsAsFactors = FALSE)
colnames(outf) <- c("num", "hap", "inv_ure", "max_diff", "avg_diff", "sd_diff")
for ( i in 1:len) {
  outf$num[i] <- i
  outf$hap[i] <- ure_trv$nata.name[i]
  if (!is.na(ure_trv$trv_can1_ugm.3[i]) ) {
    at_exp <- exp %>% filter(Pollutant.Name == ure_trv$nata.name[i])
    at_ris <- ris %>% filter(Pollutant.Name == ure_trv$nata.name[i])
    #    at_check <- at_exp[, 8:46]/ure_trv$trv_can1_ugm.3[i]  
    at_check <- at_exp[, c(8:27, 29:46)]/ure_trv$epa_inv_ure[i] - at_ris[, c(8:27, 29:46)]
    outf$inv_ure[i] <- ure_trv$epa_inv_ure[i]
    outf$max_diff[i] <- max(abs(at_check), na.rm = TRUE)
    outf$avg_diff[i] <- mean(as.matrix(at_check), na.rm = TRUE)
    outf$sd_diff[i] <- sd(as.matrix(at_check), na.rm = TRUE)
    
  }
  
}

## review forward_check.csv to make sure max, mean & sd are close to 0
write.csv(outf, "forward_check.csv")




## back check that dividing exposure by risk recovers the inverse of the cancer URE
len <- length(ure_trv$nata.name)
out <- data.frame(matrix(ncol = 6, nrow = len), stringsAsFactors = FALSE)
colnames(out) <- c("num", "hap", "inv_ure", "risk_diff", "avg_risk", "sd_risk")
for ( i in 1:len) {
  out$num[i] <- i
  out$hap[i] <- ure_trv$nata.name[i]
  if (!is.na(ure_trv$trv_can1_ugm.3[i])) {
    at_exp <- exp %>% filter(Pollutant.Name == ure_trv$nata.name[i])
    at_ris <- ris %>% filter(Pollutant.Name == ure_trv$nata.name[i])
    at_check <- at_exp[, c(8:27, 29:46)]/at_ris[, c(8:27, 29:46)]
    out$inv_ure[i] <- ure_trv$epa_inv_ure[i]
    out$risk_diff[i] <- (max(abs(at_check - ure_trv$epa_inv_ure[i] ), na.rm = TRUE)*100)/ure_trv$epa_inv_ure[i]
    out$avg_risk[i] <- mean(as.matrix(at_check), na.rm = TRUE)
    out$sd_risk[i] <- sd(as.matrix(at_check), na.rm = TRUE)
     
    print(paste(as.character(i), ure_trv$nata.name[i], 
                " : max risk diff : ", as.character(out$risk_diff[i]), as.character(out$avg_risk[i]), as.character(out$sd_risk[i])))
  }
  
}

## review reverse_check.csv 
## the risk difference column gives the %difference in risk for the value that had the maximum difference of risk
## I would suggest reviewing all case where the % diff in max risk is more than a few pct point.
## for the haps that show a greater than few pct points diff, set i to the "num" value, and run the code in thefor loop.
## write out at_check, and see where the difference is coming from...and that it is okay
write.csv(out, "reverse_check.csv")

## remove the exp df, now that we have verified the logic
rm(exp, len, i, out, outf)

## now that we have verified the logic, we read in the NATA ambient concentrations, and divide by the OR TRVs
## to get the OR-specific risk
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
or_ris <- rbind(or_ris, pah_ris)

or_ris_tracts <- or_ris %>% filter(FIPS*10^6 != Tract)
or_ris_counties <- or_ris %>% filter(FIPS*10^6 == Tract)


write.csv(or_ris, "results/or_air_toxics_ambient_cancer_risk_all.csv", row.names = FALSE)
write.csv(or_ris_tracts, "results/or_air_toxics_ambient_cancer_risk_tracts.csv", row.names = FALSE)
write.csv(or_ris_counties, "results/or_air_toxics_ambient_cancer_risk_counties.csv", row.names = FALSE)


rm(amb, ris, or_ris, or_ris_tracts, or_ris_counties, at_ris, at_exp, ure_trv, at_check, pah_ris)



