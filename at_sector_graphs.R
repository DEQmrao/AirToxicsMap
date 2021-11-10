library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(tidycensus)
library(tigris)

#library(tmap)
#library(sf)
options(tigris_use_cache = TRUE)

################################################################################################
##
## This code creates ~2400 graphs and puts them in the images folder.
## The graphs are displayed in the popups on the leaflet map.
##
################################################################################################


## read in the cancer risk data for all tracts (tracts only)
at_all <- read.csv("results/or_air_toxics_ambient_cancer_risk_tracts.csv", stringsAsFactors = FALSE)

## read in the cancer risk data for the state and county-level risk
at_st <- read.csv("results/or_air_toxics_ambient_cancer_risk_counties.csv", stringsAsFactors = FALSE)

## extract state-leve risk averages for comparison
st_avg <- at_st %>% filter(County == "Entire State") %>% select("Pollutant.Name", state.ris = "Total.Cancer.Risk..per.million.")
rm(at_st)

################################################################################################
##
## dataframe manipulations to get the five air toxins posing greatest cancer risk in each tract
##
################################################################################################

# extract top five air toxins contibuting to cancer risk for each census tract
yat <- at_all %>% group_by(Tract) %>% 

         arrange(desc(Total.Cancer.Risk..per.million.)) %>% 
       slice(1:5) %>%
       select("Tract", "County", "Population", "Pollutant.Name", "Total.Cancer.Risk..per.million.")
colnames(yat) <- c("Tract", "County", "Population", "Air.Toxin", "Cancer.Risk")


# for each tract, create an "Others" category, summing up cancer risk from remaining air toxins       
yato <- at_all %>% group_by(Tract) %>% 
                   arrange(desc(Total.Cancer.Risk..per.million.)) %>% 
                   slice(6:n()) %>%
                   summarize(County = first(County),
                             Population = first(Population),
                             Air.Toxin = "Others",
                             Cancer.Risk = sum(Total.Cancer.Risk..per.million., na.rm = TRUE)) 
# combine the two data frames
yat <- bind_rows(yat, yato)
yat <- merge(yat, st_avg, by.x = "Air.Toxin", by.y = "Pollutant.Name", all.x = TRUE)


################################################################################################
##
## dataframe manipulation to get the top five sectors contibuting to cancer risk in each tract
##
################################################################################################

# extract top five sectors contibuting to cancer risk from air toxins for each census tract
y <- at_all %>% select(-c("State", "EPA.Region", "County", "FIPS", "Population", "Pollutant.Name")) %>%
  group_by(Tract) %>% 
  summarize_all(sum, na.rm = TRUE) %>%
  pivot_longer(cols = PT.StationaryPoint.Cancer.Risk..per.million.:BACKGROUND.Cancer.Risk..per.million.,
               names_to = "sector", values_to = "risk") %>% group_by(Tract) %>% arrange(desc(risk)) %>% slice(1:5)

# for each tract, create a "Others" category, summing up cancer risk from remaining sectors      
yo <- y %>% group_by(Tract) %>% 
  summarize(tot_ris = first(Total.Cancer.Risk..per.million.),
            sector = "Other",
            ris = sum(risk, na.rm = TRUE)) %>%
  mutate(ris = tot_ris - ris)
colnames(y) <- colnames(yo)

# combine the two data frames
y <- bind_rows(y, yo)
y$sector <- str_replace(y$sector, ".Cancer.Risk..per.million.", "")


################################################################################################
##
## dataframe manipulation to get the top five sectors contibuting to formaldehyde cancer risk in each tract
##
################################################################################################

# extract top five sectors contibuting to cancer risk from formaldehyde for each census tract
yf <- at_all %>% 
  filter(Pollutant.Name == "FORMALDEHYDE") %>%
  select(-c("State", "EPA.Region", "County", "FIPS", "Population", "Pollutant.Name")) %>%
  group_by(Tract) %>% 
  summarize_all(sum, na.rm = TRUE) %>%
  pivot_longer(cols = PT.StationaryPoint.Cancer.Risk..per.million.:BACKGROUND.Cancer.Risk..per.million.,
               names_to = "sector", values_to = "risk") %>% group_by(Tract) %>% arrange(desc(risk)) %>% slice(1:5)

# for each tract, create a "Others" category, summing up cancer risk from remaining sectors      
yfo <- yf %>% group_by(Tract) %>% 
  summarize(tot_ris = first(Total.Cancer.Risk..per.million.),
            sector = "Other",
            ris = sum(risk, na.rm = TRUE)) %>%
  mutate(ris = tot_ris - ris)
colnames(yf) <- colnames(yfo)

# combine the two data frames
yf <- bind_rows(yf, yfo)
yf$sector <- str_replace(yf$sector, ".Cancer.Risk..per.million.", "")


################################################################################################
##
## dataframe manipulation to get the top five sectors contibuting to diesel PM cancer risk in each tract
##
################################################################################################

# extract top five sectors contibuting to cancer risk from diesel PM for each census tract
yd <- at_all %>% 
  filter(Pollutant.Name == "DIESEL PM") %>%
  select(-c("State", "EPA.Region", "County", "FIPS", "Population", "Pollutant.Name")) %>%
  group_by(Tract) %>% 
  summarize_all(sum, na.rm = TRUE) %>%
  pivot_longer(cols = PT.StationaryPoint.Cancer.Risk..per.million.:BACKGROUND.Cancer.Risk..per.million.,
               names_to = "sector", values_to = "risk") %>% group_by(Tract) %>% arrange(desc(risk)) %>% slice(1:5)

# for each tract, create a "Others" category, summing up cancer risk from remaining sectors      
ydo <- yd %>% group_by(Tract) %>% 
  summarize(tot_ris = first(Total.Cancer.Risk..per.million.),
            sector = "Other",
            ris = sum(risk, na.rm = TRUE)) %>%
  mutate(ris = tot_ris - ris)
colnames(yd) <- colnames(ydo)

# combine the two data frames
yd <- bind_rows(yd, ydo)
yd$sector <- str_replace(yd$sector, ".Cancer.Risk..per.million.", "")


################################################################################################
##
## retrieve census data
## create the base quantile graphs for POC, poverty & < HS edu, with blue line for state average
## the tract specific red line will be added later, to create the tract-specific graph
##
################################################################################################

# ACS tables used by EJScreen
# from pg 84: https://www.epa.gov/sites/production/files/2021-04/documents/ejscreen_technical_document.pdf 
# B01001 ; Sex by age
# B03002 : Hispanic or Latino origin by race
# B15002 : Sex by education attainment for the population 25yrs and over
# C16002 : Household language by households in which no one 14 and over speaks english only 
#          or speaks a language other than english at home and speaks english "very well"
# C17002 : Ratio of inocme to poverty level in the past 12 months
# B25034 : Year structure built

### working with ACS table B03002 

# Assign Census variables vector to race_vars  
race_vars <- c(White = "B03002_003", Black = "B03002_004", Native = "B03002_005", 
               Asian = "B03002_006", HIPI = "B03002_007", Hispanic = "B03002_012")



# Request a summary variable from the ACS
or_race <- get_acs(geography = "tract", 
                   state = "OR",
                   variables = race_vars,
                   survey = "acs5",
                   year = 2019,
                   summary_var = c(tr_pop = "B03002_001"))

# for now, focus on %non-white as an indicator of diversity
# manipulate or_race dataframe to create the appropriate dataframe for diversity
or_dem <- or_race %>% filter(variable == "White")
colnames(or_dem)[4] <- "pop_white"
colnames(or_dem)[5] <- "moe_white"
colnames(or_dem)[6] <- "pop_tot"
colnames(or_dem)[7] <- "moe_tot"
or_dem <- or_dem %>% mutate(pct_div = (100 - (pop_white*100/pop_tot))) 

# retrieve poverty variables, and merge with or_dem dataframe
or_pov <- get_acs(geography = "tract", 
                  state = "OR",
                  variables = c(over_2x_pov = "C17002_008"),
                  survey = "acs5",
                  year = 2019,
                  summary_var = "C17002_001")
or_pov$pct_u2xpov <- 100 - or_pov$estimate*100/or_pov$summary_est
or_dem <- merge(or_dem, or_pov[, c("GEOID", "pct_u2xpov")])

# retrieve education variables
# create "less than high school" education category by simming up
# male and female grades completed numbers, and calculate pct.
or_edu <- get_acs(geography = "tract", 
                  state = "OR",
                  table = c("B15002"),
                  survey = "acs5",
                  year = 2019,
                  output = "wide")
or_edu$lshs <- or_edu$B15002_003E + or_edu$B15002_004E + or_edu$B15002_005E + or_edu$B15002_006E +
  or_edu$B15002_007E + or_edu$B15002_008E + or_edu$B15002_009E + or_edu$B15002_010E +
  or_edu$B15002_020E + or_edu$B15002_021E + or_edu$B15002_022E + or_edu$B15002_023E +
  or_edu$B15002_024E + or_edu$B15002_025E + or_edu$B15002_026E + or_edu$B15002_027E
or_edu$pct_lshs <- or_edu$lshs*100/or_edu$B15002_001E
or_dem <- merge(or_dem, or_edu[, c("GEOID", "pct_lshs")])


## add columns specifying the quantile for poc, pov & lhs the tract falls into
or_dem$div_quant <- with(or_dem, cut(or_dem$pct_div, 
                                     breaks=quantile(pct_div, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                     include.lowest=TRUE,
                                     labels = FALSE))
or_dem$pov_quant <- with(or_dem, cut(or_dem$pct_u2xpov, 
                                     breaks=quantile(pct_u2xpov, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                     include.lowest=TRUE,
                                     labels = FALSE))
or_dem$lshs_quant <- with(or_dem, cut(or_dem$pct_lshs, 
                                     breaks=quantile(pct_lshs, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                     include.lowest=TRUE,
                                     labels = FALSE))

## create tables of quantile cut-offs for the three demographic variables above
dd <- data.frame(xmin = c(0, round(quantile(or_dem$pct_div, probs=seq(0,1, by=0.2), na.rm=TRUE))), 
                 xmax = c(round(quantile(or_dem$pct_div, probs=seq(0,1, by=0.2), na.rm=TRUE)), 100),
                 cat = c(rep("div", 7)), qcol = c("snow2", "snow3", "snow3", "snow3", "snow3", "snow3", "snow2"))

dp <- data.frame(xmin = c(0, round(quantile(or_dem$pct_u2xpov, probs=seq(0,1, by=0.2), na.rm=TRUE))), 
           xmax = c(round(quantile(or_dem$pct_u2xpov, probs=seq(0,1, by=0.2), na.rm=TRUE)), 100),
           cat = c(rep("pov", 7)), qcol = c("snow2", "snow3", "snow3", "snow3", "snow3", "snow3", "snow2"))

dl <- data.frame(xmin = c(0, round(quantile(or_dem$pct_lshs, probs=seq(0,1, by=0.2), na.rm=TRUE))), 
                 xmax = c(round(quantile(or_dem$pct_lshs, probs=seq(0,1, by=0.2), na.rm=TRUE)), 100),
                 cat = c(rep("lshs", 7)), qcol = c("snow2", "snow3", "snow3", "snow3", "snow3", "snow3", "snow2"))

## create baseline quantile graphs
cap <- str_wrap("This figure shows in red (top) the percent of the tract population that identify as POC, 
                (middle) the pecent of the tract population below 2x the federal poverty level, 
                (bottom) the precent of the tract population with less than a high school education.
                Blue lines indicate the average percentages across all census tracts; white lines are quantile markers.", 80)
qgd <- ggplot() + geom_rect(data = dd, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = qcol), alpha = 0.7,  color = "white", size = 2) + 
           scale_fill_manual(name = "qcol", values = c("snow2", "snow3"), guide = "none") + 
           geom_rect(aes(xmin = 23-0.4, xmax = 23+0.4, ymin = 0, ymax = 1), fill = "cadetblue3") + theme_void() + ggtitle("%POC") +
           theme(plot.title = element_text(size = 18))
qgp <- ggplot() + geom_rect(data = dp, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = qcol), alpha = 0.7,  color = "white", size = 2) + 
          scale_fill_manual(name = "qcol", values = c("snow2", "snow3"), guide = "none") + 
          geom_rect(aes(xmin = 31-0.4, xmax = 31+0.4, ymin = 0, ymax = 1), fill = "cadetblue3") + theme_void() + ggtitle("% under 2x federal poverty") +
          theme(plot.title = element_text(size = 18))
qgl <- ggplot() + geom_rect(data = dl, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = qcol), alpha = 0.7,  color = "white", size = 2) + 
          scale_fill_manual(name = "qcol", values = c("snow2", "snow3"), guide = "none") + 
          scale_x_continuous(limits = c(0, 100), breaks = c(seq(0,100, 10))) +
          geom_rect(aes(xmin = 9-0.4, xmax = 9+0.4, ymin = 0, ymax = 1), fill = "cadetblue3") + theme_classic() + ggtitle("%less than HS education") +
          theme(plot.title = element_text(size = 18)) +
          labs(caption = cap) + 
          theme(axis.line.y = element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), plot.caption = element_text(hjust = 0, size = 18)) 




################################################################################################
##
## create and save graphs for each tract for top five air toxins and sectors
##
################################################################################################
rm(at_all)

## create color palette for air toxins
ats <- unique(yat$Air.Toxin)
ats_col <- brewer.pal(n = 8, name = "Dark2")
names(ats_col) <- levels(factor(ats))

## create color palette for sectors
secs <- c(unique(y$sector),"NR.CMV_C1C2", "NR.CMV_C1C2C3_underway","OR.LightDuty.OnNetwork.Diesel", "NR.AllOther","NR.CommercialEquipment" )
secs_col <- c(brewer.pal(n = 8, name = "Set2"), brewer.pal(n=11, name = "Set3"), "#666666", "#666666", "#FDBF6F", "#CAB2D6", "#A6CEE3")
names(secs_col) <- levels(factor(secs))



## loop to create and save the graphs for risk (by air toxis and sectors) and demographics
## use patchwork package to composite the graphs together
tracts <- unique(yat$Tract)
for (itr in tracts) {
   g1 <- yat %>% filter(Tract == itr ) %>% 
           ggplot(aes(x = reorder(Air.Toxin, -Cancer.Risk), y = Cancer.Risk, fill = Air.Toxin)) +
              scale_fill_manual(name = "Air.Toxin", values = ats_col) + 
              geom_hline(yintercept = 1, col = "red") +
              geom_bar(stat = "identity", alpha = 0.8) +
              geom_point(aes(x = Air.Toxin, y = state.ris), shape=23, fill="darkgray", color="darkblue", size=3) +
              xlab(NULL) + ylab("Cancer risk (# in a million)") +
              ylim(0,15) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
              theme(text = element_text(size = 20)) +
              ggtitle("Top 5 Air Toxins", subtitle = "contributing to cancer risk in tract") +
              guides(fill = FALSE) 

#     ggsave(filename = paste0("at_risk_map/images/at_", itr, ".png"), device = "png", height = 8, width = 8, units = "in" )   
   g2 <- y %>% filter(Tract == itr) %>% 
         ggplot(aes(x=reorder(sector, -ris), y=ris, fill=sector)) +
            scale_fill_manual(name = "sector", values = secs_col) +
            geom_bar(stat="identity") +
            xlab(NULL) + ylab("Cancer risk (# in a million)") +
            ylim(0, 21) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
            theme(text = element_text(size = 20)) +
            ggtitle("Top 5 sectors", subtitle = "contributing to cancer risk in tract") +
            guides(fill = FALSE) 

   div <- as.numeric(or_dem %>% filter(GEOID == itr) %>% select(pct_div))
   pov <- as.numeric(or_dem %>% filter(GEOID == itr) %>% select(pct_u2xpov))
   lshs <- as.numeric(or_dem %>% filter(GEOID == itr) %>% select(pct_lshs))
   gd <- qgd + geom_rect(aes(xmin = div-0.4, xmax = div+0.4, ymin = 0, ymax = 1), fill = "red2")
   gp <- qgp + geom_rect(aes(xmin = pov-0.4, xmax = pov+0.4, ymin = 0, ymax = 1), fill = "red2")
   gl <- qgl + geom_rect(aes(xmin = lshs-0.4, xmax = lshs+0.4, ymin = 0, ymax = 1), fill = "red2")
 
   gr <- (g1+g2)/((gd/gp)/gl) + plot_layout(heights = c(3,1))
   ggsave(gr, filename = paste0("images/totp_", itr, ".png"), device = "png", height = 15, width = 10, units = "in") 
   
}

## sector graphs for formaldehyde
tracts <- unique(yf$Tract)
for (itr in tracts) {
    yf %>% filter(Tract == itr) %>% 
      ggplot(aes(x=reorder(sector, -ris), y=ris, fill=sector)) +
      scale_fill_manual(name = "sector", values = secs_col) +
      geom_bar(stat="identity") +
      xlab(NULL) + ylab("Cancer risk (# in a million)") +
      ylim(0, 7) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
      theme(text = element_text(size = 20)) +
      ggtitle("Top 5 sectors", subtitle = "contributing to formaldehyde in tract") +
      guides(fill = FALSE) 
    ggsave(filename = paste0("images/form_", itr, ".png"), device = "png", height = 10, width = 8, units = "in") 
}

## sector graphs for diesel PM
tracts <- unique(yd$Tract)
for (itr in tracts) {
  yd %>% filter(Tract == itr) %>% 
    ggplot(aes(x=reorder(sector, -ris), y=ris, fill=sector)) +
    scale_fill_manual(name = "sector", values = secs_col) +
    geom_bar(stat="identity") +
    xlab(NULL) + ylab("Cancer risk (# in a million)") +
    ylim(0, 8) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
    theme(text = element_text(size = 20)) +
    ggtitle("Top 5 sectors", subtitle = "contributing to diesel PM in tract") +
    guides(fill = FALSE) 
  ggsave(filename = paste0("images/dpm_", itr, ".png"), device = "png", height = 10, width = 8, units = "in") 
}








