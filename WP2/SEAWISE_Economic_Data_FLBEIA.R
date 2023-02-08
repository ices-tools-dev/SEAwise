################################################################################
#  Seawise : preparing input data                                             #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  27/11/2023                                                       #
#   Modified:  Marga Andrés:08/02/2023                                         #
################################################################################

# 01_Data_eco_2022.R - extract economic data from AER 2022
# Copyright: AZTI, 2022
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
## Distributed under the terms of the GNU GPLv3


# LOAD LIBRARIES AND FUNCTIONS --------------------------------------------

library(dplyr)
library(openxlsx)

# General settings --------------------------------------------------------

rm(list = ls())
yrn     <- "2022"
datyear <- as.numeric(yrn)

# Loading data ------------------------------------------------------------
    # AER info 
    # STECF 22 06 - EU Fleet Economic and Transversal data_fleet segment.xlsx 
    # ('FS data' sheet)

indat.dir  <- file.path('XXXXX') # Insert your file.path

aer <- read.csv( file.path(indat.dir, "AER_2022_STECF2206_FSdata.csv"), 
                 stringsAsFactors = FALSE, sep = ";", dec = ',')

# Select required economic information -----------------------------------

    # Subsetting data
    # country       : Select the country of your case study
    # supra_region  : Target areas
    # geo_indicator : Select your geo_indicator
    # fishing_tech  : Chose all your fishing techology of your case study. For example:
        # DFN	0010
        # DFN	1024
        # HOK	ALL
        # HOK	0024
        # HOK	2440
        # DTS	0010
        # DTS	1024
        # DTS	1040
        # PTS	1024
        # PTS	2440
# Area
BoB_regions <- c("AREA27", "NAO") # Example, BoB regions

aer <- aer %>% 
  filter( country_code == "FRA", supra_reg %in% BoB_regions, geo_indicator == 'NGI') 

# Corresponding fishing_tech and vessel_length
aer <- aer %>% 
  filter( (fishing_tech == "DTS" & vessel_length == "VL0010") | 
          (fishing_tech == "DTS" & vessel_length == "VL1012") | 
          (fishing_tech == "DTS" & vessel_length == "VL1218") |
          (fishing_tech == "DTS" & vessel_length == "VL1824") | 
          (fishing_tech == "DTS" & vessel_length == "VL2440") | 
          (fishing_tech == "DFN" & vessel_length == "VL0010") |
          (fishing_tech == "DFN" & vessel_length == "VL1012") |
          (fishing_tech == "DFN" & vessel_length == "VL1218") |
          (fishing_tech == "DFN" & vessel_length == "VL1824") |
          (fishing_tech == "HOK" & vessel_length == "VL0010") |
          (fishing_tech == "HOK" & vessel_length == "VL1012") |
          (fishing_tech == "HOK" & vessel_length == "VL1218") |
          (fishing_tech == "HOK" & vessel_length == "VL1824") |
          (fishing_tech == "HOK" & vessel_length == "VL2440") |
          (fishing_tech == "PTS" & vessel_length == "VL0010") | 
          (fishing_tech == "PTS" & vessel_length == "VL1012") | 
          (fishing_tech == "PTS" & vessel_length == "VL1218") | 
          (fishing_tech == "PTS" & vessel_length == "VL1824") | 
          (fishing_tech == "PTS" & vessel_length == "VL2440"))   

# Variables of interest
vars.eco <- aer %>% select(variable_name, variable_code, unit) %>% unique()

vsel <- c( "Number of vessels", "Maximum days at sea", "Engaged crew", 
           "Fishing days", "Days at sea", "Number of fishing trips",  # Effort unit?
           "Energy costs", "Personnel costs", "Other variable costs", # Variable costs
           "Repair & maintenance costs", "Other non-variable costs",  # Fixed costs
           "Value of physical capital", "Consumption of fixed capital", 
           "Gross value of landings")

vars.sel <- vars.eco %>% filter(variable_name %in% vsel)

# The fleet is defined by fishing_tech and length.
aer$fishing_tech_1 <- paste(aer$fishing_tech, aer$vessel_length, sep = '')

# Reshaping data frame
data_eco <- aer %>% 
  filter(variable_code %in% vars.sel$variable_code) %>%
  #filter(!fs_name %in% fs_remove) %>% 
  select(year, fishing_tech_1, vessel_length, variable_code, value)  # vessel_length, gear, fs_name

dc <- data_eco %>% group_by(year, fishing_tech_1,variable_code) %>% summarise(n = n())
if ( any(dc$n > 1))
  stop("REVISE: More than one value for an specific year and fleet.")


# FLBEIA indicators
# * by fleet
#   - crew share (% of the gross value)        : Personnel costs / Gross value of landings
#   - fixed costs (by vessel)                  : (Repair & maintenance costs + Other non-variable costs) / Number of vessels
#   - capital value                            : Value of physical capital
#   - fixed salarie (per crew member)          : 0          
#   - maximum effort                           : Maximum days at sea
#   - employees (by vessel)                    : Engaged crew / Number of vessels
#   - depreciation (by vessel)                 : Consumption of fixed capital / Number of vessels
#   - vessels (of the fleet)                   : Number of vessels (values for simulations taken from ARVI data)
#   - new vessel, investment share, w1, w2 (for capital dynamics)
# * by fleet and metier
#   - fuel cost (per unit of effort)           : Energy costs / Fishing days
#   - other variable cost (per unit of effort) : Other variable costs / Fishing days

ids.all <- c('fuel cost', 'crew share', 'other variable cost', 'fixed costs', 
             'capital value', 'fixed salarie', 'maximum effort', 'employees', 
             'depreciation', 'vessels', 'new vessel', 'investment share', 'w1', 'w2')

ids.met   <- c('fuel cost', 'other variable cost')
ids.fleet <- ids.all[!ids.all %in% ids.met]

data_eco <- data_eco %>% 
  tidyr::pivot_wider(names_from = variable_code, values_from = value) %>% 
  mutate( 'fuel cost' = totenercost/totfishdays, 
          'crew share'= totcrewwage/totlandginc, 
          'other variable cost' = totvarcost/totfishdays,
          'fixed costs' = (totrepcost + totnovarcost)/totves, 
          'capital value' = totdeprep/totves, 
          'fixed salarie'= 0, # Asumimos que no hay parte variable??? 
          'maximum effort' = maxseadays, 
          'employees' = totjob/totves,
          'depreciation' = totdepcost/totves,
          'vessels' = totves, 
          'new vessel' = NA, 'investment share' = NA, w1 = NA, w2 = NA) # Data from other sources, not AER

# Columns of interest

data_eco <- data_eco %>% 
  select(-any_of(vars.sel$variable_code))

# Remove years without data
data_eco <- data_eco %>% filter(!is.na(`fuel cost`))

# Transform to required format
data_eco <- data_eco %>% 
  tidyr::pivot_longer(names_to = "indicator", cols = all_of(ids.all)) %>% 
  mutate(fleet = case_when(indicator %in% ids.fleet ~ value), 
           mt1 = case_when(indicator %in% ids.met ~ value)) %>% 
  select(fishing_tech_1, indicator, year, fleet, mt1)


# # Missing info for datyear - 1 (mean of last 3 years)

 yrs.mean <- unique(data_eco$year)
 
 if (!datyear %in% yrs.mean) {
   deco_last <- data_eco %>% group_by(fishing_tech_1, indicator) %>%
     summarise(fleet = mean(fleet), mt1 = mean(mt1)) %>% 
     ungroup() %>% mutate(year = datyear - 1)
   data_eco <- data_eco %>% bind_rows(deco_last)
 }
 
# Last year first
data_eco <- data_eco %>% 
  arrange(fishing_tech_1, -year, match(indicator, ids.all))

# Generating excell file --------------------------------------------------
## DFN
# DFNVL0010: 
data_eco_DFNVL0010 <- data_eco %>% filter(fishing_tech_1 == "DFNVL0010") %>% select(-fishing_tech_1)

# DFNVL1012: 
data_eco_DFNVL1012 <- data_eco %>% filter(fishing_tech_1 == "DFNVL1012") %>% select(-fishing_tech_1)

# DFNVL1218:
data_eco_DFNVL1218 <- data_eco %>% filter(fishing_tech_1 == "DFNVL1218") %>% select(-fishing_tech_1)

# DFNVL1824: 
data_eco_DFNVL1824 <- data_eco %>% filter(fishing_tech_1 == "DFNVL1824") %>% select(-fishing_tech_1)

## DTS
# DTSVL0010
data_eco_DTSVL0010 <- data_eco %>% filter(fishing_tech_1 == "DTSVL0010") %>% select(-fishing_tech_1)

# DTSVL1012
data_eco_DTSVL1012 <- data_eco %>% filter(fishing_tech_1 == "DTSVL1012") %>% select(-fishing_tech_1)

# DTSVL1218
data_eco_DTSVL1218 <- data_eco %>% filter(fishing_tech_1 == "DTSVL1218") %>% select(-fishing_tech_1)

# DTSVL1824
data_eco_DTSVL1824 <- data_eco %>% filter(fishing_tech_1 == "DTSVL1824") %>% select(-fishing_tech_1)

# DTSVL2440
data_eco_DTSVL2440 <- data_eco %>% filter(fishing_tech_1 == "DTSVL2440") %>% select(-fishing_tech_1)


## PTS
# PTSVL0010
data_eco_PTSVL0010 <- data_eco %>% filter(fishing_tech_1 == "PTSVL0010") %>% select(-fishing_tech_1)

# PTSVL1012
data_eco_PTSVL1012 <- data_eco %>% filter(fishing_tech_1 == "PTSVL1012") %>% select(-fishing_tech_1)

# PTSVL1218
data_eco_PTSVL1218 <- data_eco %>% filter(fishing_tech_1 == "PTSVL1218") %>% select(-fishing_tech_1)

# PTSVL1824
data_eco_PTSVL1824 <- data_eco %>% filter(fishing_tech_1 == "PTSVL1824") %>% select(-fishing_tech_1)

# PTSVL2440
data_eco_PTSVL2440 <- data_eco %>% filter(fishing_tech_1 == "PTSVL2440") %>% select(-fishing_tech_1)

## HOK

# HOKVL0010
data_eco_HOKVL0010 <- data_eco %>% filter(fishing_tech_1 == "HOKVL0010") %>% select(-fishing_tech_1)

# HOKVL1012
data_eco_HOKVL1012 <- data_eco %>% filter(fishing_tech_1 == "HOKVL1012") %>% select(-fishing_tech_1)

# HOKVL1218
data_eco_HOKVL1218 <- data_eco %>% filter(fishing_tech_1 == "HOKVL1218") %>% select(-fishing_tech_1)

# HOKVL1824
data_eco_HOKVL1824 <- data_eco %>% filter(fishing_tech_1 == "HOKVL1824") %>% select(-fishing_tech_1)

# HOKVL2440
data_eco_HOKVL2440 <- data_eco %>% filter(fishing_tech_1 == "HOKVL2440") %>% select(-fishing_tech_1)


# Join in list
data_eco <- list('DFN0010' = data_eco_DFNVL0010,
                 'DFN1012' = data_eco_DFNVL1012,
                 'DFN1218' = data_eco_DFNVL1218,
                 'DFN1824' = data_eco_DFNVL1824,
                 'DTS0010' = data_eco_DTSVL0010,
                 'DTS1012' = data_eco_DTSVL1012,
                 'DTS1218' = data_eco_DTSVL1218,
                 'DTS1824' = data_eco_DTSVL1824,
                 'DTS2440' = data_eco_PTSVL2440,
                 'PTS0010' = data_eco_PTSVL0010,
                 'PTS1012' = data_eco_PTSVL1012,
                 'PTS1218' = data_eco_PTSVL1218,
                 'PTS1824' = data_eco_PTSVL1824,
                 'PTS2440' = data_eco_PTSVL2440,
                 'HOK1012' =  data_eco_HOKVL1012,
                 'HOK1218' =  data_eco_HOKVL1218,
                 'HOK1824' =  data_eco_HOKVL1824,
                 'HOK2440' =  data_eco_HOKVL2440 )

# for (st in stk.age)
#   data_eco[[st]] <- data_eco_oth


# SAVE ECONOMIC DATA ------------------------------------------------------

hs <- createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
                  fontName = "Calibri", fgFill = "darkgrey")

cw <- lapply(data_eco, function(x) c(20, 10, 15, 15))

write.xlsx( data_eco, file = file.path(indat.dir , "FR_economic_data.xlsx"), 
            colNames = TRUE, headerStyle = hs, colWidths = cw)

