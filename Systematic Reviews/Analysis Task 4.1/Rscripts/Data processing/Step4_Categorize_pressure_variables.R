#####################################################################################################################-
#####################################################################################################################-
#
#     Step 4. Categorize pressure variables and create new version of the data
#
#     By Esther Beukhof
#     June 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#
library(data.table)
library(splitstackshape)
library(openxlsx)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 3.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa.rds"))

# data                                  <- data_allScreened #in case you'd like to run with the entire dataset (incl. all columns)


#-----------------------------------------------#
# Categorize pressure variables ----
#-----------------------------------------------#

# Expected categories in a broad sense:
# - Catch
# - Bycatch
# - Fishing effort
# - Presence of fishing activity
# - Fleet capacity
# - Mortality
# - Closure/ban/MPA
# - Bycatch reduction & selectivity
# - Discarding
# - Litter
# - Electromagnetic input
# - Gear comparison
# - Light

# What about...?
# - Noise -> nothing in Pressure variable reported indicates this is studied


# To do: try to classify each paper under one of the categories as 'PV_xxx'

# Check whether there are any double spaces (as this may give issues when doing the splitting)
table(grepl("  ", data$Pressure_variable)) #only one case, does not give issues

# Split when multiple pressure variables are reported in one row into multiple rows
datPressvar                               <- cSplit(data, "Pressure_variable", " _ ", "long")
datPressvar$Pressure.variable_category    <- NA

## Catch ----
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(!(Pressure_level %in% "Bycatch") & Pressure_variable %in% c("Catch",
                                                                                              "catch",
                                                                                              "catches",
                                                                                              "total catch",
                                                                                              "Total catch",
                                                                                              "total catches",
                                                                                              "Total catches",
                                                                                              "CPUE",
                                                                                              "CpUE",
                                                                                              "catch per effort",
                                                                                              "catch per unit effort",
                                                                                              "Catch rates",
                                                                                              "Catches per fishing gear",
                                                                                              "Catches per year",
                                                                                              "Temporal catch composition (target vs. non target)",
                                                                                              "estimated average catches"),
                                                                     "Catch",Pressure.variable_category))

# Add papers reflecting the catching process and that studied target species
sort(unique(datPressvar[which(grepl("process", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% "Catch"),]$Pressure_variable)) #none

sort(unique(datPressvar[which(grepl("operation", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure.type %in% "Catch_and_bycatch" &
                                !datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("operation", datPressvar$Pressure_variable) & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       datPressvar$Pressure.type %in% "Catch_and_bycatch" &
                                                                       !datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Catch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Purification treatment"),
                                                                     "Catch", Pressure.variable_category))

sort(unique(datPressvar[which(grepl("capture", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% "Target"),]$Pressure_variable)) #only one SW4_1285
sort(unique(datPressvar[which(grepl("Capture", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% "Target"),]$Pressure_variable)) #none
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("capture", datPressvar$Pressure_variable) &
                                                                       is.na(Pressure.variable_category) &
                                                                       Pressure_level %in% "Target",
                                                                     "Catch",Pressure.variable_category))
## Add also two other rows of SW4_1285 (i.e. capture depth, hooking depth) and related papers SW4_1224 and SW4_1263
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% c("depth of the anatomical location of hooks",
                                                                                               "hook type") &
                                                                       is.na(Pressure.variable_category) &
                                                                       Pressure_level %in% "Target",
                                                                     "Catch",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("landing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #almost all
sort(unique(datPressvar[which(grepl("Landing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all

# Add papers regarding landings
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% c("annual landings",
                                                                                               "Fishing landings",
                                                                                               "fishing landings",
                                                                                               "fraction landings",
                                                                                               "Total landings",
                                                                                               "Landings") &
                                                                        is.na(Pressure.variable_category),
                                                                      "Catch",Pressure.variable_category))

# Add paper on how fishing on small pelagics affects food availability for dolphins in the Med (SW4_0081)
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% "SW4_0081" & is.na(Pressure.variable_category),
                                                                      "Catch",Pressure.variable_category))

# Add paper that assesses trait composition of fished assemblages
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Biological trait removal by fishing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0633", "Catch",Pressure.variable_category))

# Add paper that studies effect of biomass removal (besides fishing mortality and fishing effort)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("biomass", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1296", "Catch",Pressure.variable_category))

# Add paper that studies effect of crowding in the net on mortality
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("crowding", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0115", "Catch",Pressure.variable_category))

# Add paper that studies effect of catching process on behaviour and biochemical aspects of target species
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("mechanical shaking", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_2031", "Catch",Pressure.variable_category))

# Add paper that studies physiological effect of discarded or escaped Nephrops
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% "SW4_1504", "Catch", Pressure.variable_category))

# Add paper that looked at landings data to assess status of the stock and exploitation pattern of the fishery
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("landings data including size distribution", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1768","Catch",Pressure.variable_category))

# Add paper that looked ecological impact of dredging by assessing the catch and sediment
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Hydraulic dredging" & is.na(datPressvar$Pressure.variable_category) &
                                                                      (Pressure_level %in% "Target" | is.na(Pressure_level)) & SW.ID %in% "SW4_2033",
                                                                     "Catch",Pressure.variable_category))

# Add paper that looked at landings and biological traits of redfish species 
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Impact of artisanal fishery on the large red scorpionfish" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0751", "Catch",Pressure.variable_category))

# Add paper that used commercial landings from target fisheries in stock assessment
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "commercial landings" & is.na(datPressvar$Pressure.variable_category) &
                                                                     Pressure_level %in% "Target" & SW.ID %in% "SW4_0908",
                                                                     "Catch",Pressure.variable_category))

# Add paper on using keep net during recreational fisheries
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "use of keep net" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0330", "Catch",Pressure.variable_category))

# Add paper on using keep net during recreational fisheries
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "use of keep net" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0330", "Catch",Pressure.variable_category))

# Add paper on that studies prey biomass removal on dolphins
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "total prey biomass removed" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1315", "Catch",Pressure.variable_category))

# Add paper on that studies prey biomass removal on dolphins
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "landings" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1952", "Catch",Pressure.variable_category))

PV_catch                                  <- subset(datPressvar, Pressure.variable_category %in% "Catch")



## Bycatch ----
sort(unique(datPressvar[which(grepl("bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("Bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("by-catch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("By-catch", datPressvar$Pressure_variable)),]$Pressure_variable))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("bycatch",
                                                                                              "Bycatch",
                                                                                              "bycatch intensity",
                                                                                              "Bycatch risk",
                                                                                              "bycatch percentage",
                                                                                              "Bycatch (captures per year)",
                                                                                              "Bycatch of Seals",
                                                                                              "Bycatch of Seas",
                                                                                              "bycatch vs. not-caught",
                                                                                              "bycatch sex ratio"),
                                                                     "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_level %in% "Bycatch" & Pressure_variable %in% c("Catch",
                                                                                                                              "catch",
                                                                                                                              "catches",
                                                                                                                              "total catch",
                                                                                                                              "Total catch",
                                                                                                                              "total catches",
                                                                                                                              "Total catches",
                                                                                                                              "CPUE",
                                                                                                                              "CpUE",
                                                                                                                              "catch per effort",
                                                                                                                              "catch per unit effort",
                                                                                                                              "Catch rates",
                                                                                                                              "Catches per fishing gear",
                                                                                                                              "Catches per year",
                                                                                                                              "Temporal catch composition (target vs. non target)"),
                                                                     "Bycatch",Pressure.variable_category))

# Add papers reflecting the catching process and that studied bycatch or non-target species
sort(unique(datPressvar[which(grepl("process", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("process", datPressvar$Pressure_variable) &
                                                                       datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Bycatch",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("operation", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("operation", datPressvar$Pressure_variable) &
                                                                       datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Bycatch",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("capture", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("capture", datPressvar$Pressure_variable) &
                                                                       datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Bycatch",Pressure.variable_category))

# Add papers on entanglement where Pressure type is NOT Input of litter but Catch_and_bycatch
sort(unique(datPressvar[which(grepl("entangl", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Entangl", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure_level %in% c("Bycatch","Non-target")),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("entangle", datPressvar$Pressure_variable) & datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Bycatch",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Entangle", datPressvar$Pressure_variable) & datPressvar$Pressure_level %in% c("Bycatch","Non-target"),
                                                                     "Bycatch",Pressure.variable_category))

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Fishing activity" & SW.ID %in% "SW4_0350",
                                                                     "Bycatch", Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("exposure to air", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_2013",
                                                                     "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Discarding", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       !(Pressure_level %in% "Target") & SW.ID %in% "SW4_1469",
                                                                     "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "discarded/marketed catch ratio" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1332", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Fishing depth" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1496", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing depth" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0179", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing index" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0979", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing interaction" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1170", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Foraging during interaction with trawlers" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0131", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishing with beam trawl",
                                                                                              "fishing with a 12m trawl") & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1714", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Hydraulic dredging" & is.na(datPressvar$Pressure.variable_category) &
                                                                      !(Pressure_level %in% "Target") & SW.ID %in% "SW4_2033","Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "incidental catch as cause of death of marine turtles" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1788", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("teeth", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0296", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Physical contact with gear" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0476", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1354", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "proximity to trammel nets" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0311", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "commercial landings" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       !(Pressure_level %in% "Target") &
                                                                       SW.ID %in% "SW4_0908",
                                                                     "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("number of incidental interaction",
                                                                                              "number of interaction") & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0071", "Bycatch",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("number of entanglement",
                                                                                              "number of interaction") &
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0065", "Bycatch",Pressure.variable_category))

PV_bycatch                                  <- subset(datPressvar, Pressure.variable_category %in% "Bycatch")
## NOTE: studies on bycatch exclusion devices will  go under another category



## Fishing effort ----
sort(unique(datPressvar[which(grepl("fishing effort", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all, except twp
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("fishing effort", datPressvar$Pressure_variable) &
                                                                       !(Pressure_variable %in% c("Absence of fishing effort","reduction in fishing effort")),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Fishing effort", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Fishing effort", datPressvar$Pressure_variable),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("fishing intensity", datPressvar$Pressure_variable)),]$Pressure_variable)) #add
sort(unique(datPressvar[which(grepl("Fishing intensity", datPressvar$Pressure_variable)),]$Pressure_variable)) #add

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Fishing intensity","fishing intensity"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("fishing pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all but MPA & Exploitation rate
sort(unique(datPressvar[which(grepl("Fishing pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all but trawled. vs. non trawled

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishing pressure",
                                                                                              "fishing pressure (hours per habitat surface area)",
                                                                                              "fishing pressure index",
                                                                                              "Fishing pressure",
                                                                                              "Fishing pressure as desired catch"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("trawling pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add some
sort(unique(datPressvar[which(grepl("Trawling pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all (only one)

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawling pressure" & SW.ID %in% c("SW4_0161","SW4_0399"),
                                                                     "Fishing effort",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("trawling pressure (swept area)",
                                                                                              "Trawling pressure (average number of times seabed trawled per year)"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("hour", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all but one
sort(unique(datPressvar[which(grepl("Hour", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse((grepl("hour", datPressvar$Pressure_variable) | grepl("Hour", datPressvar$Pressure_variable)) & 
                                                                       is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("day", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all except decrease
sort(unique(datPressvar[which(grepl("Day", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishing days",
                                                                                              "Fishing days",
                                                                                              "fishing days at sea",
                                                                                              "Ghost net fishing number of days",
                                                                                              "mean number of fishing days per year",
                                                                                              "Days at sea"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("year", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all except catch and bycatch per year
sort(unique(datPressvar[which(grepl("Year", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all except catch and bycatch per year
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("cumulative horsepower per year",
                                                                                              "effort (trips/year)",
                                                                                              "fishing frequency as the number of time 1 squared km is swept per year",
                                                                                              "fishing trips per year",
                                                                                              "Number of times trawled each year",
                                                                                              "Number of years since first fishing",
                                                                                              "Trawling pressure (average number of times seabed trawled per year)"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("effort", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all except CPUE and MPA
sort(unique(datPressvar[which(grepl("Effort", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all 

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Changes in trawling effort and mesh size changes ('French' trawl)",
                                                                                              "Changes in trawling effort and mesh size changes (traditional trawls)",
                                                                                              "Crab pot fishery effort (number of pots)",
                                                                                              "effort",                                                                
                                                                                              "Effort",
                                                                                              "Fishries effort",
                                                                                              "Trawl effort",
                                                                                              "trawling effort",
                                                                                              "Trawling effort",
                                                                                              "Trawling effort/ intensity"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("duration", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all but food patches and protection
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("haul durations",
                                                                                              "mean trawl duration",
                                                                                              "tow duration",
                                                                                              "Tow duration"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("number", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #several directly related to effort only
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("number of fishing vessels",
                                                                                              "number of ghost net",
                                                                                              "number of trawls",
                                                                                              "number of vessel",
                                                                                              "number of vessels",
                                                                                              "number of diggers"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Number", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #several directly related to effort only
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Number of fleet",
                                                                                              "Number of vessels"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("speed", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Vessel fishing speed",
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("time", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all directly related to fishing activities only
sort(unique(datPressvar[which(grepl("Time", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (onyl one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("effective trawling time of the nets on the bottom",
                                                                                              "fishing activity (i.e., soak time)",
                                                                                              "Sorting time",
                                                                                              "time spent setting and hauling nets pre sunset and post sunrise",
                                                                                              "time trend and relation to depth",
                                                                                              "Trawling time",
                                                                                              "Time trawled"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("power", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("power", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("frequenc", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but marine litter
sort(unique(datPressvar[which(grepl("Frequency", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but marine litter
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("field frequency",
                                                                                              "fishing frequency",
                                                                                              "High fishing frequency",
                                                                                              "Immediate trawling (low fishing frequency)",
                                                                                              "swept frequency",
                                                                                              "trawl frequency",
                                                                                              "trawl frequency per rectangle",
                                                                                              "trawling frequency",
                                                                                              "trawling frequencies",
                                                                                              "Frequency of fishing gear",
                                                                                              "Frequency of trawling"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("intensities", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("intensities", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("intensity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("intensity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Intensity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Intensity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("SAR", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("SAR", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("swept area ratio", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("swept area ratio", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Swept Area Ratio", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Swept Area Ratio", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Swept area", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Swept area", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("abrasion", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("abrasion", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_1684","SW4_1723"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("density", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #almost all
sort(unique(datPressvar[which(grepl("Density", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Density of bottom trawl vessels",
                                                                                              "Density of trawl marks",
                                                                                              "Trawl mark density",
                                                                                              "Trawling tracks density"),
                                                                     "Fishing effort",Pressure.variable_category))

# Add papers manually after having checked papers in detail
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("fishing activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0543","SW4_0596","SW4_0120","SW4_0024",
                                                                                    "SW4_0252","SW4_0307"), "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Fishing activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0024","SW4_0120","SW4_0307","SW4_0465", 
                                                                                    "SW4_0531"), "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Trawling activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0253", "SW4_0361"), "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Fishing capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0252", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Bottom trawling", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1960", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Demersal fishing disturbance", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0919", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("direct and indirect impact of fishing treating them as predators", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1069", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishing capacity",
                                                                                              "number of trawlers") & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0179", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Fishing Pressure" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1501", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Trawling activity",
                                                                                              "Re-suspension of sediment due to trawling activity") &
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0829", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Percentage of damage of shipwrecks (supposed due to trawling)" &
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0688", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Period of fishing exposure" & is.na(datPressvar$Pressure.variable_category) &
                                                    SW.ID %in% "SW4_1634", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Temporal aggregation of fishing events" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0705", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawlers abundance" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1821", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawling-induced erosion rate" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0279", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawling disturbance (SPUE)" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1625", "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawling disturbance" & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0865", "Fishing effort",Pressure.variable_category))

PV_fishingEffort                          <- subset(datPressvar, Pressure.variable_category %in% "Fishing effort")



## Presence of fishing activity ----
sort(unique(datPressvar[which(grepl("presence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Presence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #almost all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("presence", datPressvar$Pressure_variable),
                                                                     "Presence of fishing activity",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Presence", datPressvar$Pressure_variable) & !SW.ID %in% "SW4_1034",
                                                                     "Presence of fishing activity",Pressure.variable_category))


sort(unique(datPressvar[which(grepl("activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add some
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("activity", datPressvar$Pressure_variable) & 
                                                                       SW.ID %in% c("SW4_0717","SW4_1833","SW4_1724"),
                                                                     "Presence of fishing activity",Pressure.variable_category))
# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(SW.ID %in% c("SW4_1485","SW4_1727","SW4_0359","SW4_0383") & is.na(Pressure.variable_category),
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Damage from fishing", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% "SW4_1034",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Trawled area", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% "SW4_1374",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("direct impact of mechanised clam dedging", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% "SW4_0077",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("disturbance event compared to control", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% "SW4_1803",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Evidence of Rapido trawl disturbance", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% "SW4_1735",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("experimental clam passes",
                                                                                              "experimental trawl passes"),
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing boats" & SW.ID %in% "SW4_0693",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing disturbance" & SW.ID %in% "SW4_1270",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Fishing Pressure" & SW.ID %in% "SW4_1151",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "occurrence of trawl marks" & SW.ID %in% "SW4_0484",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawled vs untrawled area" & SW.ID %in% "SW4_0885",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "occurrence of trawling" & SW.ID %in% "SW4_0778",
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Physical disturbance of sediment due to trawling" & 
                                                                       SW.ID %in% "SW4_1662", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "short and medium term effects of beam trawling" & 
                                                                       SW.ID %in% "SW4_1622", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawl scour" & 
                                                                       SW.ID %in% "SW4_0064", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawl tracks" & 
                                                                       SW.ID %in% "SW4_0622", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawling" & SW.ID %in% c("SW4_1826","SW4_1586","SW4_1585"),
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawling pressure" & SW.ID %in% c("SW4_0125","SW4_0156"),
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "before and after illegal trawling activity" & 
                                                                       SW.ID %in% "SW4_1209", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "before and after trawlling / disturbance" & 
                                                                       SW.ID %in% "SW4_1812", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Not directly studied but performed in a known heavily fished ecosystem" & 
                                                                       SW.ID %in% "SW4_1621", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Use of FADs" & SW.ID %in% "SW4_1758", 
                                                                     "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Number of red coral fragments spead over the sea bottom" & 
                                                                       SW.ID %in% "SW4_0518", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawling" & 
                                                                       SW.ID %in% "SW4_1487", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Before and after trawling" & 
                                                                       SW.ID %in% "SW4_1489", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawling" & 
                                                                       SW.ID %in% "SW4_1637", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawled vs. untrawled" & 
                                                                       SW.ID %in% "SW4_0251", "Presence of fishing activity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawling" & 
                                                                       SW.ID %in% "SW4_1722", "Presence of fishing activity",Pressure.variable_category))

PV_activity                          <- subset(datPressvar, Pressure.variable_category %in% "Presence of fishing activity")



## Fleet capacity ----
sort(unique(datPressvar[which(grepl("capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #some
sort(unique(datPressvar[which(grepl("Capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #none
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("capacity", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% c("SW4_0516","SW4_1807"),
                                                                     "Fleet capacity",Pressure.variable_category))
# Include some manually
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% c("SW4_1867","SW4_0870","SW4_0738"),
                                                                     "Fleet capacity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Engine power" & SW.ID %in% "SW4_0361",
                                                                      "Fleet capacity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% c("gross tonnage",
                                                                                               "trawler modernization") & SW.ID %in% "SW4_0440",
                                                                      "Fleet capacity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Trawl tracks" & 
                                                                       SW.ID %in% "SW4_0622", "Presence of fishing activity",Pressure.variable_category))

PV_capacity                          <- subset(datPressvar, Pressure.variable_category %in% "Fleet capacity")



## Mortality ----
sort(unique(datPressvar[which(grepl("mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but one
sort(unique(datPressvar[which(grepl("Mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishing mortality",
                                                                                              "Fishing mortality",
                                                                                              "Fishing mortality and exploitation rate",
                                                                                              "Gear-specific fishing mortality",
                                                                                              "mortality",
                                                                                              "Total fishing mortality",
                                                                                              "Mortality")
                                                                     & is.na(datPressvar$Pressure.variable_category),
                                                                     "Mortality",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("exploitation", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("explotation", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only one
sort(unique(datPressvar[which(grepl("Exploitation", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only one
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("exploitation", datPressvar$Pressure_variable) 
                                                                     & is.na(datPressvar$Pressure.variable_category),
                                                                     "Mortality",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("explotation", datPressvar$Pressure_variable) 
                                                                     & is.na(datPressvar$Pressure.variable_category),
                                                                     "Mortality",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Exploitation", datPressvar$Pressure_variable) 
                                                                     & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1536",
                                                                     "Mortality",Pressure.variable_category))

# Add paper where primary production required to sustain fishery was calculated, which represents the exploitation level
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Primary production required to sustain fishery (PPR)" & 
                                                                        SW.ID %in% "SW4_0670", "Mortality",Pressure.variable_category))

PV_mortality                          <- subset(datPressvar, Pressure.variable_category %in% "Mortality")
#CHECK: SW4_1392???


## Closure/ban/MPA ----
sort(unique(datPressvar[which(grepl("closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("close", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Close", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("close", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Close", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("ban", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only those referring to ban
sort(unique(datPressvar[which(grepl("Ban", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #none
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Fishing ban",
                                                                                              "pre and post fishing ban",
                                                                                              "Regulatory ban on capelin",
                                                                                              "trawl ban",
                                                                                              "Trawl ban",
                                                                                              "trawling ban"),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("MPA", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("MPA", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("protect", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Protect", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("protect", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Protect", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))


sort(unique(datPressvar[which(grepl("absence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
sort(unique(datPressvar[which(grepl("Absence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add only one
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Absence of fishing effort",
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("area", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all 
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("fishery/no fishery area",
                                                                                              "Fishing pressure: untrawled vs. trawled areas",
                                                                                              "Untrawled area"),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("reduc", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but one
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Fishing mortality reduction",
                                                                                              "reduction in fishing effort",
                                                                                              "reduction in fishing mortality"),
                                                                     "Closure",Pressure.variable_category))
# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Decrease on number of fishing days" & is.na(Pressure.variable_category) & 
                                                                        SW.ID %in% "SW4_0304", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "before and after experimental dredging" & 
                                                                        is.na(Pressure.variable_category) & SW.ID %in% "SW4_1663",
                                                                      "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Fishing restriction" & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_0362", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "spearfishing prohibition" & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_1072", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Trawling vs. Trawler Moratorium" & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_1829", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% c("Catch quota",
                                                                                               "Habitat credit system") & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_0445", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "No trawling to estimate recovery" & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_1707", "Closure",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% "Effort reduction" & is.na(Pressure.variable_category) &
                                                                        SW.ID %in% "SW4_1455", "Closure",Pressure.variable_category))

PV_closure                         <- subset(datPressvar, Pressure.variable_category %in% "Closure")



## Bycatch reduction & selectivity ----
sort(unique(datPressvar[which(grepl("exclu", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("exclu", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("selec", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("selec", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Selec", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Selec", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("pinger", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only one
sort(unique(datPressvar[which(grepl("Pinger", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("pinger", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Pinger", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("device", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only one (left)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("device", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% c("SW4_1343") & is.na(Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("bar spacing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0679",
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fishing with guarding nets" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1987",
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "use of visual deterrents" & 
                                                                       is.na(datPressvar$Pressure.variable_category) & SW.ID %in% "SW4_0333",
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Modified otter trawl vs. normal otter trawl" & 
                                                                       is.na(datPressvar$Pressure.variable_category) & SW.ID %in% "SW4_1320",
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))
##This paper should also go under Gear comparison, so will be added manually to that category below

PV_bycatchRedSel                         <- subset(datPressvar, Pressure.variable_category %in% "Bycatch reduction & selectivity")



## Discarding ----
sort(unique(datPressvar[which(grepl("discard", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure.type %in% "Discarding"),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("discard", datPressvar$Pressure_variable) & 
                                                                       is.na(datPressvar$Pressure.variable_category) &
                                                                       Pressure.type %in% "Discarding",
                                                                     "Discarding",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Discard", datPressvar$Pressure_variable) & 
                                is.na(datPressvar$Pressure.variable_category) &
                                datPressvar$Pressure.type %in% "Discarding"),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Discard", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("food", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("food", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("before and after", datPressvar$Pressure_variable) & 
                                                                       SW.ID %in% "SW4_1702" & is.na(Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

PV_discard                         <- subset(datPressvar, Pressure.variable_category %in% "Discarding")



## Litter ----
sort(unique(datPressvar[which(grepl("litter", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("litter", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Litter", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Litter", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only one
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #no results
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("lost", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("lost", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("abandon", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("abandon", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "frequency of ML occurence inside octopus traps" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0007",
                                                                     "Litter",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "observation of lost long-lines" & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0484",
                                                                     "Litter",Pressure.variable_category))

PV_litter                        <- subset(datPressvar, Pressure.variable_category %in% "Litter")



## Electromagnetic input ----
sort(unique(datPressvar$Pressure_variable[datPressvar$Pressure.type %in% "Electromagnetic input"])) #include all but one as in doubt
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("duty cycle",                    
                                                                                              "Exposure to electrical pulse",
                                                                                              "field frequency",
                                                                                              "field strength",
                                                                                              "Pulse direct current exposure",
                                                                                              "Pulsed bipolar currents"),
                                                                     "Electromagnetic input",Pressure.variable_category))

PV_electro                        <- subset(datPressvar, Pressure.variable_category %in% "Electromagnetic input")



## Gear comparison ----
sort(unique(datPressvar[which(grepl("gear", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #empty
sort(unique(datPressvar[which(grepl("Gear", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Gear type"),
                                                                     "Gear comparison",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("vs", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("12m beam trawl vs. baited trap",                                               
                                                                                              "beam trawl vs. baited trap",                                                   
                                                                                              "high pressure water jet and sorting vs. low pressure water jet and no sorting",
                                                                                              "Hydrodynamic drag of pulse trawl vs. beam trawl",                              
                                                                                              "pulse trawl vs beam trawl",                                                    
                                                                                              "pulse trawl vs. beam trawl",                                                   
                                                                                              "shrimp bait vs. worm bait",
                                                                                              "pot vs. traditional trammel net",
                                                                                              "Otter trawl vs. beam trawl"),
                                                                     "Gear comparison",Pressure.variable_category))
# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "change from trawling to creeling" & 
                                                                       is.na(datPressvar$Pressure.variable_category) & SW.ID %in% "SW4_0562",
                                                                     "Gear comparison",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "trawl dimensions" & 
                                                                       is.na(datPressvar$Pressure.variable_category) & SW.ID %in% "SW4_1714",
                                                                     "Gear comparison",Pressure.variable_category))

# SW4_1320 already under Bycatch reduction & selectivity, but add also to Gear comparison
SW4_1320                              <- subset(datPressvar, SW.ID %in% "SW4_1320")
SW4_1320$Pressure.variable_category   <- "Gear comparison"
datPressvar                           <- rbind(datPressvar, SW4_1320)
rm(SW4_1320)

PV_gear                        <- subset(datPressvar, Pressure.variable_category %in% "Gear comparison")



## Light ----
sort(unique(datPressvar[which(grepl("light", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all  (only one)
sort(unique(datPressvar[which(grepl("Light", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #empty
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("light"),
                                                                     "Light",Pressure.variable_category))

PV_light                        <- subset(datPressvar, Pressure.variable_category %in% "Light")



######### Check papers still left to categorize
table(datPressvar$Pressure.variable_category, useNA = "always")

papers                                <- datPressvar[is.na(datPressvar$Pressure.variable_category),]
sort(unique(papers$Pressure_variable))




#-----------------------------------------------#
# Save dataset ----
#-----------------------------------------------#

# Convert dataPressvar back into multiple pressure variables per row. Consideration is needed when a row has been split
# into multiple ones. If all split rows got assigned the same Pressure variable category, then all can be merged into
# one row again. If split rows got assigned different Pressure variable categories, then they have to remain split.

# Add pasted info for rows with multiple pressure variable categories
data_collapsed <- datPressvar[0,]

for(iRow in unique(datPressvar$ROWID)){
  subdat                            <- subset(datPressvar, ROWID %in% iRow)
  
  # If there's only one pressure variable, add row
  if(nrow(subdat) == 1){
    
    data_collapsed <- rbind(data_collapsed, subdat)
    
  }
  # If there's more than one pressure variable but all have assigned the same category, collapse and add as one row
  if(nrow(subdat) > 1 & 
     length(unique(subdat$Pressure.variable_category)) == 1){
    
    subdat$Pressure_variable <- paste(subdat$Pressure_variable, collapse = " _ ")
    data_collapsed <- rbind(data_collapsed, subdat[1,])
    
  }
  # If there's more than one pressure variable but not all have assigned the same category and they are all unique, add all rows
  if(nrow(subdat) > 1 &
     length(unique(subdat$Pressure.variable_category)) > 1 &
     length(unique(subdat$Pressure.variable_category)) == nrow(subdat)){
    
    data_collapsed <- rbind(data_collapsed, subdat)
  }
  # If there's more than one pressure variable but not all have assigned the same category but they are not all unique, combine the above options
  if(nrow(subdat) > 1 &
     length(unique(subdat$Pressure.variable_category)) > 1 &
     !length(unique(subdat$Pressure.variable_category)) == nrow(subdat)){
    
    for(iCat in unique(subdat$Pressure.variable_category)){
      subsubdat <- subset(subdat, Pressure.variable_category %in% iCat)
      
      if(nrow(subsubdat) == 1){
        
        data_collapsed <- rbind(data_collapsed, subsubdat)
      }
      
      if(nrow(subsubdat) > 1){
        
        subsubdat$Pressure_variable <- paste(subsubdat$Pressure_variable, collapse = " _ ")
        data_collapsed <- rbind(data_collapsed, subsubdat[1,])
      }
    }

  }
  
}

# Put Pressure variable category column after Pressure variable
data_collapsed <- cbind(data_collapsed[,c(1:23)], data_collapsed[,34], data_collapsed[,c(24:33)])

# Save
saveRDS(data_collapsed, paste0(datPath,"data_correctTaxa_PressVar.rds"))
write.xlsx(data_collapsed, file=paste0(datPath, "data_correctTaxa_PressVar.xlsx"))


# # In case you're running it with the entire dataset (incl. all columns)
# data_collapsed <- cbind(data_collapsed[,c(1:41)], data_collapsed[,53], data_collapsed[,c(42:52)])
# saveRDS(data_collapsed, paste0(datPath,"data_AllScreened_correctTaxa_PressVar.rds"))
# write.xlsx(data_collapsed, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar.xlsx"))

