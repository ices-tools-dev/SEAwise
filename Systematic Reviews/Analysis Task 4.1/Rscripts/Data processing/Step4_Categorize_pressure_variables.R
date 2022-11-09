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


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 3.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa.rds"))

data                                  <- data_allScreened #in case you'd like to run with the entire dataset (incl. all columns)


#-----------------------------------------------#
# Categorize pressure variables ----
#-----------------------------------------------#

# Expected categories in a broad sense:
# - Catch
# - Bycatch
# - Fishing pressure/intensity/effort/SAR
# - Presence of fishing activity
# - Fleet capacity
# - Mortality
# - Closure/ban/MPA (marine protected area)
# - Before/after studies? -> only 4 studies...
# - Bycatch reduction & selectivity
# - Discarding
# - Marine litter & ghost nets
# - Electromagnetic input
# - Light
# - Noise


# To do: try to classify each paper under one of the categories as 'PV_xxx'

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
                                                                                              "Catches per year"),
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

sort(unique(datPressvar[which(grepl("landing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Landing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all

# Add papers regarding landings
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(Pressure_variable %in% c("Fishing landings",
                                                                                               "fishing landings",
                                                                                               "fraction landings",
                                                                                               "Total landings",
                                                                                               "Landings") &
                                                                        is.na(Pressure.variable_category),
                                                                      "Catch",Pressure.variable_category))

# Add paper on how fishing on small pelagics affects food availability for dolphins in the Med (SW4_0081)
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% "SW4_0081" &
                                                                        is.na(Pressure.variable_category),
                                                                      "Catch",Pressure.variable_category))

# Add paper that assesses trait composition of fished assemblages
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Biological trait removal by fishing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0633",
                                                                     "Catch",Pressure.variable_category))

# Add paper that studies effect of biomass removal (besides fishing mortality and fishing effort)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("biomass", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1296",
                                                                     "Catch",Pressure.variable_category))

# Add paper that studies effect of crowding in the net on mortality
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("crowding", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0115",
                                                                     "Catch",Pressure.variable_category))

# Add paper that studies effect of catching process on behaviour and biochemical aspects of target species
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("mechanical shaking", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_2031",
                                                                     "Catch",Pressure.variable_category))

PV_catch                                  <- subset(datPressvar, Pressure.variable_category %in% "Catch")

#CHECK whether 0081, 0633, 1296 are included


## Bycatch ----
sort(unique(datPressvar[which(grepl("bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("Bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("by-catch", datPrssvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("By-catch", datPrssvar$Pressure_variable)),]$Pressure_variable))

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
                                                                                                                                 "Catches per year"),
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
# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "Fishing activity" & SW.ID %in% "SW4_0350",
                                                                     "Bycatch", Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% "fraction discards" & SW.ID %in% "SW4_0722",
                                                                     "Bycatch", Pressure.variable_category))

PV_bycatch                                  <- subset(datPressvar, Pressure.variable_category %in% "Bycatch")
## NOTE: studies on bycatch exclusion devices will  go under another category


## Fishing effort ----
sort(unique(datPressvar[which(grepl("fishing effort", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("fishing effort", datPressvar$Pressure_variable),
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

sort(unique(datPressvar[which(grepl("duration", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all but food patches
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
                                                                                              "number of vessels"),
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

sort(unique(datPressvar[which(grepl("frequency", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but marine litter
sort(unique(datPressvar[which(grepl("Frequency", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all but marine litter
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("field frequency",
                                                                                              "fishing frequency",
                                                                                              "High fishing frequency",
                                                                                              "Immediate trawling (low fishing frequency)",
                                                                                              "swept frequency",
                                                                                              "trawl frequency",
                                                                                              "trawl frequency per rectangle",
                                                                                              "trawling frequency",
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

# Add papers manually after having checked papers in detail
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("fishing activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0543","SW4_0596","SW4_0120","SW4_0024",
                                                                                    "SW4_0252","SW4_0307"),
                                                                     "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Fishing activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0024","SW4_0120","SW4_0307","SW4_0465", 
                                                                                    "SW4_0531"),
                                                                     "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Trawling activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% c("SW4_0253", "SW4_0361"),
                                                                     "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Fishing capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0252",
                                                                     "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("beam trawl vs pulse trawl", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0153",
                                                                     "Fishing effort",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Bottom trawling", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_1960",
                                                                     "Fishing effort",Pressure.variable_category))

PV_fishingEffort                          <- subset(datPressvar, Pressure.variable_category %in% "Fishing effort")

## CHECK whether SW4_0111, SW4_0153, SW4_1960 is included?


## Presence of fishing activity ----
sort(unique(datPressvar[which(grepl("presence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Presence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("presence", datPressvar$Pressure_variable),
                                                                     "Presence of fishing activity",Pressure.variable_category))
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Presence", datPressvar$Pressure_variable),
                                                                     "Presence of fishing activity",Pressure.variable_category))


sort(unique(datPressvar[which(grepl("activity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add some
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("activity", datPressvar$Pressure_variable) & 
                                                                       SW.ID %in% c("SW4_0717","SW4_1833","SW4_1724"),
                                                                     "Presence of fishing activity",Pressure.variable_category))
#CHECK whether SW4_0619 'Presence of trawl marks', SW4_1724 'Fishing activity' is included

# 'before and after' paper SW4_1209 should be checked together with other before/after papers -> likely put under Fishing effort


# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(SW.ID %in% c("SW4_1485","SW4_1727","SW4_0359","SW4_0383") & is.na(Pressure.variable_category),
                                                                     "Presence of fishing activity",Pressure.variable_category))




PV_activity                          <- subset(datPressvar, Pressure.variable_category %in% "Presence of fishing activity")



## Fleet capacity ----
sort(unique(datPressvar[which(grepl("capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #some
sort(unique(datPressvar[which(grepl("Capacity", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #none
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("capacity", datPressvar$Pressure_variable) 
                                                                     & SW.ID %in% c("SW4_0516","SW4_1807","SW4_0179"),
                                                                     "Fleet capacity",Pressure.variable_category))
# Include some manually
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% c("SW4_1867","SW4_0870","SW4_0738"),
                                                                     "Fleet capacity",Pressure.variable_category))

PV_capacity                          <- subset(datPressvar, Pressure.variable_category %in% "Fleet capacity")


## Mortality ----
sort(unique(datPressvar[which(grepl("mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse((grepl("mortality", datPressvar$Pressure_variable) | grepl("Mortality", datPressvar$Pressure_variable)) 
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

PV_mortality                          <- subset(datPressvar, Pressure.variable_category %in% "Mortality")


## Closure/ban/MPA ----
sort(unique(datPressvar[which(grepl("closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("closure", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("ban", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #only those referring to ban
sort(unique(datPressvar[which(grepl("Ban", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Fishing ban",
                                                                                              "Regulatory ban on capelin",
                                                                                              "trawl ban",
                                                                                              "Trawl ban",
                                                                                              "trawling ban"),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("MPA", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("MPA", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("protected", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure.variable_category %in% "Marine protected area",
                                                                     "Closure",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("absence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all (only one)
sort(unique(datPressvar[which(grepl("Absence", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #not this one, as it is bycatch study
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure.variable_category %in% "Marine protected area",
                                                                     "Closure",Pressure.variable_category))

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

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar, ifelse(SW.ID %in% c("SW4_1343") & is.na(Pressure.variable_category),
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("bar spacing", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category) &
                                                                       SW.ID %in% "SW4_0679",
                                                                     "Bycatch reduction & selectivity",Pressure.variable_category))

PV_bycatchRedSel                         <- subset(datPressvar, Pressure.variable_category %in% "Bycatch reduction & selectivity")



## Discarding ----
sort(unique(datPressvar[which(grepl("discard", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("discard", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("Discard", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Discard", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("food", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("food", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Discarding",Pressure.variable_category))

# Some manual additions
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Discard", datPressvar$Pressure_variable) & 
                                                                       SW.ID %in% "SW4_1485" & is.na(Pressure.variable_category),
                                                                     "Presence of fishing activity",Pressure.variable_category))

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

sort(unique(datPressvar[which(grepl("Entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Entang", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #no results
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("ingest", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("lost", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("lost", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Litter",Pressure.variable_category))

PV_litter                        <- subset(datPressvar, Pressure.variable_category %in% "Litter")


######### Check papers still left to categorize
table(datPressvar$Pressure.variable_category, useNA = "always")

papers                                <- datPressvar[is.na(datPressvar$Pressure.variable_category),]
sort(unique(papers$Pressure_variable))



#SW4_0619: removed one row where impact of litter or trawling was not directly studied by the paper itself (Pressure variable 'Anthropogenic disturbance (Trawling and litter)')
# -> check whether this is excluded next time
# 'Pressure variable 'Trawling impact': have changed to 'Presence of trawl marks' -> can go under PResence of fishing activity


# - use of FADs (SW4_1758)?
# - 'low pressure jet' -> more general category: Improving selectivity & reducing ecological impact of gear?
# - What to do with 'disturbance'?



#-----------------------------------------------#
# Temporarily save dataset ----
#-----------------------------------------------#

# Convert dataPressvar back into multiple pressure variables per row. Consideration is needed when a row has been split
# into multiple ones. If all split rows got assigned the same Pressure variable category, then all can be merged into
# one row again. If split rows got assigned different Pressure variable categories, then they have to remain split.

# Add pasted info for rows with multiple pressure variable categories
data_collapsed <- datPressvar[0,]

for (iRow in unique(datPressvar$ROWID)){
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


# In case you're running it with the entire dataset (incl. all columns)
data_collapsed <- cbind(data_collapsed[,c(1:41)], data_collapsed[,53], data_collapsed[,c(42:52)])
saveRDS(data_collapsed, paste0(datPath,"data_AllScreened_correctTaxa_PressVar.rds"))
write.xlsx(data_collapsed, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar.xlsx"))


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Check studies on MPAs, closures and bans ----
#-----------------------------------------------#

# Check for possible wordings in pressure variable that could refer to this
sort(unique(data$Pressure_variable))

wordings <- c("Absence of fishing effort", 
              "close nursery areas",
              "closed area vs unclosed area ",
              "Decrease on number of fishing days",
              "Effect of a fishery closure - Plaice box",
              "effort_mpa",
              "fishery closures",
              "fishery/no fishery area",
              "Fishing activity presence",
              "Fishing ban",
              "Fishing effort decline",
              "fishing presence",
              "Fishing presence ",
              "fishing pressure (MPA)",
              "Fishing pressure: trawled vs. Non trawled areas",
              "Fishing restriction",
              "Marine protected area",
              "MPA",
              "MPA BACI",
              "No pressure variable (presence of cuttlefish traps)",
              "None",
              "Outcomes inside and outside MPAs",
              "presence of fishing vessels",
              "presence of gillnets",
              "presence of trawling",
              "Presence of trawling activity",
              "presence/absence of trawling",
              "Protection level",
              "Regulatory ban on capelin",
              "reserve protection level",
              "reserve protection level _ year",
              "Spawning closures for plaice, sole, and place and sole combined.",
              "spearfishing prohibition",
              "trawl ban",
              "trawl ban ",
              "Trawl ban",
              "trawling ban",
              "Trawling presence",
              "Trawling presence ",
              "Trawling Presence _ pre and post fishing ban",
              "Trawling vs. Trawler Moratorium",
              "Untrawled area")

# Check studies using such wordings
studies                            <- subset(data_allScreened, Pressure_variable %in% wordings)
length(unique(studies$SW.ID)) #51 studies

# Check the direction of relationship
studiesDeDup                       <- studies[!duplicated(studies[,c("SW.ID","Ecosystem.component_level1","Response.variable_category","Direction.of.relationship")])]
table(studiesDeDup$Direction.of.relationship) #mostly positive

# For studies with 'positive' effect, what's the pressure variable?
table(studiesDeDup$Pressure_variable[studiesDeDup$Direction.of.relationship %in% "Positive"])
length(unique(studiesDeDup$Pressure_variable[studiesDeDup$Direction.of.relationship %in% "Positive"])) #24 pressure variables out of the 43 wordings

# Go through the papers and mark those which report on the effect of the MPA/ban/closure/absence of fishing
# NOTE: papers for which reporting has been done reversely should not be included (e.g. they studied ban on fishing, but reported then the opposite,
# so that the table would be read as 'increase in )
studiesClosure    <- c("SW4_1042","SW4_1136")

