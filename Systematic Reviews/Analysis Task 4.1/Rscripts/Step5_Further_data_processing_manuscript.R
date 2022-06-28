#####################################################################################################################-
#####################################################################################################################-
#
#     Additional data processing of the data extraction files from SEAwise task 4.1
#     Step 5. Preparing tht data for publishing manuscript and database
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

# library(data.table)
# library(RColorBrewer)
# library(raster)
# library(plotrix)
# library(sf)
library(viridis)
library(ggplot2)
library(splitstackshape)
library(patchwork)
library(splitstackshape)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 1 and processed species file in step 3.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened.rds"))

# load(file = paste0(outPath, "FatePapers.Rdata"))


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Check response variable categories and see whether they can be merged ----
#-----------------------------------------------#

# Reduce data so that there are no duplicates for each paper regarding the ecosystem component, response variable and direction
datDeDupl                             <- data[!duplicated(data[,c("SW.ID","Ecosystem.component_level1","Response.variable_category","Direction.of.relationship")])]


##### Biodiversity & Community composition ----

# Check data categorized as biodiversity
biodiv                               <- subset(data, Response.variable_category %in% "Biodiversity")
table(biodiv$Response.variable_paper) #looks good, all related to biodiversity

# Check data categorized as community composition
comcom                               <- subset(data, Response.variable_category %in% "Community composition")
comcom$Response.variable_paper
# have made changes to the original data extraction files on 24 & 27 June 2022

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Biodiversity"])) #50
length(unique(data$SW.ID[data$Response.variable_category %in% "Community composition"]))#72

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Biodiversity"]) #mostly negative
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Community composition"]) #mostly multiple, also some negative

# Merging? Possible, but the direction is often not so straightforward for community composition as it is for biodiversity (e.g. richness increases 
# or decreases in response to fishing). The composition may change in response to fishing, but it is often not possible to say whether there was a
# positive or negative relationship.
# It also seems reviewers have distinguished between the two response variables (as anticipated), and for cases this was not done, this has
# been changed in the data extraction files.
# Based on above two reasons, OK to keep the two separate.


##### Mortality & survival ----

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Mortality"])) #64
length(unique(data$SW.ID[data$Response.variable_category %in% "Survival"])) #25

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Mortality"]) #mostly positive
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Survival"]) #mostly negative, but also some 'no impact'

# Merging? Possible by changing response variable to 'mortality' for survival studies and reverse the direction of the relationship (this because 
# there are fewer survival than mortality studies).
# Although some survival studies might particularly have focussed context-wise on survival rather than mortality, two are exchangable:
# x% survival = 1-x% mortality.



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Categorize pressure variables ----
#-----------------------------------------------#

# Expected categories in a broad sense:
# - Catch
# - Bycatch
# - Fishing pressure/intensity/effort/SAR
# - Fishing mortality
# - Closure/ban/MPA (marine protected area)
# - Before/after studies?
# - Bycatch reduction & selectivity
# - Discarding
# - Occurrence/abundance of litter/ghost nets
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

PV_catch                                  <- subset(datPressvar, Pressure.variable_category %in% "Catch")


## Bycatch ----
sort(unique(datPressvar[which(grepl("bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("Bycatch", datPressvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("by-catch", datPrssvar$Pressure_variable)),]$Pressure_variable))
sort(unique(datPressvar[which(grepl("By-catch", datPrssvar$Pressure_variable)),]$Pressure_variable))

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("bycatch",
                                                                                              "Bycatch",
                                                                                              "bycatch mortality",
                                                                                              "bycatch intensity",
                                                                                              "Bycatch risk",
                                                                                              "bycatch mortality differences between species",
                                                                                              "bycatch percentage",
                                                                                              "temporal difference in bycatch mortality",
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


PV_bycatch                                  <- subset(datPressvar, Pressure.variable_category %in% "Bycatch")
## NOTE: "bycatch exclusion device" will need to go under another category


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

sort(unique(datPressvar[which(grepl("fishing pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all but MPA
sort(unique(datPressvar[which(grepl("Fishing pressure", datPressvar$Pressure_variable)),]$Pressure_variable)) #add all but trawled. vs. non trawled

datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(Pressure_variable %in% c("Exploitation rate (mean community fishing pressure)",
                                                                                              "fishing pressure",
                                                                                              "fishing pressure (hours per habitat surface area)",
                                                                                              "fishing pressure index",
                                                                                              "Fishing pressure",
                                                                                              "Fishing pressure as desired catch"),
                                                                     "Fishing effort",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("hour", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #add all
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

sort(unique(datPressvar[which(grepl("Swept area", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("Swept area", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category),
                                                                     "Fishing effort",Pressure.variable_category))

PV_fishingEffort                          <- subset(datPressvar, Pressure.variable_category %in% "Fishing effort")


## Mortality ----
sort(unique(datPressvar[which(grepl("mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Mortality", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse((grepl("mortality", datPressvar$Pressure_variable) | grepl("Mortality", datPressvar$Pressure_variable)) 
                                                                     & is.na(datPressvar$Pressure.variable_category),
                                                                     "Mortality",Pressure.variable_category))

sort(unique(datPressvar[which(grepl("exploitation", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #all
sort(unique(datPressvar[which(grepl("Exploitation", datPressvar$Pressure_variable) & is.na(datPressvar$Pressure.variable_category)),]$Pressure_variable)) #don't include
datPressvar$Pressure.variable_category    <- with(datPressvar,ifelse(grepl("exploitation", datPressvar$Pressure_variable) 
                                                                     & is.na(datPressvar$Pressure.variable_category),
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



PV_litter                        <- subset(datPressvar, Pressure.variable_category %in% "Litter")


table(datPressvar$Pressure.variable_category, useNA = "always")

papers                                <- datPressvar[is.na(datPressvar$Pressure.variable_category),]
sort(unique(papers$Pressure_variable))


# use of FADs and keep net to Bycatch reduc?
# # 'low pressure jet' -> more general category: Improvind selecting & reducing ecological impact of gear?
# What to do with 'disturbance'?


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Check studies on MPAs, closures and bans ----
#
# Note: this excludes bycatch studies, e.g. 'absence of sievenets in the trawl' or 'use of TED'
#-----------------------------------------------#

# Check for possible wordings in pressure variable that could refer to this
sort(unique(data$Pressure_variable))

wordings <- c("Absence of fishing effort", 
              "Absence of sievenets in the trawl",
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
# NOTE: papers for which reported has been done reversely should not be included (e.g. they studied ban on fishing, but reported then the opposite,
# so that the table would be read as 'increase in )
studiesClosure    <- c("SW4_1042","SW4_1136")

