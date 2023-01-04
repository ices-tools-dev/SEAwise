#####################################################################################################################-
#####################################################################################################################-
#
#     Step 6. Process methods
#
#     By Esther Beukhof
#     Dec 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#
library(data.table)
library(splitstackshape)
library(stringr)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 5.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa_PressVar_RespVar.rds"))

# Remove some columns to improve viewing the data frame
data_allScreened$SearchID <- data_allScreened$Open.Access <- NULL

# data                                  <- data_allScreened #in case you'd like to run with the entire dataset (incl. all columns)


#-----------------------------------------------#
# Check Sampling Methods ----
#-----------------------------------------------#

# Create dataset where one row represents unique combination of SW.ID and Sampling method
datSampl           <- data
datSampl$samplID   <- paste(datSampl$SW.ID, datSampl$Sampling.Method.used.for.data.collection)
datSampl           <- datSampl[!duplicated(datSampl$samplID),]

# Check how often methods occur
as.data.frame(sort(table(datSampl$Sampling.Method.used.for.data.collection, useNA = "ifany"), decreasing = TRUE)) 
#mostly fisheries dependent data or fisheries independent surveys
#no NAs
#quite some cases where 'Other' has been chosen: 83 papers


#-----------------------------------------------#
# Check Description of Other Sampling Method for 'Other' Sampling Method & create new categories ----
#-----------------------------------------------#

# Split dataset by Description column
datDescr            <- cSplit(data, "Description.Other.Sampling.Method", " _ ", "long")

# Create dataset where one row represents unique combination of SW.ID, Sampling method and Description of Other Sampling method
datMethod           <- datDescr
datMethod$samplID   <- paste(datMethod$SW.ID, datMethod$Sampling.Method.used.for.data.collection, datMethod$Description.Other.Sampling.Method)
datMethod           <- datMethod[!duplicated(datMethod$samplID),]
otherMethods        <- sort(unique(datMethod$Description.Other.Sampling.Method))


## NEW: Grab, core and dredge ----
sort(unique(datDescr[which(grepl("grab", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 
sort(unique(datDescr[which(grepl("Grab", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% c("benthic grabs",
                                                                                                                    "Benthic sampling by corer and grab",
                                                                                                                    "Day grab",
                                                                                                                    "Hamon grab",
                                                                                                                    "grab and box corer sampling",
                                                                                                                    "Modified Smith-McIntyre grab",
                                                                                                                    "Van Veen grab",
                                                                                                                    "van Veen grab",
                                                                                                                    "Grabs") &
                                                                     Sampling.Method.used.for.data.collection %in% "Other", "Grab/core/epibenthic dredge", Sampling.Method.used.for.data.collection))

sort(unique(datDescr[which(grepl("core", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 
sort(unique(datDescr[which(grepl("Core", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% c("Benthic cores",
                                                                                                                    "box corer",
                                                                                                                    "box corer sampling",
                                                                                                                    "Independent electro trawls sediment cores",
                                                                                                                    "Nioz box corer",                                                                                                                                                                                                                                                                                                                                              
                                                                                                                    "Sediment core sampling",                                                                                                                                                                                                                                                                                                                                      
                                                                                                                    "sediment corer (hydraulically damped KC multicorer)",                                                                                                                                                                                                                                                                                                         
                                                                                                                    "sediment cores",                                                                                                                                                                                                                                                                                                                                              
                                                                                                                    "sediment cores for sediment biogeochemistry analysis") &
                                                                             Sampling.Method.used.for.data.collection %in% "Other", "Grab/core/epibenthic dredge", Sampling.Method.used.for.data.collection))

sort(unique(datDescr[which(grepl("dredge", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% "epibenthic dredge" &
                                                                             Sampling.Method.used.for.data.collection %in% "Other", "Grab/core/epibenthic dredge", Sampling.Method.used.for.data.collection))

# Some manual additions
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("snapshot benthos sampling",
                                                                                                    "sediment sample")] <- "Grab/core/epibenthic dredge"

# Make Description column NA
datDescr$Description.Other.Sampling.Method[datDescr$Sampling.Method.used.for.data.collection %in% "Grab/core/epibenthic dredge"] <- NA



## NEW: Strandings ----

sort(unique(datDescr[which(grepl("strand", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 
sort(unique(datDescr[which(grepl("Strand", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

## Manual adjustment
datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% "SW4_0261"] <- "Count of strandings of 'submerged derelict fishing gears in expanded PVC'"

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% c("Count of strandings",                                                                                                                                                                                    
                                                                                                                    "Count of strandings of 'submerged derelict fishing gears in expanded PVC'",
                                                                                                                    "dead-stranded individuals found on the coast",                                                                                                                                                            
                                                                                                                    "Records of sea turtles found on the coast (‘stranded’) or gathered at sea (‘floating’; not found in fishing gear) were collected from the databases of six projects and from published lists of records.",
                                                                                                                    "reports of stranded dolphins",                                                                                                                                                                            
                                                                                                                    "stranding network",
                                                                                                                    "Strandings network locations/carcass reverse drift modelling") &
                                                                             Sampling.Method.used.for.data.collection %in% "Other", "Strandings", Sampling.Method.used.for.data.collection))

# Make Description column NA
datDescr$Description.Other.Sampling.Method[datDescr$Sampling.Method.used.for.data.collection %in% "Strandings"] <- NA



## NEW: Interview/questionnaire ----

sort(unique(datDescr[which(grepl("interview", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 
sort(unique(datDescr[which(grepl("Interview", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% c("Fisher interview",
                                                                                                                    "interviews",
                                                                                                                    "interviews with fishers",
                                                                                                                    "Interviews") &
                                                                             Sampling.Method.used.for.data.collection %in% "Other", "Interview/questionnaire", Sampling.Method.used.for.data.collection))

sort(unique(datDescr[which(grepl("quest", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 
sort(unique(datDescr[which(grepl("Quest", datDescr$Description.Other.Sampling.Method) & datDescr$Sampling.Method.used.for.data.collection %in% "Other"),]$Description.Other.Sampling.Method)) 

datDescr$Sampling.Method.used.for.data.collection <- with(datDescr, ifelse(Description.Other.Sampling.Method %in% c("questionnaire",
                                                                                                                    "Questionaries",
                                                                                                                    "Questionary") &
                                                                             Sampling.Method.used.for.data.collection %in% "Other", "Interview/questionnaire", Sampling.Method.used.for.data.collection))

# Some manual additions
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "traditional fisheries knowledge"] <- "Interview/questionnaire"

# Make Description column NA
datDescr$Description.Other.Sampling.Method[datDescr$Sampling.Method.used.for.data.collection %in% "Interview/questionnaire"] <- NA



## Categorize under existing methods ----

### Visual Analyses of Quadrats/Transects ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("Aereal Survey",
                                                                                                    "Aerial Survey",
                                                                                                    "Autonomous underwater video",
                                                                                                    "Distance sampling from aircraft and ships obtained from third-party database",
                                                                                                    "ROV video",
                                                                                                    "ROV Video")] <- "Visual Analyses of Quadrats/Transects"

### Behavioural Observations ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "reburrowing behaviour sampled with camera"] <- "Behavioural Observations"

### Irregular Fisheries Independent Survey ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("Fish pots with and without Seal exclusion device",
                                                                                                    "adenylate energy charge of clam muscle samples collected after dredging in the field and after simulated stress in laboratory experiement",
                                                                                                    "physiologcal stress experiments using haemolymph samples")] <- "Irregular Fisheries Independent Survey"

### In situ structural growth ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "In situ growth experiment"] <- "In situ structural growth"

### Fisheries Dependent Data ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("market sampling",
                                                                                                    "Observer program",                                                                                                                                                                                                                                                                                                                                            
                                                                                                    "onboard observer program",
                                                                                                    "Opportunistic observations during the fishing hauls",
                                                                                                    "using VMS data for SAR and calculating 3 benthic impact indicators")] <- "Fisheries Dependent Data"
### Active Acoustic Sampling Survey ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("Multibeam",
                                                                                                    "Side scan sonar")] <- "Active Acoustic Sampling Survey"

### Simulated dynamics ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0955" & datDescr$Sampling.Method.used.for.data.collection %in% "Other"] <- "Simulated dynamics"

datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "Input parameters were mainly compiled from available published and unpublished information of the Istituto di Scienze Marine — Sede di Ancona (CNR– ISMAR, Italy). Biomass values (Bi) were obtained from data collection using the swept area method, sediment cores, bottom dredge sampling, acoustic surveys and information available in the literature."] <- "Simulated dynamics"


# Make Description column NA
datDescr$Description.Other.Sampling.Method[datDescr$Description.Other.Sampling.Method %in% c("adenylate energy charge of clam muscle samples collected after dredging in the field and after simulated stress in laboratory experiement",  
                                                                                             "Aereal Survey",
                                                                                             "Aerial Survey",
                                                                                             "Autonomous underwater video",
                                                                                             "Distance sampling from aircraft and ships obtained from third-party database",
                                                                                             "ROV video",
                                                                                             "ROV Video",
                                                                                             "reburrowing behaviour sampled with camera",
                                                                                             "Fish pots with and without Seal exclusion device",
                                                                                             "Input parameters were mainly compiled from available published and unpublished information of the Istituto di Scienze Marine — Sede di Ancona (CNR– ISMAR, Italy). Biomass values (Bi) were obtained from data collection using the swept area method, sediment cores, bottom dredge sampling, acoustic surveys and information available in the literature.",
                                                                                             "In situ growth experiment",
                                                                                             "market sampling",
                                                                                             "Observer program",                                                                                                                                                                                                                                                                                                                                            
                                                                                             "onboard observer program",
                                                                                             "physiologcal stress experiments using haemolymph samples",
                                                                                             "Multibeam",
                                                                                             "Opportunistic observations during the fishing hauls",
                                                                                             "Side scan sonar",
                                                                                             "using VMS data for SAR and calculating 3 benthic impact indicators")] <- NA



## Remove or change descriptions ----

datDescr <- subset(datDescr, !Description.Other.Sampling.Method %in% c("BACI design",
                                                                       "Chemical analyses",
                                                                       "spatialized environmental data"))

datDescr <- subset(datDescr, !(Description.Other.Sampling.Method %in% "environmental data" & SW.ID %in% c("SW4_1364","SW4_1962","SW4_1987")))

#Manual change for Sw4_0115 which erroneously described paper as in vitro and lab experiment (experiment was done in aquaculture cage)
datDescr$Description.Other.Sampling.Method[datDescr$Description.Other.Sampling.Method %in% "In vitro tank experiments"] <- "Crowding experiment in aquaculture cage"
datDescr$Study.type[datDescr$SW.ID %in% "SW4_0115"]                                                                     <- "Field experiment/observations"

datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% c("SW4_0955","SW4_1133")] <- NA

datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% "SW4_0904"] <- "Existing fishing effort and biological data from various sources"

datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% "SW4_1707"] <- "Scientific literature on the impact of trawling on benthos" #correct spelling mistake

datDescr$Description.Other.Sampling.Method[datDescr$Description.Other.Sampling.Method %in% "Database based on publications/ reports..."] <- "Database on publications/reports" 

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_2031" & datDescr$Study.type %in% "Field experiment/observations"]


### Following papers are still categorized as Other
sort(unique(datDescr$Description.Other.Sampling.Method[datDescr$Sampling.Method.used.for.data.collection %in% "Other"]))




#-----------------------------------------------#
# Check Description of Other Sampling Method for already classified Sampling Methods ----
#-----------------------------------------------#

# How many papers have a description while Sampling Method is already filled in
datSamplDescr         <- subset(datDescr, !(Sampling.Method.used.for.data.collection %in% "Other") & !is.na(Description.Other.Sampling.Method))
length(unique(datSamplDescr$SW.ID)) #quite many papers


## Change or add Sampling Method ----

### Interview/questionnaire ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0007" & datDescr$Study.type %in% c("Questionnaire/interview")] <- "Interview/questionnaire"

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0481" & datDescr$Study.type %in% c("Questionnaire/interview")] <- "Interview/questionnaire"


### Grab/core/epibenthic dredge ----
newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0118",]
newRow$Sampling.Method.used.for.data.collection   <- "Grab/core/epibenthic dredge"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0383",]
newRow$Sampling.Method.used.for.data.collection   <- "Grab/core/epibenthic dredge"
datDescr                                          <- rbind(datDescr, newRow)

### Behavioural Observations ----
newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0131",]
newRow$Sampling.Method.used.for.data.collection   <- "Behavioural observations"
datDescr                                          <- rbind(datDescr, newRow)

### Active Acoustic Sampling Survey ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("side scan sonar")] <- "Active Acoustic Sampling Survey"

### Other ----
newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0388",]
newRow$Sampling.Method.used.for.data.collection   <- "Other"
newRow$Description.Other.Sampling.Method          <- "lugworms were dug using either an Alvey bait pump, fork or shovel"
datDescr                                          <- rbind(datDescr, newRow)


## Make Description column NA ----
datDescr$Description.Other.Sampling.Method[datDescr$Description.Other.Sampling.Method %in% c("& local ecological knowledge",
                                                                                             "survey to investigate the direct clam fishing effects (through damage)",
                                                                                             "Roses otter trawl fishing vessel using a commercial bottom trawl net with a cod-end with a square mesh size of 40 mm.",
                                                                                             "traditional deep-water cast nets",
                                                                                             "Behavioural observations",
                                                                                             "observation of catch",
                                                                                             "GPS tagging",
                                                                                             "Comparison of the depth values recorded by the sensors during fishing operations to the bottom depth data allowed estimating the bottom impact of the PS fleet",
                                                                                             "Data provided from previous studies",
                                                                                             "Grid whereof in quadrants the amount of faecal casts were counted._Every 3 to 5 sampling points, lugworms were dug using either an Alvey bait pump",
                                                                                             "Scuba diving_three replicate belt-transects of 25×5m (125m2)",
                                                                                             "The beam trawl used had horizontal and vertical openings of 2 m and 0.5 m, respectively, and a cod-end mesh size of 5 mm.",
                                                                                             "mechanized dredge")] <- NA

datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% c("SW4_0118","SW4_0186","SW4_0383","SW4_0481","SW4_0508")] <- NA
