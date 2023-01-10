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
library(openxlsx)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 5.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa_PressVar_RespVar.rds"))

# # Remove some columns to improve viewing the data frame
# data_allScreened$SearchID <- data_allScreened$Open.Access <- NULL

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
                                                                                                    "physiologcal stress experiments using haemolymph samples",
                                                                                                    "Independent electro trawls sediment cores")] <- "Irregular Fisheries Independent Survey"
# Papers that were initially categorized as 'Grab/core/dredge'
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("Independent electro trawls sediment cores",
                                                                                                    "sediment cores",
                                                                                                    "box corer sampling",
                                                                                                    "Nioz box corer",
                                                                                                    "sediment cores for sediment biogeochemistry analysis",
                                                                                                    "Benthic cores",
                                                                                                    "grab and box corer sampling",
                                                                                                    "Modified Smith-McIntyre grab",
                                                                                                    "sediment corer (hydraulically damped KC multicorer)",
                                                                                                    "Sediment core sampling",
                                                                                                    "sediment sample",
                                                                                                    "epibenthic dredge",
                                                                                                    "snapshot benthos sampling",
                                                                                                    "benthic grabs")] <- "Irregular Fisheries Independent Survey"

datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "Day grab" & datDescr$SW.ID %in% "SW4_1663"] <- "Irregular Fisheries Independent Survey"
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "Grabs" & datDescr$SW.ID %in% "SW4_1721"]    <- "Irregular Fisheries Independent Survey"

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% c("SW4_0675","SW4_1199", "SW4_1270")] <- "Irregular Fisheries Independent Survey"

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

### Regular Fisheries Independent Survey ----

# Papers that were initially categorized as 'Grab/core/dredge'
newRow                                            <- subset(datDescr, Description.Other.Sampling.Method %in% "grab and box corer sampling")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% "Benthic sampling by corer and grab"] <- "Regular Fisheries Independent Survey"

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_0675")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
newRow$Description.Other.Sampling.Method          <- NA
newRow                                            <- newRow[!duplicated(newRow),]
datDescr                                          <- rbind(datDescr, newRow)


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

# For those papers that were initially classified as 'Grab/core/dredge'
datDescr$Description.Other.Sampling.Method[datDescr$Description.Other.Sampling.Method %in% c("benthic grabs",
                                                                                             "Benthic sampling by corer and grab",
                                                                                             "Day grab",
                                                                                             "Hamon grab",
                                                                                             "grab and box corer sampling",
                                                                                             "Modified Smith-McIntyre grab",
                                                                                             "Van Veen grab",
                                                                                             "van Veen grab",
                                                                                             "Grabs",
                                                                                             "Benthic cores",
                                                                                             "box corer",
                                                                                             "box corer sampling",
                                                                                             "Independent electro trawls sediment cores",
                                                                                             "Nioz box corer",                                                                                                                                                                                                                                                                                                                                              
                                                                                             "Sediment core sampling",                                                                                                                                                                                                                                                                                                                                      
                                                                                             "sediment corer (hydraulically damped KC multicorer)",                                                                                                                                                                                                                                                                                                         
                                                                                             "sediment cores",                                                                                                                                                                                                                                                                                                                                              
                                                                                             "sediment cores for sediment biogeochemistry analysis",
                                                                                             "snapshot benthos sampling",
                                                                                             "sediment sample",
                                                                                             "epibenthic dredge")] <- NA

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


# Make sure any duplicated rows are dropped
datDescr <- datDescr[!duplicated(datDescr),]

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
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0772"]                                                         <- "Interview/questionnaire"
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_1566" & datDescr$Study.type %in% c("Questionnaire/interview")] <- "Interview/questionnaire"

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
datDescr   

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_1713",]
newRow$Sampling.Method.used.for.data.collection   <- "Other"
datDescr   <- rbind(datDescr, newRow)
datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% "SW4_1228" & datDescr$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"] <- NA

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_1713"] <- "Other"

### Fisheries Dependent Data ----
newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0596",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0731",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0943",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_0972",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_1062",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_1069",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_1177",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- datDescr[datDescr$SW.ID %in% "SW4_1355",]
newRow$Sampling.Method.used.for.data.collection   <- "Fisheries Dependent Data"
datDescr                                          <- rbind(datDescr, newRow)

### Regular Fisheries Independent Survey ----
newRow                                            <- subset(datDescr, Description.Other.Sampling.Method %in% "The North Sea First Quarter (Q1) International Bottom TrawlSurvey (IBTS)")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_0918")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1109")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1651")
newRow$Sampling.Method.used.for.data.collection   <- "Regular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

### Simulated dynamics ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0918"] <- "Simulated dynamics"

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1455")
newRow$Sampling.Method.used.for.data.collection   <- "Simulated dynamics"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_2036")
newRow$Sampling.Method.used.for.data.collection   <- "Simulated dynamics"
datDescr                                          <- rbind(datDescr, newRow)

### Irregular Fisheries Independent Survey ----
newRow                                            <- subset(datDescr, SW.ID %in% "SW4_0918")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_0984" & Description.Other.Sampling.Method %in% "Scientific trawling campaigns")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1099")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1238")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1374")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1521")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1847")
newRow$Sampling.Method.used.for.data.collection   <- "Irregular Fisheries Independent Survey"
datDescr                                          <- rbind(datDescr, newRow)

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_1489"] <- "Irregular Fisheries Independent Survey"

datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_1994"] <- "Irregular Fisheries Independent Survey"

### Stomach Contents Analyses ----
newRow                                            <- subset(datDescr, SW.ID %in% "SW4_0918")
newRow$Sampling.Method.used.for.data.collection   <- "Stomach Contents Analyses"
datDescr                                          <- rbind(datDescr, newRow)

### Strandings ----
newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1185")
newRow$Sampling.Method.used.for.data.collection   <- "Strandings"
datDescr                                          <- rbind(datDescr, newRow)

### Visual Analyses of Quadrats/Transects ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$Description.Other.Sampling.Method %in% c("UVC of angling indicator species")] <- "Visual Analyses of Quadrats/Transects"

newRow                                            <- subset(datDescr, SW.ID %in% "SW4_1315")
newRow$Sampling.Method.used.for.data.collection   <- "Visual Analyses of Quadrats/Transects"
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
                                                                                             "mechanized dredge",
                                                                                             "Van veen grabs",
                                                                                             "Cores taken along transect",
                                                                                             "Box core and trawl - sampling of sediment plume",
                                                                                             "Fisheries Dependent Data",
                                                                                             "The North Sea First Quarter (Q1) International Bottom TrawlSurvey (IBTS)",
                                                                                             "scallop dredge_2m beam trawl_underwater camera sledge",
                                                                                             "Video survey; Norwegian Mareano program",
                                                                                             "T-bar anchor tags attached at the base of the dorsal fin",
                                                                                             "Access-point surveys",
                                                                                             "reference fleet",
                                                                                             "Deep-sea trawl surveys",
                                                                                             "ROV video images were recorded during the PROMARES-OASIS DEL MAR cruise",
                                                                                             "Distribution of fishing vessels: VMS data",
                                                                                             "Van Veen Grabs",
                                                                                             "experimental fishing",
                                                                                             "interviews with fishers",
                                                                                             "MEDITS program",
                                                                                             "VMS data",
                                                                                             "Mooring line with turbidimeters and Acoustic Doppler Current Profiler (ADCP)",
                                                                                             "CTD transect",
                                                                                             "Oceanographic cruises",
                                                                                             "gillnets - 36 stations",
                                                                                             "Also bottom trawl survey ARSA (benthic fish), ECOCADIZ survey (pelagic fish), stomach content analyses (diet composition)",
                                                                                             "fisheries dependent informaton",
                                                                                             "UVC of angling indicator species",
                                                                                             "Experimental fishing",
                                                                                             "Research trawl surveys",
                                                                                             "Trawl surveys",
                                                                                             "Scientific deepwater surveys (combined with VMS data)",
                                                                                             "MEDITS survey",
                                                                                             "Scientific trawling campaigns",
                                                                                             "Fisheries Department of the Catalonia Government dataset",
                                                                                             "self-sampling from artisinal fishermen and observers",
                                                                                             "van Veen grab sampling",
                                                                                             "VMS data was used to estimate trawl intensity over the previous 10 years prior to collecting the snapshot.",
                                                                                             "Irish Groundfih Surveys",
                                                                                             "Fisheries landings from 2007 and literature information",
                                                                                             "CEFAS fisheries",
                                                                                             "MEDITS bottom trawl survey",
                                                                                             "sediment grab","CTD","dredge",
                                                                                             "Testing of Turtle Excluding Devices",
                                                                                             "experimental trawl surveys",
                                                                                             "Stable Isotope Analysis",
                                                                                             "Bycatch sampling",
                                                                                             "strandings data",
                                                                                             "Catch data",
                                                                                             "Sampling within and outsite a small and larger MPA",
                                                                                             "video/acoustic recording",
                                                                                             "And in situ obervation and data collection",
                                                                                             "Visual Analyses of Quadrats/Transects",
                                                                                             "onboard observers",
                                                                                             "epibenthic dredge and van Veen grab sampling",
                                                                                             "Side scan sonar images for trawl area, and surface dredge for epifauna and and Van Veen grab for infauna sampling",
                                                                                             "Box core and trawl",
                                                                                             "Benthos ecological model",
                                                                                             "Sampling onboard a commercial vessel and macroscopic visible damage observation",
                                                                                             "Sediment cores",
                                                                                             "TV survey",
                                                                                             "Interviews + aerial surveys",
                                                                                             "experimental fleets",
                                                                                             "benthic samples",
                                                                                             "Tissue samples for trophic level",
                                                                                             "Bottom trawl survey",
                                                                                             "0.1 m2 van Veen grab",
                                                                                             "Rapido trawler towing a box dredge, scuba divers, and sidescan sonar",
                                                                                             "Onboard a small-scale dolphinfish fishery vessel (llaüt), with a purse seine net with a 2mm mesh size.",
                                                                                             "Chartered commercial trawler",
                                                                                             "Video observation (survey) using underwater television (UWTV = low-light sensitive colour video camera) of rapido trawl's sledge/impact on trawled area, combined with quantitative analysis of by-catches and remote observations of the benthos before and after fishing",
                                                                                             "trammel nets used",
                                                                                             "Discard sampling programme onboard commercial bottom-otter trawlers, and research vessels",
                                                                                             "Side scan sonar and grab samples",
                                                                                             "Benthos sampling",
                                                                                             "Onboard a commercial turbot gillneter",
                                                                                             "Weight/length data",
                                                                                             "Comparison of predation estimates on Norway Pout from survey and model which can be used to validate impacts of fishing on non target stocks.")] <- NA

datDescr$Description.Other.Sampling.Method[datDescr$SW.ID %in% c("SW4_0186","SW4_0481","SW4_0508")] <- NA

# Remove duplicated rows
datDescr <- datDescr[!duplicated(datDescr),]



#-----------------------------------------------#
# Check when Sampling Method is Other but Description not given ----
#-----------------------------------------------#

## Active Acoustic Sampling Survey ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0424"] <- "Active Acoustic Sampling Survey"

## Simulated dynamics ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% c("SW4_0437","SW4_0798")] <- "Simulated dynamics"

## Irregular Fisheries Independent Survey ----
datDescr$Sampling.Method.used.for.data.collection[datDescr$SW.ID %in% "SW4_0440"] <- "Irregular Fisheries Independent Survey"

# Check no NAs left
unique(datDescr$SW.ID[datDescr$Sampling.Method.used.for.data.collection %in% "Other" & is.na(datDescr$Description.Other.Sampling.Method)])



#-----------------------------------------------#
# Check Analytical Method ----
#-----------------------------------------------#

# Remove weird codes
datDescr$Analytical.method.used.for.inference <- str_replace_all(datDescr$Analytical.method.used.for.inference, "_x0002_" , "")

# Check underscores without spaces and replace them by underscores with spaces
withoutSpace             <- datDescr$Analytical.method.used.for.inference[which(!str_detect(datDescr$Analytical.method.used.for.inference, " _ "))]
withoutSpaceUnderscore   <- withoutSpace[str_detect(withoutSpace, "_")]
idx                      <- which(datDescr$Analytical.method.used.for.inference %in% withoutSpaceUnderscore)
datDescr$Analytical.method.used.for.inference[idx] <- str_replace(withoutSpaceUnderscore, "_"," _ ")

## Some manual changes
datDescr$Analytical.method.used.for.inference <- str_replace_all(datDescr$Analytical.method.used.for.inference, 
                                                             "Multi-Dimensional Scaling analysis _ ANOSIM_SIMPER_GLMM_", 
                                                             "Multi-Dimensional Scaling analysis _ ANOSIM _ SIMPER _ GLMM")

datDescr$Analytical.method.used.for.inference <- str_replace_all(datDescr$Analytical.method.used.for.inference, 
                                                             "length-frequency distributions_functional diversity indices_number of species_shannon_Mann–Whitney U test", 
                                                             "length-frequency distributions _ functional diversity indices _ number of species _ shannon_Mann–Whitney U test")

# Check how many papers have NA
length(unique(datDescr$SW.ID[is.na(datDescr$Analytical.method.used.for.inference)])) #8 papers



#-----------------------------------------------#
# Save dataset ----
#-----------------------------------------------#

#Just as in Steps 3 and 4, it would be possible to collapse the data with regards to Sampling Method, e.g. multiple Sampling Methods for the
#the otherwise same row can be combined into one row. However, Sampling Method was not a free text variable (like reported Pressure variable or
#Response variable), the category Other may have unique descriptions, and the newly added Sampling Methods 'only' added around 90 additional rows.
#Therefore, the dataset is not collapsed based on Sampling Method, but this is possible in the future if deemed necessary.

# Check duplicated ROWID
rowIDdupl               <- datDescr$ROWID[duplicated(datDescr$ROWID)]
datDupl                 <- subset(datDescr, ROWID %in% rowIDdupl) #this is because rows were split or duplicated and got a new Sampling Method
datDescr$ROWID          <- c(1:nrow(datDescr)) #give new unique row ID


# Save
saveRDS(datDescr, paste0(datPath,"data_correctTaxa_PressVar_RespVar_Methods.rds"))
write.xlsx(datDescr, file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods.xlsx"))

# # In case you're running it with the entire dataset (incl. all columns)
# saveRDS(datDescr, paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods.rds"))
# write.xlsx(datDescr, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods.xlsx"))

