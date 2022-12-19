#####################################################################################################################-
#####################################################################################################################-
#
#     Step 5. Process and clean Response variable and Direction of relationship, and create new version of the data
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
#  This section depends on the processed data file produced in step 4.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa_PressVar.rds"))

# Remove some columns to improve viewing the data frame
data_allScreened$SearchID <- data_allScreened$Open.Access <- NULL

# data                                  <- data_allScreened #in case you'd like to run with the entire dataset (incl. all columns)


#-----------------------------------------------#
# Data preparation ----
#-----------------------------------------------#

# Check whether there are any double spaces (as this gives issues when doing the splitting)
table(grepl("  ", data$Response.variable_paper)) #several cases

# Replace double spacing by single spacing
data$Response.variable_paper <- str_squish(data$Response.variable_paper)

# Create version of the dataset where reported Response variable is split
datRespvar                             <- cSplit(data, "Response.variable_paper", " _ ", "long")


#-----------------------------------------------#
# Check whether response variable categories can be merged ----
#-----------------------------------------------#

# Reduce data so that there are no duplicates for each paper regarding the ecosystem component, response variable and direction
datDeDupl                             <- data[!duplicated(data[,c("SW.ID","Ecosystem.component_level1","Response.variable_category","Direction.of.relationship")])]


##### Biodiversity & Community composition ----

# Check data categorized as biodiversity
biodiv                               <- subset(data, Response.variable_category %in% "Biodiversity")
sort(unique(biodiv$Response.variable_paper)) #looks good now, had made some changes to the data extraction files in June and Dec 2022

# Check data categorized as community composition
comcom                               <- subset(data, Response.variable_category %in% "Community composition")
sort(unique(comcom$Response.variable_paper))
# have made changes to the original data extraction files on 24 & 27 June 202, and Dec 2022

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Biodiversity"])) #55
length(unique(data$SW.ID[data$Response.variable_category %in% "Community composition"]))#74

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Biodiversity"]) #mostly negative
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Community composition"]) #mostly multiple, also quite some negative

# Merging? Possible, but the direction is often not so straightforward for community composition as it is for biodiversity (e.g. richness increases 
# or decreases in response to fishing). The composition may change in response to fishing, but it is often not possible to say whether there was a
# positive or negative relationship.
# It also seems reviewers have distinguished between the two response variables (as anticipated), and for cases this was not done, this has
# been changed in the data extraction files.
# Based on above two reasons, OK to keep the two separate.



##### Mortality & survival ----

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Mortality"])) #67
length(unique(data$SW.ID[data$Response.variable_category %in% "Survival"])) #26

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Mortality"]) #mostly positive
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Survival"]) #mostly negative or 'no impact'

# Check categorized Pressure variable
table(datDeDupl$Pressure.variable_category[datDeDupl$Response.variable_category %in% "Mortality"]) #mostly Fishing effort and Bycatch
table(datDeDupl$Pressure.variable_category[datDeDupl$Response.variable_category %in% "Survival"]) #mostly Bycatch

# Merging? Possible by changing response variable to 'mortality' for survival studies and reverse the direction of the relationship (because 
# there are fewer survival than mortality studies).
# Although some survival studies might particularly have focussed context-wise on survival rather than mortality, the two are exchangeable:
# x% survival = 1-x% mortality.

# Perhaps good to first await how the case studies for the manuscript develop before merging them.

# # Merge by converting Survival to mortality
# SurvToMort <- data[data$Response.variable_category %in% "Survival",]
# table(SurvToMort$Direction.of.relationship)
# 
# SurvToMort$Direction.of.relationship <- with(SurvToMort, ifelse(Direction.of.relationship %in% "Positive","Negative",
#                                                                 ifelse(Direction.of.relationship %in% "Negative","Positive",Direction.of.relationship)))
# table(SurvToMort$Direction.of.relationship)
# 
# SurvToMort$Response.variable_category <- "Mortality"



#-----------------------------------------------#
# Check 'Other' category ----
#-----------------------------------------------#

# Create subset of data classified as Other
other      <- subset(datRespvar, Response.variable_category %in% 'Other')
length(unique(other$SW.ID)) #69 papers

# Check Ecosystem component
table(other$Ecosystem.component_level1) #mostly physical habitats, followed by benthos

# Check Pressure type and variable
table(other$Pressure.type) #mostly physical disturbance
table(other$Pressure.variable_category) #mostly fishing effort

# Check Direction
table(other$Ecosystem.component_level1, other$Direction.of.relationship) #mostly negative

# Check reported RV for ECL Physical habitats
other_phy      <- subset(other, Ecosystem.component_level1 %in% 'Physical_habitats')
sort(other_phy$Response.variable_paper)

# Similar for benthos?
other_ben      <- subset(other, Ecosystem.component_level1 %in% 'Benthos')
sort(other_ben$Response.variable_paper) #some overlap, maybe due to ECL being more like Physical habitats -> should be checked next



#-----------------------------------------------#
# Categorization ----
#-----------------------------------------------#

## Below new categories are created ##

### Sediment, seabed & physical properties ----
sort(unique(datRespvar[which(grepl("sedim", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Sedim", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("sedim", datRespvar$Response.variable_paper),
                                                                     "Sediment & physical properties", Response.variable_category))
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("Sedim", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("grain", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all (only one)
sort(unique(datRespvar[which(grepl("Grain", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all (only one)
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("median grain size (Q50)",
                                                                                                  "Grain size"),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("organic", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Organic", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #empty
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("organic", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("concentr", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Concentr", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #empty
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("Excess 210-Pb concentrations",
                                                                                                  "pH and solute concentrations",
                                                                                                  "210Pb"),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("flux", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Flux", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #empty
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("flux", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("susp", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all (only one)
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("susp", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("seafloor", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("seafloor", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("Area", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("seafloor", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("scar", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("seafloor", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

# Some manual additions
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("Roughness",
                                                                                                  "redox depth",
                                                                                                  "rugosity distribution",
                                                                                                  "seabed morphology",
                                                                                                  "water turbidity",
                                                                                                  "Chemical assay",
                                                                                                  "Carbon mineralisation",
                                                                                                  "chlorophyll-a",
                                                                                                  "selection coefficient",
                                                                                                  "surface thickness",
                                                                                                  "thickness of layers"),
                                                                   "Sediment & physical properties", Response.variable_category))

datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("Area impacted by gears",
                                                                                                  "Trawl door scars on the bottom",
                                                                                                  "presence of holes and fauna",                                                                    
                                                                                                  "presence of tracks",
                                                                                                  "Benthic habitat quality (BHQ)",
                                                                                                  "Benthic impact in terms of fishing effort",
                                                                                                  "Percentage impacted",
                                                                                                  "habitat vulnerability",
                                                                                                  "Number of trawl marks",
                                                                                                  "Penetration",
                                                                                                  "Recoverability after physical disturbance",
                                                                                                  "Sensitivity to physical disturbance",
                                                                                                  "Vulnerability to physical disturbance",
                                                                                                  "Various types of seabed substrate disturbance"),
                                                                   "Sediment & physical properties", Response.variable_category))

datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% "Bottom contact" & Ecosystem.component_level1 %in% "Physical_habitats",
                                                                   "Sediment & physical properties", Response.variable_category))

RV_sediment <- subset(datRespvar, Response.variable_category %in% "Sediment & physical properties")
#CHECK SW4_0133



### Damage & entanglement ----

## Check also those that are already assigned to another category than Other, as they may also be put under this new category.

sort(unique(datRespvar[which(grepl("damage", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Damage", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("damage", datRespvar$Response.variable_paper),
                                                                   "Damage & entanglement", Response.variable_category))
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("Damage", datRespvar$Response.variable_paper),
                                                                   "Damage & entanglement", Response.variable_category))

sort(unique(datRespvar[which(grepl("entangl", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Entangl", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("entangl", datRespvar$Response.variable_paper),
                                                                   "Damage & entanglement", Response.variable_category))
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("Entangl", datRespvar$Response.variable_paper),
                                                                   "Damage & entanglement", Response.variable_category))

sort(unique(datRespvar[which(grepl("broken", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Broken", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #empty
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("broken", datRespvar$Response.variable_paper),
                                                                   "Damage & entanglement", Response.variable_category))

# Some manual additions
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("Observation of pale tissue",
                                                                                                  "scars on shells",
                                                                                                  "Mud cover",
                                                                                                  "Fragility",
                                                                                                  "Frequency of deep-hooking"),
                                                                   "Damage & entanglement", Response.variable_category))

datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% "Bottom contact" & Ecosystem.component_level1 %in% "Plants",
                                                                   "Damage & entanglement", Response.variable_category))

RV_damage <- subset(datRespvar, Response.variable_category %in% "Damage & entanglement")


papers                                   <- datRespvar[datRespvar$Response.variable_category %in% "Other",]
sort(unique(papers$Response.variable_paper))
#CHECK: 'density' in 'Other'


### List all reported Response variables just to have a check
