#####################################################################################################################-
#####################################################################################################################-
#
#     Additional data processing of the data extraction files from SEAwise task 4.1
#     Step 5a. Check study types "Other"
#
#     By Karin van der Reijden
#     October 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#

library(data.table)
# library(RColorBrewer)
# library(raster)
# library(plotrix)
# library(sf)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 1.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data.rds"))


unique(data$Study.type)
# "Modelling/simulation"
# "Field experiment"
# "Fisheries independent survey"
# "Meta-analysis"               
# "Fisheries dependent survey" 
# "Other"                       
# "Lab experiment"
# NA         (3x)
 
#-----------------------------------------------#
# Investigate the "Others" in Study.type
#-----------------------------------------------#
dat                                   <- data[,c("Sampling.Method.used.for.data.collection",
                                                 "Description.Other.Sampling.Method",
                                                 "Study.type", 
                                                 "SW.ID",
                                                 "Abstract", 
                                                 "Title")]
Others                                <- dat[Study.type == "Other" | is.na(Study.type)==TRUE,,]
Others                                <- Others[!duplicated(Others),] 
## Note that only 22 papers actually have study type classified as "Other" or NA.
length(unique(Others$SW.ID))
## Yet, 'Others' has 28 rows, meaning some papers have multiple sampling methods mentioned / class or description.
table(Others$Sampling.Method.used.for.data.collection)
table(Others$SW.ID) # 4 papers with multiple appearances.

# ## Change study type from "Other" to "Fisheries dependent survey" if 'sampling method' states "fisheries-dependent data" (n=4)
# data$Study.type                       <- ifelse(data$Study.type == "Other" & 
#                                                   data$Sampling.Method.used.for.data.collection == "Fisheries Dependent Data", 
#                                                 "Fisheries dependent survey", data$Study.type)
# ## Change study type from "Other" to "Fisheries independent survey" if 'sampling method' states "fisheries-independent data" (n=9)
# data$Study.type                       <- ifelse(data$Study.type == "Other" & 
#                                                   data$Sampling.Method.used.for.data.collection %in% c("Irregular Fisheries Independent Survey",
#                                                                                                        "Regular Fisheries Independent Survey"), 
#                                                 "Fisheries independent survey", data$Study.type)

## Read abstracts of "Other" studies
unique(Others$Abstract)
# --> Questionnaires
#     Small-scale fisheries vs monk seals in the MedSea (SW4_0199)
#     MedSea resource perception by fishers (SW4_0738)
#     Assessment of turtle bycatch level in Valencia region (SW4_0693)
#     Assessment of turtle bycatch levels in Italy (SW4_0565)

# --> Fisheries dependent survey
#     Spanish purse-seine effects on ETP-species (and also more general impacts) (SW4_0065)
#     Overview of (Plaice) bycatch in German brown shrimp fisheries (SW4_1177)

# --> Modelling/Simulations
#     Overlap fishing activity with Hake nursery areas (SW4_0368)
#     Fishing mortality vs effort English beam trawl fleet (SW4_0915)
#     Effects pulse trawling (compared to beam trawl) based on VMS and RBS-model (SW4_0153)
#     Bottom trawling impact based on benthic community longevity (SW4_0409)
#     Fisheries impact in Kattegat area (SW4_0624)

# --> Observations 'in the field'
#     video observations of reef structure, related to observed trawl activity (SW4_0484)
#     Core-analysis of sedimentation (SW4_0259)
#     Observations of orcas in the field, interacting with bluefin tuna fishery (depredation long-lines) (SW4_0644)
#     Comparison of 'observations' of stomach content (Hake) between fished and unfished area (SW4_0995)
#     comparisons of in-field observations on Gull diet during two fisheries-regimes (SW4_1811)
#     Observation of abnormal specimen of Raja clavata due to non-lethal entrapment in fishing gear (SW4_0468)
#     Observations of harbour porpoise strandings, linked to fishing activity (SW4_0703)

# --> Field experiment
#     Clam abundance before and after MPA establishment (SW4_0022)
#     Examining bottom trawl impact using experimental trawling (SW4_0186)
#     Benthic invertebrate bycatch survival study (SW4_0154)
#     Benthic community structure after MPA establishment (SW4_0883)


