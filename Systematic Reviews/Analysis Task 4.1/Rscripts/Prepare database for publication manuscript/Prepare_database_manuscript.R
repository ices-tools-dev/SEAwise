#####################################################################################################################-
#####################################################################################################################-
#
#     Script to produce a final version of the database to be published alongside manuscript
#
#     This version includes all columns except those SEAwise specific or deemed irrelevant for publication.
#     It includes all papers for which data were extracted, i.e. all papers excluded based on the exclusion criteria
#     are not included.
#
#     Author(s): Esther Beukhof
#     November 2024
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths ----
#-----------------------------------------------#

library(openxlsx)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/Database/"


#-----------------------------------------------#
# Read in data ----
#
#  info:
#  This section depends on the processed data file produced under Rscripts/Data processing
# 
#-----------------------------------------------#

data_allScreened                      <- readRDS(file=paste0(datPath, "data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))



#-----------------------------------------------#
# Remove excluded papers ---- 
#-----------------------------------------------#

data                                  <- subset(data_allScreened, is.na(Exclusion.Criteria))



#-----------------------------------------------#
# Create new paper ID column ---- 
#-----------------------------------------------#

data$Paper.ID                         <- data$SW.ID
data                                  <- subset(data, select = -c(SearchID,SW.ID))



#-----------------------------------------------#
# Check bibliographical columns ---- 
#
# Remove irrelevant or incomplete columns where needed
#
#-----------------------------------------------#

# To do checks, make cropped version where one row is one paper
data.ID                               <- data[!duplicated(data$Paper.ID),]


### Link ----

# Check what information is in database
length(unique(data.ID$Link)) #only 27 links provided, so drop column
colRemove                             <- "Link"


### Open access ----

# Check what information is in database
table(data.ID$Open.Access, data.ID$Database)




#-----------------------------------------------#
# Adapt Region column ---- 
#-----------------------------------------------#
# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                           ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                  ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                         ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

data_allScreened$RegionSEAwise        <- data_allScreened$Region
data_allScreened$Region               <- with(data_allScreened, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                                       ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                              ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                                     ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))



#-----------------------------------------------#
# Adapt Region column ---- 
#-----------------------------------------------#
# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                           ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                  ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                         ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

data_allScreened$RegionSEAwise        <- data_allScreened$Region
data_allScreened$Region               <- with(data_allScreened, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                                       ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                              ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                                     ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))




###########################################################################################################################----
#-----------------------------------------------#
# Check papers that study pulse or rapido trawl ----
#-----------------------------------------------#

# Select papers on pulse trawl

## Title
paperIDs                               <- sort(unique(data_allScreened$SW.ID[which(grepl("pulse",data_allScreened$Title))]))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Pulse",data_allScreened$Title))])))

## Abstract
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("pulse",data_allScreened$Abstract))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Pulse",data_allScreened$Abstract))])))

## Target species/metier
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("pulse",data_allScreened$Target.species_metier))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Pulse",data_allScreened$Target.species_metier))])))

## Pressure reported
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("pulse",data_allScreened$Pressure_variable))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Pulse",data_allScreened$Pressure_variable))])))

# Add papers on rapido trawl

## Title
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("rapido",data_allScreened$Title))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Rapido",data_allScreened$Title))])))

## Abstract
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("rapido",data_allScreened$Abstract))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Rapido",data_allScreened$Abstract))])))

## Target species/metier
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("rapido",data_allScreened$Target.species_metier))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Rapido",data_allScreened$Target.species_metier))])))

## Pressure reported
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("rapido",data_allScreened$Pressure_variable))])))
paperIDs                               <- c(paperIDs,sort(unique(data_allScreened$SW.ID[which(grepl("Rapido",data_allScreened$Pressure_variable))])))

# Sort and get unique paper IDs
paperIDs                               <- sort(unique(paperIDs))
length(paperIDs) #28 papers
                                            