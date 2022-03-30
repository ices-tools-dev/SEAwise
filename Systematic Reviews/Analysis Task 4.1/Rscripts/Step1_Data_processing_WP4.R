#####################################################################################################################
#####################################################################################################################
#
#     Script to read, merge, and analyse data extraction files from SEAwise task 4.1
#     Step 1. Read, merge and process the data extraction files.
#
#     By Marie Savinar, adapted by Karin van der Reijden
#     March 2022
#
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

#-----------------------------------------------
# Load libraries and set Paths.
#-----------------------------------------------
library(xlsx) 
library(data.table)
library(RColorBrewer)
library(raster)
library(plotrix)
library(sf)
library(viridis)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Data_Extraction_Files/" 
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS"

################################################
#-----------------------------------------------
# Read and merge data files
#
#  info:
#  This section reads in and merges the data extraction files. 
#  The most recent versions should be downloaded from the SEAwise sharepoint before starting this analysis/exercise.
#  Some files may require manual processing to ensure correct reading, however, these should be corrected at the sharepoint.
#  Note: I took the "editable" version of the Uhlmann_Reid file (and changed the name of the file)
#  Note: I manually switched rows 1 and 2 in the Binch-file, otherwise the file is not read correctly.
#  Note: I manually removed the ".xlsx" from the filename for Potier.
#  Note: I manually restored some of the column-names in the file for Thorpe.
#-----------------------------------------------
# readers                                <- c("Altuna-Etxabe", "Anastasopoulou", "Astarloa", "Basurko", "Binch", "Bluemel", "Brown",
#                                             "Carbonara", "Festjens", "garcia", "Girardin", "Halouani", "Lefkaditou_Chatzispyrou", "MacMillan", "Papadopoulou", "Potier",
#                                             "Romagnoni", "Seghers", "Smith", "Spedicato", "Thorpe", "Thuenen","Tsagarakis", "Uhlmann_Reid", "VanHoey", "vdReijden")
# #Missing: Dinesen.
# 
# ## Create a table from the first reader
# people                                 <- readers[1]
# tab                                    <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1)
# colnames(tab)
# tab$Reader                             <- people
# 
# ## Add all other readers
# for(people in readers[2:length(readers)]) {
#   print(people)
#   tab1                                 <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1)
#   tab1$Reader                          <- people
#   tab2                                 <- tab1[,colnames(tab1) %in% colnames(tab)] # remove empty / additional columns
#   tab2                                 <- subset(tab2, !is.na(tab2$SW.ID)==T) # remove empty rows
#   tab                                  <- rbind(tab,tab2)
# } # end people-loop
# 
# # Save as rds-file
# saveRDS(tab, paste0(outPath, "tab.rds"))
# rm(tab1, tab2, people, readers)



#-----------------------------------------------




################################################
#-----------------------------------------------
# Overview: treated, excluded, contributors
#
#  info:
#  This section provides a short overview of the dataset, including the number of papers processed, excluded and retained. 
#  It shows the number of papers per reviewer, and why the papers got excluded.
#
#  Comments:
#  Thorpe should check the exclusion criteria used. For now, I made a work-around method.
#
#-----------------------------------------------
tab                                   <- readRDS(paste0(outPath, "tab.rds"))
tab                                   <- as.data.table(tab)

## check if all rows have a SW.ID
table(is.na(tab$SW.ID))

## Check if exclusion.criteria make sense
table(tab$Exclusion.Criteria)
# Thorpe has incorrect exclusion criteria: this should be fixed in the data extraction file, but will do it here as well for now.
tab$Exclusion.Criteria                <- ifelse(tab$Reader == "Thorpe" & tab$Exclusion.Criteria %in% c("4.4", "4.3", "4.3 _ 4.4", "None"), NA, 
                                                ifelse(tab$Exclusion.Criteria == "Evidence", "EXCLUDE on evidence", tab$Exclusion.Criteria))

## how many papers treated : 721 papers (out of XXX?)
length(unique(tab$SW.ID))

## Check who did how many papers
contributors                          <- tab[,.(Papers_read = length(unique(SW.ID))), by="Reader"]

## how many papers retained : 541 papers
retained                              <- unique(subset(tab, is.na(Exclusion.Criteria)==TRUE)$SW.ID)
length(retained)

## how many rejected : 180 papers
excluded                              <- unique(subset(tab, !is.na(Exclusion.Criteria) == TRUE)$SW.ID)
length(excluded)

## Why excluded?
table(tab[SW.ID %in% excluded, Exclusion.Criteria])

#-----------------------------------------------




################################################
#-----------------------------------------------
# Processing: cleaning / reformatting etc
#
#  info:
#  This section aims to clean the data of the retained papers.
#  It corrects for input-typos /inconsistencies and splits some double-entries over two lines.
#  Note: I removed some papers as their data extraction was incomplete. these are stored in the "tobechecked" file.
#-----------------------------------------------
## Select the retained papers
data                                  <- tab[SW.ID %in% retained,,]
rm(tab, contributors, retained, excluded)

## For easyness, skip the long-text columns.
data                                  <- data[,c(1, 5, 19:29, 32:49, 51)]

## Check the regions (all fine)
table(is.na(data$Region)) # 1 NA
table(data$Region)

tobechecked                           <- data[is.na(Region)==T,,]
data                                  <- data[is.na(Region)==F,,]

## Check the spatial scale and resolution, and correct for input mistakes
table(is.na(data$Scale...Spatial..m.)) # There are 14 NAs
table(data$Scale...Spatial..m.)
data$Scale...Spatial..m.              <- ifelse(data$Scale...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Scale...Spatial..m. == "100-501", "100-500", 
                                                       ifelse(data$Scale...Spatial..m. == "50-101", "50-100", data$Scale...Spatial..m.)))
table(is.na(data$Resolution...Spatial..m.)) # There are 80 NAs
table(data$Resolution...Spatial..m.)
data$Resolution...Spatial..m.         <- ifelse(data$Resolution...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Resolution...Spatial..m. == "50-101", "50-100", 
                                                       ifelse(data$Resolution...Spatial..m. == "5-11", "5-10", 
                                                              ifelse(data$Resolution...Spatial..m. == "100-501", "100-500", data$Resolution...Spatial..m.))))

## Check the temporal scale and resolution (all fine)
table(data$Scale...Temporal)
table(data$Resolution...Temporal)
table(is.na(data$Scale...Temporal)) # There are 13 NAs
table(is.na(data$Resolution...Temporal)) # There are 43 NAs

## Check the Response variable category
table(is.na(data$Response.variable_category)) # There are 9 NA's. 
## 2 are described as "spatial distribution changes", which are classified to Abundance/biomass/density. The remaining NAs are classified as Other.
data$Response.variable_category       <- ifelse(data$Response.variable_paper == "spatial distribution changes" & data$SW.ID == "SW4_1094", "Abundance/biomass/density", data$Response.variable_category)
data$Response.variable_category[is.na(data$Response.variable_category)==TRUE] <- "Other"

table(data$Response.variable_category)
data$Response.variable_category       <- ifelse(data$Response.variable_category %in% c("abundance", "Abundance", "Abundance by taxon"), "Abundance/biomass/density",
                                                ifelse(data$Response.variable_category %in% c("other"), "Other",
                                                       ifelse(data$Response.variable_category == "Other_physical", "Physiology", data$Response.variable_category)))


## Check the Pressure types (level 1)
table(is.na(data$Pressure.type)) ## 2 NAs 
# Check SW4_1285 --> incomplete info, remove this row for now (paper has 2 more rows with complete info)
tobechecked                           <- rbind(tobechecked, data[(SW.ID == "SW4_1285" & is.na(Pressure.type)==TRUE)])
data                                  <- data[!(data$SW.ID == "SW4_1285" & is.na(data$Pressure.type) == TRUE),]
# Check SW4_1478 --> set to "Discarding", as that is what is affecting the birds.
data$Pressure.type                    <- ifelse(data$SW.ID == "SW4_1478" & is.na(data$Pressure.type) == T, "Discarding", data$Pressure.type)
table(data$Pressure.type)

## Check the Pressure types (level 2)
table(is.na(subset(data, Pressure.type == "Catch_and_bycatch")$Pressure_level)) # There are 114 NAs (for rows where non-NA was expected). These are set to "non specified".

table(data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure_level == "target", "Target", data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure.type == "Catch_and_bycatch" & is.na(data$Pressure_level), "Not specified", data$Pressure_level)
# 'Fix' the input of paper SW4_1621, that states both target and non-target (in data)
b                                    <- subset(data, SW.ID == "SW4_1621")
b$Pressure_level                     <- "Target"
a                                    <- b
a$Pressure_level                     <- "Non-target"
data                                 <- subset(data, !SW.ID == "SW4_1621")
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)

## Check the ecosystem component level 1
table(is.na(data$Ecosystem.component_level1)) # 2 NAs. 
# THey dominantly focus on fish_teleost and Nephrops (SW.ID SW4_0479)
# 'Fix' the input of paper SW4_0479, that should refer to both Nephrops and fish_teleost flatfish
b                                    <- data[(SW.ID == "SW4_0479" & is.na(data$Ecosystem.component_level1)==T),]
b$Ecosystem.component_level1         <- "Fish_teleost"
b$Ecosystem.component_level2         <- "Flatfish"
a                                    <- b
a$Ecosystem.component_level1         <- "Benthos"
a$Ecosystem.component_level2         <- "Benthic_epifauna"
data                                 <- data[!(data$SW.ID == "SW4_0479"& is.na(data$Ecosystem.component_level1)==T),]
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)

## Check the ecosystem component level 2
table(is.na(subset(data, Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous",
                                                     "Physical_habitats", "Plankton", "Plants", "Reptiles"))$Ecosystem.component_level2)==T) # 283 NA's where there should be none.
table(data$Ecosystem.component_level2)
# 'Fix' the input of paper SW4_0746, that states both benthic epifauna and infauna
b                                    <- subset(data, SW.ID == "SW4_0746")
b$Ecosystem.component_level2         <- "Benthic_epifauna"
a                                    <- b
a$Ecosystem.component_level2         <- "Benthic_infauna"
data                                 <- subset(data, !SW.ID == "SW4_0746")
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)

## Check Fishery types and Gear_level1
table(is.na(data$Fishery.type)) # all fine
table(is.na(data$Gear_level1))  # 195 NAs
table(data$Fishery.type)
data$Fishery.type                    <- ifelse(data$Fishery.type %in% c("commercial", "Artisanal"), "Commercial", 
                                               ifelse(data$SW.ID == "SW4_1000", "Commercial", data$Fishery.type))
table(data$Gear_level1)
data$Gear_level1                     <- ifelse(data$Gear_level1 == "Demersal_trawls", "Demersal trawls",
                                               ifelse(data$Gear_level1 %in% c("Pelagic _trawls", "Pelagic_trawls"), "Pelagic trawls",
                                                      ifelse(data$Gear_level1 == "Mixed gears", NA,  data$Gear_level1)))

## Check for WP4 Task mentioning
table(is.na(data$WP4.task)) # 1 NA
table(data$WP4.task)
data$WP4.task                        <- ifelse(data$WP4.task == "none", "None", 
                                               ifelse(data$WP4.task == "4.3_4.4", "4.3 _ 4.4", data$WP4.task))
## Look up some strange/missing tasks in the data extraction template
b                                    <- data[WP4.task %in% c("4.1", "Mediterranean - non CS", "4.6", "4.7", NA)]
unique(b$SW.ID) # "SW4_1991" (4.2), "SW4_1090" (NONE), "SW4_1216" (4.4), "SW_0154" (NONE)
data$WP4.task                        <- ifelse(data$SW.ID == "SW4_1991", "4.2",
                                               ifelse(data$SW.ID %in% c("SW4_1090", "SW4_0154"), "None",
                                                      ifelse(data$SW.ID == "SW4_1216", "4.4", data$WP4.task)))

## Check what relationships have been reported
table(data$Direction.of.relationship)
table(is.na(data$Direction.of.relationship)) # 41 NA's --> classify those as not specified
data$Direction.of.relationship       <- ifelse(data$Direction.of.relationship == "negative", "Negative", data$Direction.of.relationship)
data$Direction.of.relationship[is.na(data$Direction.of.relationship)] <- "Not specified"

## Check what species are commonly mentioned
length(unique(data$Species.taxonomic.group.s.)) # 453 unique input... let's skip for now.

## Check what pressure variables are commonly mentioned
length(unique(data$Pressure_variable)) # 380 unique input... Let's skip for now.


#-----------------------------------------------


################################################
#-----------------------------------------------
# Save the processed data file
#-----------------------------------------------
saveRDS(data, file=paste0(outPath, "data.rds"))



