#####################################################################################################################
#####################################################################################################################
#
#     Script to explore, process and clean data extraction files from SEAwise task 4.1
#     Step 2. Process and clean data
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
library(data.table)
library(stringr)


outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS"


################################################
#-----------------------------------------------
# Overview: treated, excluded, contributors
#
#  info:
#  This section provides a short overview of the dataset, including the number of papers processed, excluded and retained. 
#  It shows the number of papers per reviewer, and why the papers got excluded.
#
#-----------------------------------------------
tab                                   <- readRDS(paste0(outPath, "tab.rds"))
tab                                   <- as.data.table(tab)
tab$ROWID                             <- c(1:nrow(tab))
# write.csv(tab, paste0(outPath, "tab.csv"))

## check if all rows have a SW.ID
table(is.na(tab$SW.ID))

## Check if exclusion.criteria make sense
table(tab$Exclusion.Criteria)

## how many papers treated : 731 papers (out of 731)
length(unique(tab$SW.ID))

## Check who did how many papers
contributors                          <- tab[,.(Papers_read = length(unique(SW.ID))), by="Reader"]

## how many papers retained : 528 papers
retained                              <- unique(subset(tab, is.na(Exclusion.Criteria)==TRUE)$SW.ID)
length(retained)

## how many rejected : 203 papers
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
#rm(tab, contributors, retained, excluded)

## For easyness, skip the long-text columns.
## !!!! SKIP THIS STEP FOR PROCESSING THE DATA INCLUDING ALL COLUMNS !!!!
data                                  <- data[,c(1, 5, 19:29, 32:49, 51:52)]

## Check the regions (all fine)
table(is.na(data$Region))
table(data$Region)

tobechecked                           <- data[is.na(Region)==T,,]
data                                  <- data[is.na(Region)==F,,]

## Check the spatial scale and resolution, and correct for input mistakes
table(is.na(data$Scale...Spatial..m.)) # There are 22 NAs
table(data$Scale...Spatial..m.)
data$Scale...Spatial..m.              <- ifelse(data$Scale...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Scale...Spatial..m. == "100-501", "100-500", 
                                                       ifelse(data$Scale...Spatial..m. == "50-101", "50-100", data$Scale...Spatial..m.)))
table(is.na(data$Resolution...Spatial..m.)) # There are 106 NAs
table(data$Resolution...Spatial..m.)
data$Resolution...Spatial..m.         <- ifelse(data$Resolution...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Resolution...Spatial..m. == "50-101", "50-100", 
                                                       ifelse(data$Resolution...Spatial..m. == "5-11", "5-10", 
                                                              ifelse(data$Resolution...Spatial..m. == "100-501", "100-500", data$Resolution...Spatial..m.))))

## Check the temporal scale and resolution (all fine)
table(data$Scale...Temporal)
table(data$Resolution...Temporal)
table(is.na(data$Scale...Temporal)) # There are 17 NAs
table(is.na(data$Resolution...Temporal)) # There are 64 NAs

## Check the Response variable category
table(is.na(data$Response.variable_category)) #no NAs
table(data$Response.variable_category)

data$Response.variable_category       <- ifelse(data$Response.variable_category %in% c("abundance", "Abundance", "Abundance by taxon"), "Abundance/biomass/density",
                                                ifelse(data$Response.variable_category %in% c("other"), "Other",
                                                       ifelse(data$Response.variable_category == "Other_physical", "Physiology", data$Response.variable_category)))


## Check the Pressure types (level 1)
table(is.na(data$Pressure.type)) ## all fine 

## Check the Pressure types (level 2)
table(is.na(subset(data, Pressure.type == "Catch_and_bycatch")$Pressure_level)) # There are 115 NAs (for rows where non-NA was expected). These are set to "non specified".

table(data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure_level == "target", "Target", data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure.type == "Catch_and_bycatch" & is.na(data$Pressure_level), "Not specified", data$Pressure_level)


## Check the ecosystem component level 1
table(is.na(data$Ecosystem.component_level1)) #no NAs 

## Check the ecosystem component level 2
table(is.na(subset(data, Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous",
                                                     "Physical_habitats", "Plankton", "Plants", "Reptiles"))$Ecosystem.component_level2)==T) #324 NAs
table(data$Ecosystem.component_level2)


## Check Fishery types and Gear_level1
table(is.na(data$Fishery.type)) # all fine

table(data$Fishery.type)
data$Fishery.type                    <- ifelse(data$Fishery.type %in% "Artisanal", "Commercial", 
                                               ifelse(data$SW.ID == "SW4_1000", "Commercial", data$Fishery.type))

table(data$Gear_level1)
data$Gear_level1                     <- ifelse(data$Gear_level1 == "Demersal_trawls", "Demersal trawls",
                                               ifelse(data$Gear_level1 %in% c("Pelagic _trawls", "Pelagic_trawls"), "Pelagic trawls",
                                                      ifelse(data$Gear_level1 == "Mixed gears", NA,  data$Gear_level1)))

## Check for WP4 Task mentioning
table(is.na(data$WP4.task)) # all fine
table(data$WP4.task)
data$WP4.task                        <- ifelse(data$WP4.task == "none", "None", 
                                               ifelse(data$WP4.task == "4.3_4.4", "4.3 _ 4.4",
                                                      ifelse(data$WP4.task == "4.4000000000000004", "4.4", data$WP4.task)))

## Check what relationships have been reported
table(data$Direction.of.relationship, useNA = "always")
table(is.na(data$Direction.of.relationship)) # 4 NA's --> classify those as not specified
data$Direction.of.relationship       <- ifelse(data$Direction.of.relationship == "negative", "Negative", data$Direction.of.relationship)
data$Direction.of.relationship[is.na(data$Direction.of.relationship)] <- "Not specified"

## Check what species are commonly mentioned
length(unique(data$Species.taxonomic.group.s.)) # 465 unique input... Let's try to group/categorize these in a separate script (step 3)

## fix some rows with double input
a                                    <- data[Species.taxonomic.group.s. == "asteroids and lamp shells",,]
a$Species.taxonomic.group.s.         <- "Asteroidea"
b                                    <- a
b$Species.taxonomic.group.s.         <- "Brachiopoda"
b$ROWID                              <- max(data$ROWID)+1
data                                 <- data[!ROWID == a$ROWID,,]
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)


## Check what pressure variables are commonly mentioned
length(unique(data$Pressure_variable)) # 386 unique input... Let's skip for now.

## Check whether ECL2 contains only sediment information when ECL1 == Physical_habitat
a                                    <- data[Ecosystem.component_level2 %in% c("Gravel", "Mixed", "Mud", "Sand", "Unknown")]
table(a$Ecosystem.component_level1) ## All fine!

## Change missing sediment info when ECL1 == Physical_habitats to "Unknown"
data$Ecosystem.component_level2      <- ifelse(data$Ecosystem.component_level1 == "Physical_habitats" & is.na(data$Ecosystem.component_level2) == TRUE, "Unknown", data$Ecosystem.component_level2)

## Remove sediment information (Ecosystem.component_benthos_sediment) when Ecosystem component != "Benthos"
data$Ecosystem.component_benthos_sediment <- ifelse(data$Ecosystem.component_level1 == "Benthos" & is.na(data$Ecosystem.component_benthos_sediment) == TRUE, "Unknown", 
                                                    ifelse(data$Ecosystem.component_level1 == "Benthos", data$Ecosystem.component_benthos_sediment, NA))
data$Ecosystem.component_benthos_sediment <- ifelse(data$Ecosystem.component_benthos_sediment == "sand", "Sand", data$Ecosystem.component_benthos_sediment)

## Correct input in Sampling.Method.used.for.data.collection to pre-chosen classes
data$Sampling.Method.used.for.data.collection <- ifelse(data$Sampling.Method.used.for.data.collection %in% c("other", "Other - box corer"), "Other", data$Sampling.Method.used.for.data.collection)

## Correct input in Study.type to pre-chosen classes
data$Study.type                      <- ifelse(data$Study.type %in% c("combination of field surveys, byctach and over many decades"), "Other", 
                                               ifelse(data$Study.type == "Fisheries Dependent Data", "Fisheries dependent survey", data$Study.type))

## Correct input in study type according to decisions from script 5a
data$Study.type                      <- ifelse(data$SW.ID %in% c("SW4_0065", "SW4_1177") & data$Study.type == "Other", "Fisheries dependent survey",
                                               ifelse(data$SW.ID %in% c("SW4_0368", "SW4_0915", "SW4_0153", "SW4_0409", "SW4_0624") & data$Study.type == "Other", "Modelling/simulation",
                                               ifelse(data$SW.ID %in% c("SW4_0484", "SW4_0259", "SW4_0644", "SW4_0995", "SW4_1811", 
                                                                        "SW4_0468", "SW4_0703", "SW4_0022", "SW4_0186", "SW4_0154", "SW4_0883") & data$Study.type == "Other", "Field experiment",
                                                      ifelse(data$SW.ID %in% c("SW4_0199", "SW4_0330", "SW4_0565", "SW4_0693", "SW4_0738", "SW4_0772",
                                                                               "SW4_0934", "SW4_1294", "SW4_1527", "SW4_1788", "SW4_1354"), "Questionnaire", data$Study.type))))
data$Study.type                      <- ifelse(data$Study.type == "Field experiment", "Field experiment/observations", data$Study.type)
data$Study.type                      <- ifelse(data$Study.type == "Questionnaire", "Questionnaire/interview", data$Study.type)

## Correct some misspellings

### Pressure variable
data$Pressure_variable    <- with(data,ifelse(grepl("distrubance event compared to control", Pressure_variable), 
                                              "disturbance event compared to control", Pressure_variable))

data$Pressure_variable    <- with(data,ifelse(grepl("daily mechanical shaking simulated distrurbance", Pressure_variable), 
                                              "daily mechanical shaking simulated disturbance", Pressure_variable))

data$Pressure_variable    <- with(data,ifelse(grepl("Exposure to electical pulse", Pressure_variable), 
                                              "Exposure to electrical pulse", Pressure_variable))

#only to be run when 'data' is based on 'data_allScreened'
if(ncol(data) > 50){
  ## Remove some weird codes in Concluding remarks
  data$Concluding.statement.or.quotable.quote <- str_replace_all(data$Concluding.statement.or.quotable.quote, "_x0002_" , "")
  data$Concluding.statement.or.quotable.quote <- str_replace_all(data$Concluding.statement.or.quotable.quote, "~" , "i")
  
  ## And remove new lines
  data$Concluding.statement.or.quotable.quote <- str_replace_all(data$Concluding.statement.or.quotable.quote, "[\r\n]" , "")
  
  ## Remove quotation marks in Concluding statement and in Magnitude of relationship by Potier
  data$Concluding.statement.or.quotable.quote <- str_replace_all(data$Concluding.statement.or.quotable.quote, '"' , '')
  data$Magnitude.of.relationship[data$Reader %in% "Potier"] <- str_replace_all(data$Magnitude.of.relationship[data$Reader %in% "Potier"], '"' , '')
}




#-----------------------------------------------


################################################
#-----------------------------------------------
# Save the processed data file
#-----------------------------------------------
saveRDS(data, file=paste0(outPath, "data.rds"))

## Paste back excluded papers and save -> make sure you first undo dropping columns in data and removing tab and retained
## !!!! ONLY WHEN YOU ARE PROCESSING THE DATA INCLUDING ALL COLUMNS !!!!
data_allScreened                     <- rbind(data, tab[!SW.ID %in% retained,], fill=TRUE) #IMPORTANT NOTE: ROWID does not match between data & tab and data_allScreened!
saveRDS(data_allScreened, file=paste0(outPath, "data_allScreened.rds"))



################################################
#-----------------------------------------------
# Merge cleaned extracted data with screening results
#-----------------------------------------------

# Load cleaned extracted data (including papers that were excluded)
data_allScreened                         <- readRDS(paste0(outPath, "data_allScreened.rds"))

# Load screening results
screening.raw                            <- read.csv(paste0(outPath,"../Screening/screening coding all.csv"), stringsAsFactors = FALSE)

# Following needs to done:
# * Mark papers that were excluded during screening and under which exclusion criteria
# * Mark papers that were included during screening and under which inclusion label
# * Mark papers that were excluded during data extraction and under which exclusion criteria
# * Mark papers that remained including in data extraction and under which inclusion label (a few may have changed)

# Screening
## Subset to relevant columns
screening                                <- screening.raw[,c("SW_ID","Screening.Code","InclExcl")]
names(screening)                         <- c("SW.ID","Screening.Code","Screening.Fate")
screening$Screening.Exclusion.Code       <- with(screening, ifelse(Screening.Fate %in% "Excluded",Screening.Code,NA))

## Correct paper that got both inclusion and exclusion label
screening$Screening.Code[screening$SW.ID %in% "SW4_0647"] <- "INCLUDE on title and abstract"

## Create WP4 task labels
screening$task4.2                        <- with(screening.raw, ifelse(!is.na(INCLUDE.4.2.bycatch.PET.species),"4.2",NA))
screening$task4.3                        <- with(screening.raw, ifelse(!is.na(INCLUDE.4.3.benthic.habitats),"4.3",NA))
screening$task4.4                        <- with(screening.raw, ifelse(!is.na(INCLUDE.4.4.food.webs...diversity),"4.4",NA))
screening$task4.5                        <- with(screening.raw, ifelse(!is.na(INCLUDE.4.5.litter),"4.5",NA))
screening$task.none                      <- with(screening.raw, ifelse(!is.na(INCLUDE.on.title.and.abstract),"None",NA))

## Combine
screening$Screening.WP4.task             <- apply(screening[,c(5:9)], 1, function(x) paste(x[!is.na(x)], collapse = " _ "))
screening$Screening.WP4.task             <- with(screening, ifelse(Screening.WP4.task %in% "", NA, Screening.WP4.task))
screening$task4.2                        <- screening$task4.3 <- screening$task4.4 <- screening$task4.5 <- screening$task.none <- NULL


# Data extraction
## Subset to relevant columns
extraction                               <- data_allScreened[,c("SW.ID","Exclusion.Criteria","WP4.task")]
names(extraction)                        <- c("SW.ID","Extraction.Exclusion.Code","Extraction.WP4.task")  

## Clean and add some columns
extraction$Extraction.WP4.task           <- with(extraction, ifelse(!is.na(Extraction.Exclusion.Code), NA, Extraction.WP4.task))
SW.IDs                                   <- unique(extraction$SW.ID)
extraction.tasks                         <- data.frame(SW.ID = SW.IDs, Extraction.WP4.task = NA)
for(iID in 1:length(SW.IDs)){
  ss                                        <- subset(extraction, SW.ID %in% SW.IDs[iID])
  if(length(unique(ss$Extraction.WP4.task)) == 1){
    extraction.tasks$Extraction.WP4.task[iID] <- ss$Extraction.WP4.task
  } else{
    tasks                                     <- sort(unique(ss$Extraction.WP4.task))
    extraction.tasks$Extraction.WP4.task[iID] <- paste(tasks, collapse = " _ ")
  }
}
extraction$Extraction.WP4.task           <- NULL                                        
extraction                               <- merge(extraction, extraction.tasks, by="SW.ID")
extraction                               <- extraction[!duplicated(extraction),]
extraction$Extraction.Fate               <- with(extraction, ifelse(is.na(Extraction.Exclusion.Code), "Included", "Excluded"))


# Merge the two datasets
FatePapers                              <- merge(screening, extraction, by="SW.ID", all.x=TRUE)

# Check for duplicates and correct
FatePapers$SW.ID[which(duplicated(FatePapers$SW.ID))] #paper received two exclusion criteria during data extraction
FatePapers$Extraction.Exclusion.Code[FatePapers$SW.ID %in% "SW4_1550"] <- paste(FatePapers$Extraction.Exclusion.Code[FatePapers$SW.ID %in% "SW4_1550"], collapse = " _ ")

# Remove duplicated paper
FatePapers                              <- FatePapers[!duplicated(FatePapers$SW.ID),]

# Save
save(FatePapers, file=paste0(outPath, "FatePapers.RData"))
