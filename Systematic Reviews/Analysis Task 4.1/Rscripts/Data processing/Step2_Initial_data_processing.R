#####################################################################################################################-
#####################################################################################################################-
#
#     Script to explore, process and clean data extraction files from SEAwise task 4.1
#     Step 2. Process and clean data
#
#     By Marie Savinar, adapted by Karin van der Reijden
#     March 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list=ls())

#-----------------------------------------------#
# Load libraries and set Paths ----
#-----------------------------------------------#
library(data.table)
library(stringr)
library(openxlsx)


outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS"


################################################-
#-----------------------------------------------#
# Overview: treated, excluded, contributors ----
#
#  info:
#  This section provides a short overview of the dataset, including the number of papers processed, excluded and retained. 
#  It shows the number of papers per reviewer, and why the papers got excluded.
#
#-----------------------------------------------#
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

## how many papers retained : 527 papers
retained                              <- unique(subset(tab, is.na(Exclusion.Criteria)==TRUE)$SW.ID)
length(retained)

## how many rejected : 204 papers
excluded                              <- unique(subset(tab, !is.na(Exclusion.Criteria) == TRUE)$SW.ID)
length(excluded)

## Why excluded?
table(tab[SW.ID %in% excluded, Exclusion.Criteria])

#-----------------------------------------------#




################################################-
#-----------------------------------------------#
# Processing: cleaning / reformatting etc. ----
#
#  info:
#  This section aims to clean the data of the retained papers.
#  It corrects for input-typos /inconsistencies and splits some double-entries over two lines.
#  Note: I removed some papers as their data extraction was incomplete. these are stored in the "tobechecked" file.
#-----------------------------------------------#
## Select the retained papers
data                                  <- tab[SW.ID %in% retained,,]
#rm(tab, contributors, retained, excluded)


## Should the full or cropped version of the dataset be produced?
## If full, nothing needs to be changed, if cropped, then columns are removed
if(dataset == "cropped"){
  data                                  <- data[,c(1, 5, 19:29, 32:49, 51:52)]
}


################################################-
## Bibliographic information ----
################################################-

## Check some bibliographic information - only for the full dataset
if(dataset == "full"){
  
  # Create dataset where each row is one paper
  data.ID   <- data[!duplicated(data$SW.ID),]
  
  # Authors
  table(!is.na(data.ID$Authors)) #no NAs
  
  # Title
  table(!is.na(data.ID$Title)) #no NAs
  
  # Year
  table(!is.na(data.ID$Year)) #no NAs
  
  # Journal title
  table(!is.na(data.ID$Source.title)) #no NAs
  
  # Volume number
  table(!is.na(data.ID$Volume)) #two NAs
  
  ## SW4_0111: missing because paper was accepted end of 2021 but first published in 2022 after literature search.
  #Keep as is, to reflect search results as they were at the time of searching.
  
  ## SW4_0368: missing volume number -> add
  data$Volume[data$SW.ID %in% "SW4_0368"]   <- 142
  
  # Issue
  table(!is.na(data.ID$Issue)) #182 NAs, but quite common for journals not to have issue number, only volume
  
  # Page numbers
  table(!is.na(data.ID$Page.start)) #85 NAs
  table(!is.na(data.ID$Page.end)) #85 NAs, but common for some journals to only have online version, e.g. PLoS ONE, Scientific Reports
 
  # DOI
  table(!is.na(data.ID$DOI)) #15 NAs
  
  # Add DOI when missing and DOI available
  data$DOI[data$SW.ID %in% "SW4_1297"]   <-"10.47536/jcrm.v10i2.647"
  data$DOI[data$SW.ID %in% "SW4_1826"]   <-"10.1007/BF00662185"
  
  # Link
  length(unique(data.ID$Link)) #only 27 links provided
  table(data.ID$Reader[!is.na(data.ID$Link)]) #by two reviewers
  #Note that a Scopus link was originally provided for all Scopus records, but not for WoS, so 
  #they were deleted for all, as this was not a link directly to the article - just to Scopus with information
  #about the article.
  
  # Abstract
  table(!is.na(data.ID$Abstract)) #no NAs
  
    # Language
  table(data.ID$Language, useNA = "always") #some are only Spanish so check those out
  
  # Spanish papers
  unique(data.ID$SW.ID[data.ID$Language %in% "Spanish"]) #Spanish abstract but all else English
  
  # Change language Spanish paper to English; Spanish
  data$Language[data$SW.ID %in% "SW4_1271"]   <- "English; Spanish"
 
  # Document type
  table(data.ID$Document.Type, useNA = "always")
  table(data.ID$Document.Type, data.ID$Database)
  
  # Check those that are Conference Papers
  data_conf   <- subset(data.ID, Document.Type %in% "Conference Paper")
  sort(data_conf$SW.ID)
  table(data_conf$Source.title, useNA = "always") #from variety of journals - seem 'normal' scientific journal articles
  
  # Check those that are reviews
  data_rev    <- subset(data.ID, Document.Type %in% "Review")
  sort(data_rev$SW.ID) #are either meta-analyses or original research articles
  
  # Open access by database
  table(data.ID$Open.Access, data.ID$Database, useNA = "always")
  #Seems to be NA when not open access according to the green/bronze/gold system
  data.OA     <- subset(data.ID, !is.na(Open.Access)) #variety of years and journals
  data.notOA  <- subset(data.ID, is.na(Open.Access)) #variety of years and journals

  # Database
  table(data.ID$Database, useNA = "always") #extra space bar for Web of Science -> correct
  data$Database[data$Database %in% "Web of Science "]   <- "Web of Science"
  #Number of papers from Web of Science is under-represented, as when they were occurring in both WoS and Scopus,
  #the record from Scopus was taken. Go through the original search results again, and add both databases where
  #applicable
  
  ## Load search results
  sco1 <- read.csv(file = "Systematic Reviews/Analysis Task 4.1/Search/Raw search results/E4_Scopus_20220110.csv", header = TRUE, encoding = "UTF-8")
  sco2 <- read.csv(file = "Systematic Reviews/Analysis Task 4.1/Search/Raw search results/E4_Scopus_20220204.csv", header = TRUE, encoding = "UTF-8")
  sco  <- rbind(sco1, sco2)
  wos  <- read.xlsx(xlsxFile = "Systematic Reviews/Analysis Task 4.1/Search/Raw search results/E4_WebOfScience_20220110.xlsx", sheet = 1)
  
  ## Reformat and simplify Scopus Record format
  colnames(sco)[colnames(sco) == "X.U.FEFF.Authors"] <- "Authors"
  colnames(sco)[colnames(sco) == "Language.of.Original.Document"] <- "Language"
  colnames(sco)[colnames(sco) == "Source"] <- "Database"
  sco[, c("Author.s..ID", "Art..No.", "Page.count", "Cited.by", "Publication.Stage", "EID", "Link")] <- NULL
 
  # Convert WoS record format to Scopus Record format (and remove extra fields)
  colnames(wos)[colnames(wos) == "Article.Title"] <- "Title"
  colnames(wos)[colnames(wos) == "Source.Title"] <- "Source.title"
  colnames(wos)[colnames(wos) == "Start.Page"] <- "Page.start"
  colnames(wos)[colnames(wos) == "End.Page"] <- "Page.end"
  colnames(wos)[colnames(wos) == "Open.Access.Designations"] <- "Open.Access"
  colnames(wos)[colnames(wos) == "Publication.Year"] <- "Year"
  
  wos$Database <- as.character(rep_len("Web of Science", nrow(wos)))
  
  wos <- wos[, c(colnames(wos)[colnames(wos) %in% colnames(sco)])]
  
  ## Merge all records and delete duplicates
  all <- rbind(sco, wos)
  
  ## Check DOI and occurrence in databases
  dat.occ <- as.data.frame(table(all$DOI,all$Database))
  dat.occ <- subset(dat.occ, !Var1 %in% "")
  dat.occ.l <- reshape2::dcast(dat.occ, Var1 ~ Var2)
  dat.occ.l$total <- dat.occ.l$Scopus + dat.occ.l$`Web of Science`
  
  ## Select DOIs which occur in both database and change in main file that they occur in both databases
  idx <- which(data$DOI %in% dat.occ.l$Var1[dat.occ.l$total == 2])
  data$Database[idx] <- "Scopus; Web of Science"
  
  # Create dataset where each row is one paper
  data.ID   <- data[!duplicated(data$SW.ID),]
  
  ## Check which papers miss information on database, extract information from original search results and
  ## and add to main datafile
  dat.miss <- subset(data.ID, is.na(Database))
  length(unique(dat.miss$SW.ID)) #7 papers
  idx <- which(dat.miss$DOI %in% sco$DOI)
  idx #all 7 in Scopus
  idx2 <- which(dat.miss$DOI %in% wos$DOI)
  idx2 #none in WoS
  data$Database[data$SW.ID %in% dat.miss$SW.ID] <- "Scopus"
  
  # Create dataset where each row is one paper
  data.ID   <- data[!duplicated(data$SW.ID),]
  
  rm(data_conf,data_rev,data.notOA,data.OA,sco,sco1,sco2,wos,all,dat.occ,dat.occ.l,dat.miss,data.ID,idx,idx2)
  
}


################################################-
## Common fields (region, scale etc.) ----
################################################-

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

## Check for WP4 Task mentioning
table(is.na(data$WP4.task)) # all fine
table(data$WP4.task)
data$WP4.task                        <- ifelse(data$WP4.task == "none", "None", 
                                               ifelse(data$WP4.task == "4.3_4.4", "4.3 _ 4.4",
                                                      ifelse(data$WP4.task == "4.4000000000000004", "4.4", data$WP4.task)))


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


################################################-
## Ecosystem component ----
################################################-

## Check the ecosystem component level 1
table(is.na(data$Ecosystem.component_level1)) #no NAs 

## Check the ecosystem component level 2
table(is.na(subset(data, Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous", #only for those with level 2 as option
                                                           "Physical_habitats", "Plankton", "Plants", "Reptiles"))$Ecosystem.component_level2)) #331 NAs
table(is.na(data$Ecosystem.component_level2)) #across all

# If level 2 not provided but option was given, mark as 'Not specified
table(data$Ecosystem.component_level1[data$Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous", #only for those with level 2 as option
                                                                             "Physical_habitats", "Plankton", "Plants", "Reptiles") &
                                        is.na(data$Ecosystem.component_level2)]) #mostly not provided for benthos, as well as fish and elasmobranchs
data$Ecosystem.component_level2[data$Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous",
                                                                       "Physical_habitats", "Plankton", "Plants", "Reptiles") &
                                  is.na(data$Ecosystem.component_level2)]   <- "Not specified"

# If ECL1 is physical habitats and ECL2 'Not specified', check whether this information was listed under Ecosystem.component_benthos_sediment and use that instead
papers <- sort(unique(data$SW.ID[data$Ecosystem.component_level1 %in% "Physical_habitats" & data$Ecosystem.component_level2 %in% "Not specified"])) #list all relevant papers
data[data$SW.ID %in% papers & data$Ecosystem.component_level1 %in% "Physical_habitats", c("Ecosystem.component_level2","Ecosystem.component_benthos_sediment")]
data$Ecosystem.component_level2[data$Ecosystem.component_level1 %in% "Physical_habitats" & data$Ecosystem.component_level2 %in% "Not specified" &
                                  !is.na(data$Ecosystem.component_benthos_sediment)] <-
  data$Ecosystem.component_benthos_sediment[data$Ecosystem.component_level1 %in% "Physical_habitats" & data$Ecosystem.component_level2 %in% "Not specified" &
                                    !is.na(data$Ecosystem.component_benthos_sediment)]

# Go through papers and add sediment type as ECL2 for physical habitats manually for remaining 'Not specified' papers
papers <- sort(unique(data$SW.ID[data$Ecosystem.component_level1 %in% "Physical_habitats" & data$Ecosystem.component_level2 %in% "Not specified"])) #list all relevant papers again

### SW4_0361
# Sediment is mostly consisting of silt, followed by clay, which both contain of smaller particles than sand. This was not an option in the data extraction form,
# so add manually here. Choose silt as this was most dominant
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0361" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Silt"

### SW4_0368
# Study area included multiple habitat types
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0368" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Mixed"

### SW4_0424
# Sediment is mostly consisting of silt, followed by clay, which both contain of smaller particles than sand. This was not an option in the data extraction form,
# so add manually here. Choose silt as this was most dominant
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0424" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Silt"

### SW4_0440
# Sediment is mostly consisting of silt, followed by clay, which both contain of smaller particles than sand. This was not an option in the data extraction form,
# so add manually here. Choose silt as this was most dominant
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0440" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Silt"

### SW4_0562
# Study area included multiple habitat types
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0562" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Mixed"

### SW4_0622
# Study area included multiple habitat types, and 'Mixed' is also the sediment type assigned to benthos in the same paper
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0622" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Mixed"

### SW4_1304
# Study area included multiple habitat types
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_1304" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Mixed"

### SW4_1832
# Study area included multiple habitat types, and 'Mixed' is also the sediment type assigned to benthos in the same paper
data$Ecosystem.component_level2[data$SW.ID %in% "SW4_1832" & data$Ecosystem.component_level1 %in% "Physical_habitats"]   <- "Mixed"


## Check whether ECL2 contains only sediment information when ECL1 == Physical_habitat
a                                    <- data[Ecosystem.component_level2 %in% c("Gravel", "Mixed", "Mud", "Sand", "Unknown","Silt")]
table(a$Ecosystem.component_level1) ## All fine!

## Check when both Benthos and Physical habitats were studied whether the same type of sediment was chosen

# Which papers study both Benthos and Physical habitats?
datSed                               <- subset(data, Ecosystem.component_level1 %in% c("Benthos","Physical_habitats"))
papers                               <- unique(datSed$SW.ID)
papSed                               <- data.frame(SW.ID = papers, benthosStudied = NA, physicalHabStudied = NA)
for(iPap in 1:length(papers)){
  print(iPap)
  # Select paper
  subDat   <- subset(datSed, SW.ID %in% papers[iPap])
  
  # Check whether Benthos was studied
  if("Benthos" %in% subDat$Ecosystem.component_level1){
    papSed$benthosStudied[iPap] <- "Yes"
  }
  
  # Check whether Physical habitat was studied
  if("Physical_habitats" %in% subDat$Ecosystem.component_level1){
    papSed$physicalHabStudied[iPap] <- "Yes"
  }
}

# Check whether the sediment type was listed - if not, check and change
papSedBoth                          <- subset(papSed, benthosStudied %in% "Yes" & physicalHabStudied %in% "Yes")     
papSedBoth$sedBenthos <- papSedBoth$sedPhysicalHab <- NA
papersBoth                          <- sort(papSedBoth$SW.ID)
for(iPap in 1:length(papersBoth)){
  
  # Select paper
  subDat   <- subset(datSed, SW.ID %in% papersBoth[iPap])
  
  # Check sediment benthos
  papSedBoth$sedBenthos    <- unique(subDat$Ecosystem.component_benthos_sediment[subDat$Ecosystem.component_level1 %in% "Benthos"])
  
  # Check sediment physical habitats
  papSedBoth$sedPhysicalHab    <- unique(subDat$Ecosystem.component_level2[subDat$Ecosystem.component_level1 %in% "Physical_habitats"])
}

# Are the same sediment types chosen?
identical(papSedBoth$sedBenthos, papSedBoth$sedPhysicalHab) #yes, so no further action needed

## Check ecosystem component level 3 - only for benthos and marine mammals
table(is.na(subset(data, Ecosystem.component_level1 %in% c("Benthos","Marine_mammals"))$Ecosystem.component_level3)) #395 NAs
table(is.na(data$Ecosystem.component_level3)) #across all

# If level 3 not provided for Benthos or Marine mammals, mark as 'Not specified
table(data$Ecosystem.component_level1[data$Ecosystem.component_level1 %in% c("Benthos", "Marine_mammals") & is.na(data$Ecosystem.component_level3)]) #mostly not provided for benthos
data$Ecosystem.component_level3[data$Ecosystem.component_level1 %in% c("Benthos", "Marine_mammals") & is.na(data$Ecosystem.component_level3)]   <- "Not specified"

# Check what species/taxonomic groups are provided when level 3 wasn't specified
sort(unique(data$Species.taxonomic.group.s.[data$Ecosystem.component_level3 %in% "Not specified"])) #leave for now - this column will be cleaned later in step 3

## Check sediment type when ecosystem level 1 is benthos
table(is.na(data$Ecosystem.component_benthos_sediment)) #across all
table(is.na(data$Ecosystem.component_benthos_sediment[data$Ecosystem.component_level1 %in% "Benthos"])) #across benthos - 157 NAs

# Check papers that have NA for benthos sediment
subDat                               <- data[data$Ecosystem.component_level1 %in% "Benthos" & is.na(Ecosystem.component_benthos_sediment),]
length(unique(subDat$SW.ID)) #57 papers

# Check whether we could borrow information if Physical habitats were studied in the same paper - if yes, assign that as benthos sediment
papers                               <- sort(unique(subDat$SW.ID))
subDat2                              <- subset(data, SW.ID %in% papers)
subDat2[subDat2$Ecosystem.component_level1 %in% "Physical_habitats",c("SW.ID","Ecosystem.component_level1","Ecosystem.component_level2")]

data$Ecosystem.component_benthos_sediment[data$SW.ID %in% "SW4_1209" & data$Ecosystem.component_level1 %in% "Benthos"] <-
  data$Ecosystem.component_level2[data$SW.ID %in% "SW4_1209" & data$Ecosystem.component_level1 %in% "Physical_habitats"]

data$Ecosystem.component_benthos_sediment[data$SW.ID %in% "SW4_0562" & data$Ecosystem.component_level1 %in% "Benthos"] <-
  data$Ecosystem.component_level2[data$SW.ID %in% "SW4_0562" & data$Ecosystem.component_level1 %in% "Physical_habitats"]

# Assign all remaining benthos sediment that are NA as 'Not specified
data$Ecosystem.component_benthos_sediment[is.na(data$Ecosystem.component_benthos_sediment) & data$Ecosystem.component_level1 %in% "Benthos"] <- "Not specified"
  
# Remove all benthos sediment types when ECL is anything other than benthos
length(unique(data$SW.ID[!data$Ecosystem.component_level1 %in% "Benthos" & !is.na(data$Ecosystem.component_benthos_sediment)])) #54 papers for which information was filled
table(data$Ecosystem.component_level1[!data$Ecosystem.component_level1 %in% "Benthos" & !is.na(data$Ecosystem.component_benthos_sediment)]) #provided for variety of other components
table(data$Ecosystem.component_benthos_sediment[!data$Ecosystem.component_level1 %in% "Benthos" & !is.na(data$Ecosystem.component_benthos_sediment)]) #mostly unknown
data$Ecosystem.component_benthos_sediment[!data$Ecosystem.component_level1 %in% "Benthos"]   <- NA

# Final check on benthos sediment
table(data$Ecosystem.component_benthos_sediment, useNA = "always") #correct spelling mistake
data$Ecosystem.component_benthos_sediment <- ifelse(data$Ecosystem.component_benthos_sediment == "sand", "Sand", data$Ecosystem.component_benthos_sediment)

## Check what species are commonly mentioned
length(unique(data$Species.taxonomic.group.s.)) # 469 unique input... Let's try to group/categorize these in a separate script (step 3)

## fix some rows with double input
a                                    <- data[Species.taxonomic.group.s. == "asteroids and lamp shells",,]
a$Species.taxonomic.group.s.         <- "Asteroidea"
b                                    <- a
b$Species.taxonomic.group.s.         <- "Brachiopoda"
b$ROWID                              <- max(data$ROWID)+1
data                                 <- data[!ROWID == a$ROWID,,]
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)


################################################-
## Pressure ----
################################################-

## Check the Pressure types (level 1)
table(data$Pressure.type, useNA = "always") ## all fine 

## Check the Pressure types (level 2) - only applicable to Catch_and_bycatch
table(is.na(subset(data, Pressure.type == "Catch_and_bycatch")$Pressure_level)) # There are 116 NAs (for rows where non-NA was expected). These are set to "not specified".
length(unique(data$SW.ID[data$Pressure.type %in% "Catch_and_bycatch" & is.na(data$Pressure_level)])) #corresponding to 32 papers

table(data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure_level == "target", "Target", data$Pressure_level) #correct lowercase
data$Pressure_level                  <- ifelse(data$Pressure.type == "Catch_and_bycatch" & is.na(data$Pressure_level), "Not specified", data$Pressure_level)

## Check reported pressure variable
table(is.na(data$Pressure_variable)) #no NAs
length(unique(data$Pressure_variable)) # 387 unique input... Let's skip for now and process further in step 4

# Only correct some misspellings
data$Pressure_variable    <- with(data,ifelse(grepl("distrubance event compared to control", Pressure_variable), 
                                              "disturbance event compared to control", Pressure_variable))

data$Pressure_variable    <- with(data,ifelse(grepl("daily mechanical shaking simulated distrurbance", Pressure_variable), 
                                              "daily mechanical shaking simulated disturbance", Pressure_variable))

data$Pressure_variable    <- with(data,ifelse(grepl("Exposure to electical pulse", Pressure_variable), 
                                              "Exposure to electrical pulse", Pressure_variable))

## Check other pressures
table(is.na(data$Pressure_other)) #1208 NAs, but this is fine - only when pressures other than fishing were studied
sort(unique(data$Pressure_other))


################################################-
## Gear ----
################################################-

## Check Fishery types and Gear_level1
table(data$Fishery.type, useNA = "always") # all fine, but Artisanal was not an option in the data extraction -> change to Commercial
data$Fishery.type                    <- ifelse(data$Fishery.type %in% "Artisanal", "Commercial", 
                                               ifelse(data$SW.ID == "SW4_1000", "Commercial", data$Fishery.type))

table(data$Gear_level1, useNA = "always") #221 NAs, some misspellings
data$Gear_level1                     <- ifelse(data$Gear_level1 == "Demersal_trawls", "Demersal trawls",
                                               ifelse(data$Gear_level1 %in% c("Pelagic _trawls", "Pelagic_trawls"), "Pelagic trawls",
                                                      ifelse(data$Gear_level1 == "Mixed gears", NA,  data$Gear_level1)))
# Check how many papers with NA for gear level 1
length(unique(data$SW.ID[is.na(data$Gear_level1)])) #77 papers, likely because gear was unknown or record represents multiple gear types

## Check Gear_level2
table(data$Gear_level2, useNA = "always") #all fine


################################################-
## Response, direction of relationships, magnitude ----
################################################-

## Check the Response variable category
table(is.na(data$Response.variable_category)) #no NAs
table(data$Response.variable_category) #soem misspellings

data$Response.variable_category       <- ifelse(data$Response.variable_category %in% c("abundance", "Abundance", "Abundance by taxon"), "Abundance/biomass/density",
                                                ifelse(data$Response.variable_category %in% c("other"), "Other",
                                                       ifelse(data$Response.variable_category == "Other_physical", "Physiology", data$Response.variable_category)))
## Check what relationships have been reported
table(data$Direction.of.relationship, useNA = "always")
table(is.na(data$Direction.of.relationship)) # 4 NA's --> classify those as not specified
data$Direction.of.relationship       <- ifelse(data$Direction.of.relationship == "negative", "Negative", data$Direction.of.relationship)
data$Direction.of.relationship[is.na(data$Direction.of.relationship)] <- "Not specified"


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


#-----------------------------------------------#


################################################-
#-----------------------------------------------#
# Save the processed data file ----
#-----------------------------------------------#
if(dataset == "cropped"){
  saveRDS(data, file=paste0(outPath, "data.rds"))
}
if(dataset == "full"){
  data_allScreened                     <- rbind(data, tab[!SW.ID %in% retained,], fill=TRUE) 
  saveRDS(data_allScreened, file=paste0(outPath, "data_allScreened.rds"))
}
#IMPORTANT NOTE: ROWID does not match between data & tab and data_allScreened!



################################################-
#-----------------------------------------------#
# Merge cleaned extracted data with screening results ----
#-----------------------------------------------#

# Do this only when full dataset is used
if(dataset == "full"){
  
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
      extraction.tasks$Extraction.WP4.task[iID] <- unique(ss$Extraction.WP4.task)
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
}
