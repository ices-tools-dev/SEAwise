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

# If cropped dataset should be used, do nothing.
# If full dataset should be used, select this:
if(dataset == "full"){
  data                                  <- data_allScreened
}

# # Remove some columns to improve viewing the data frame
# data_allScreened$SearchID <- data_allScreened$Open.Access <- NULL


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

rm(biodiv,comcom,datDeDupl)


#-----------------------------------------------#
# Check 'Other' category ----
#-----------------------------------------------#

# Create subset of data classified as Other
other      <- subset(datRespvar, Response.variable_category %in% 'Other')
length(unique(other$SW.ID)) #61 papers

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

### NEW: Sediment, seabed & physical properties ----
sort(unique(datRespvar[which(grepl("sedim", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Sedim", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("sedim", datRespvar$Response.variable_paper) & Response.variable_category %in% "Other",
                                                                     "Sediment & physical properties", Response.variable_category))
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("Sedim", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("grain", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all (one already categorized)
sort(unique(datRespvar[which(grepl("Grain", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all (only one)
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% c("grain structure",
                                                                                                  "median grain size (Q50)",
                                                                                                  "Grain size") & Response.variable_category %in% "Other",
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("organic", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Organic", datRespvar$Response.variable_paper)),]$Response.variable_paper)) #empty
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

sort(unique(datRespvar[which(grepl("susp", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all but one
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("susp", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other",
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("seafloor", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
sort(unique(datRespvar[which(grepl("Seafloor", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all (only one)
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("seafloor", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))
datRespvar$Response.variable_category    <- with(datRespvar,ifelse(grepl("Seafloor", datRespvar$Response.variable_paper),
                                                                   "Sediment & physical properties", Response.variable_category))

sort(unique(datRespvar[which(grepl("Area", datRespvar$Response.variable_paper) & datRespvar$Response.variable_category %in% "Other"),]$Response.variable_paper)) #all
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
                                                                                                  "Various types of seabed substrate disturbance",
                                                                                                  "effort displacement"),
                                                                   "Sediment & physical properties", Response.variable_category))

datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% "Bottom contact" & Ecosystem.component_level1 %in% "Physical_habitats",
                                                                   "Sediment & physical properties", Response.variable_category))

RV_sediment <- subset(datRespvar, Response.variable_category %in% "Sediment & physical properties")


# Now check whether some papers in this category have Benthos as ECL, and whether this is appropriate
notPhysHabPapers           <- subset(RV_sediment, !Ecosystem.component_level1 %in% "Physical_habitats")
##Have gone through them and made changes in script and/or in data extraction files


### NEW: Damage & entanglement ----

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
                                                                                                  "Frequency of deep-hooking",
                                                                                                  "Covering"),
                                                                   "Damage & entanglement", Response.variable_category))

datRespvar$Response.variable_category    <- with(datRespvar,ifelse(Response.variable_paper %in% "Bottom contact" & Ecosystem.component_level1 %in% "Plants",
                                                                   "Damage & entanglement", Response.variable_category))

RV_damage <- subset(datRespvar, Response.variable_category %in% "Damage & entanglement")



### Adding to/changing existing categories ----

# Papers on recovery rate
datRespvar$Response.variable_category <- with(datRespvar, ifelse(Response.variable_paper %in% c("Fished seabed habitat recovery as a function of time between disturbance",
                                                                                                "Habitat-specific recoverability estimate"),
                                                                 "Growth", Response.variable_category)) #SW4_1162, SW4_1386

# Paper on injury
datRespvar$Response.variable_category <- with(datRespvar, ifelse(Response.variable_paper %in% "Injury", "Damage & entanglement", Response.variable_category)) #SW4_0115

# Papers on Mixed Trophic Impact: make sure they are marked as Trophic structure
datRespvar$Response.variable_category[datRespvar$Response.variable_paper %in% c("Mixed Trophic Impact","mixed trophic impact","Mixed trophic impact")] <- "Trophic structure"

# Paper on trawl mark density
datRespvar$Response.variable_category[datRespvar$Response.variable_paper %in% "Trawl mark density"] <- "Sediment & physical properties" #SW4_0442

# Papers having biodiversity metric as response variable but not categorized as Biodiversity
papersBioDiv    <- subset(datRespvar, grepl("diversity", datRespvar$Response.variable_paper) & !Response.variable_category %in% "Biodiversity")
papersBioDiv    <- rbind(papersBioDiv, subset(datRespvar, grepl("richness", datRespvar$Response.variable_paper) & !Response.variable_category %in% "Biodiversity"))
papersBioDiv    <- papersBioDiv[!duplicated(papersBioDiv),]

# Paper reporting trait-based community metric of benthos categorized as 'Other' -> assign to Community composition
datRespvar$Response.variable_category <- with(datRespvar, ifelse(Response.variable_paper %in% c("Recoverability after physical disturbance",
                                                                                                "Sensitivity to physical disturbance",
                                                                                                "Vulnerability to physical disturbance"),
                                                                 "Community composition", Response.variable_category)) #SW4_1960

## For the following papers, replace category by Biodiversity
datRespvar$Response.variable_category[datRespvar$SW.ID %in% c("SW4_0955","SW4_0537","SW4_1826","SW4_0311","SW4_0968","SW4_1448","SW4_1826") & 
                                        (grepl("diversity", datRespvar$Response.variable_paper) | grepl("richness", datRespvar$Response.variable_paper))] <- "Biodiversity"
datRespvar$Response.variable_category[datRespvar$SW.ID %in% "SW4_0208" & 
                                        datRespvar$Response.variable_paper %in% "Macrofaunal indicators (inc.functional richness, functional dispersion)"] <- "Biodiversity"

## For SW4_0208 and SW4_0973, duplicate rows and replace category by Biodiversity
papersBioDiv    <- subset(datRespvar, grepl("diversity", datRespvar$Response.variable_paper) & !Response.variable_category %in% "Biodiversity")
papersBioDiv    <- rbind(papersBioDiv, subset(datRespvar, grepl("richness", datRespvar$Response.variable_paper) & !Response.variable_category %in% "Biodiversity"))
papersBioDiv    <- papersBioDiv[!duplicated(papersBioDiv),]

papersDupl                              <- subset(papersBioDiv, SW.ID %in% c("SW4_0208","SW4_0973"))
papersDupl$Response.variable_category   <- "Biodiversity"

datRespvar                              <- rbind(datRespvar, papersDupl)
datRespvar                              <- datRespvar[order(datRespvar$SW.ID),]

rm(papersBioDiv, papersDupl)


### List all reported Response variables to have a final check ----
sort(unique(datRespvar$Response.variable_paper))

#Corrected some misspellings and excluded a paper (directly in data extraction files on the Sharepoint)


### Check which papers still classified as 'Other' ----
papers                                   <- datRespvar[datRespvar$Response.variable_category %in% "Other",]
sort(unique(papers$Response.variable_paper))
#Three unclassified left - best to keep as 'Other'



#-----------------------------------------------#
# Direction of relationship ----
#-----------------------------------------------#

### Check missing values ----
datNotSpec        <- subset(datRespvar, Direction.of.relationship %in% "Not specified")
datNotSpec[,c(1,33)]
#Have checked them and made changes in the data extraction files on the Sharepoint.


### Do some manual changes after in-depth exploration of case studies ----
datRespvar$Direction.of.relationship[datRespvar$SW.ID %in% "SW4_0703"]  <- "Negative"

datRespvar$Response.variable_paper[datRespvar$SW.ID %in% "SW4_1107"]    <- "bycatch"
datRespvar$Response.variable_category[datRespvar$SW.ID %in% "SW4_1107"] <- "Behaviour"

datRespvar$Response.variable_category[datRespvar$SW.ID %in% "SW4_0355"] <- "Behaviour"

datRespvar$Direction.of.relationship[datRespvar$SW.ID %in% "SW4_1991"] <- "Positive"


#-----------------------------------------------#
# Magnitude of relationship ----
#-----------------------------------------------#

#!NOTE; can only be checked when running code above on data_allScreened

if(ncol(datRespvar) > 50){
  
  ### Check missing values ----
  datMagnNA        <- subset(datRespvar, is.na(Magnitude.of.relationship) & is.na(Exclusion.Criteria))
  
  # How many papers?
  length(unique(datMagnNA$SW.ID)) #37 papers
  
  # Check for each Ecosystem component the number of papers with NA for magnitude of relationship
  ecoComp     <- unique(datRespvar$Ecosystem.component_level1)
  ecoComp     <- ecoComp[-is.na(ecoComp)]
  ecoMagn     <- data.frame(Ecosystem.component_level1 = ecoComp, NoPapersNoMagn = NA, PercPapersNoMagn = NA)
  
  for(iEco in 1:length(ecoComp)){
    
    subdat   <- subset(datRespvar, Ecosystem.component_level1 %in% ecoComp[iEco])
    
    magNA    <- length(unique(subdat$SW.ID[is.na(subdat$Magnitude.of.relationship)]))
    noPapers <- length(unique(subdat$SW.ID))
    
    ecoMagn[iEco,2] <- magNA
    ecoMagn[iEco,3] <- round(magNA / noPapers * 100,1)
    
  }
  
  # Check for each Response variable category the number of papers with NA for magnitude of relationship
  respVar     <- unique(datRespvar$Response.variable_category)
  respVar     <- respVar[-is.na(respVar)]
  respMagn    <- data.frame(Response.variable_category = respVar, NoPapersNoMagn = NA, PercPapersNoMagn = NA)
  
  for(iRes in 1:length(respVar)){
    
    subdat   <- subset(datRespvar, Response.variable_category %in% respVar[iRes])
    
    magNA    <- length(unique(subdat$SW.ID[is.na(subdat$Magnitude.of.relationship)]))
    noPapers <- length(unique(subdat$SW.ID))
    
    respMagn[iRes,2] <- magNA
    respMagn[iRes,3] <- round(magNA / noPapers * 100,1)
    
  }
  
  # Check for each Pressure variable category the number of papers with NA for magnitude of relationship
  pressVar     <- unique(datRespvar$Pressure.variable_category)
  pressVar     <- pressVar[-is.na(pressVar)]
  pressMagn    <- data.frame(Pressure.variable_category = pressVar, NoPapersNoMagn = NA, PercPapersNoMagn = NA)
  
  for(iPre in 1:length(pressVar)){
    
    subdat   <- subset(datRespvar, Pressure.variable_category %in% pressVar[iPre])
    
    magNA    <- length(unique(subdat$SW.ID[is.na(subdat$Magnitude.of.relationship)]))
    noPapers <- length(unique(subdat$SW.ID))
    
    pressMagn[iPre,2] <- magNA
    pressMagn[iPre,3] <- round(magNA / noPapers * 100,1)
    
  }
  
  
  ### Check text, spaces, underscores and clean ---
  
  # Remove new lines
  datRespvar$Magnitude.of.relationship <- str_replace_all(datRespvar$Magnitude.of.relationship, "[\r\n]" , "")
  
  # Remove weird codes
  datRespvar$Magnitude.of.relationship <- str_replace_all(datRespvar$Magnitude.of.relationship, "_x0002_" , "")
  datRespvar$Magnitude.of.relationship <- str_replace_all(datRespvar$Magnitude.of.relationship, "_x0001_" , "")
  
}



#-----------------------------------------------#
# Save dataset ----
#-----------------------------------------------#

# Convert datRespvar back into multiple response variables per row. Consideration is needed when a row has been split
# into multiple ones. If all split rows got assigned the same Response variable category, then all can be merged into
# one row again. If split rows got assigned different Response variable categories, then they have to remain split.

# Add pasted info for rows with multiple pressure variable categories
data_collapsed <- datRespvar[0,]

for(iRow in unique(datRespvar$ROWID)){
  subdat                            <- subset(datRespvar, ROWID %in% iRow)
  
  # If there's only one pressure variable, add row
  if(nrow(subdat) == 1){
    
    data_collapsed <- rbind(data_collapsed, subdat)
    
  }
  # If there's more than one response variable but all have assigned the same category, collapse and add as one row
  if(nrow(subdat) > 1 & 
     length(unique(subdat$Response.variable_category)) == 1){
    
    subdat$Response.variable_paper <- paste(subdat$Response.variable_paper, collapse = " _ ")
    data_collapsed <- rbind(data_collapsed, subdat[1,])
    
  }
  # If there's more than one response variable but not all have assigned the same category and they are all unique, add all rows
  if(nrow(subdat) > 1 &
     length(unique(subdat$Response.variable_category)) > 1 &
     length(unique(subdat$Response.variable_category)) == nrow(subdat)){
    
    data_collapsed <- rbind(data_collapsed, subdat)
  }
  # If there's more than one response variable but not all have assigned the same category but they are not all unique, combine the above options
  if(nrow(subdat) > 1 &
     length(unique(subdat$Response.variable_category)) > 1 &
     !length(unique(subdat$Response.variable_category)) == nrow(subdat)){
    
    for(iCat in unique(subdat$Response.variable_category)){
      subsubdat <- subset(subdat, Response.variable_category %in% iCat)
      
      if(nrow(subsubdat) == 1){
        
        data_collapsed <- rbind(data_collapsed, subsubdat)
      }
      
      if(nrow(subsubdat) > 1){
        
        subsubdat$Response.variable_paper <- paste(subsubdat$Response.variable_paper, collapse = " _ ")
        data_collapsed <- rbind(data_collapsed, subsubdat[1,])
      }
    }
    
  }
  
}


# Check duplicated ROWID
rowIDdupl               <- data_collapsed$ROWID[duplicated(data_collapsed$ROWID)]
datDupl                 <- subset(data_collapsed, ROWID %in% rowIDdupl) #this is because rows were split or duplicated and got a new response variable category
data_collapsed$ROWID    <- c(1:nrow(data_collapsed)) #give new unique row ID


# If cropped dataset should be saved:
if(dataset == "cropped"){
  saveRDS(data_collapsed, paste0(datPath,"data_correctTaxa_PressVar_RespVar.rds"))
  write.xlsx(data_collapsed, file=paste0(datPath, "data_correctTaxa_PressVar_RespVar.xlsx"))
}

# If full dataset should be saved:
if(dataset == "full"){
  saveRDS(data_collapsed, paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar.rds"))
  write.xlsx(data_collapsed, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar.xlsx"))
}
