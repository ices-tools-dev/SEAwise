#####################################################################################################################
#####################################################################################################################
#
#     Step 3. Detailed look at species and creating new version of the data with corrected taxonomy
#
#     By Karin van der Reijden
#     April 2022
#
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

#-----------------------------------------------
# Load libraries and set Paths.
#-----------------------------------------------
library(data.table)
library(RColorBrewer)
library(raster)
library(plotrix)
library(sf)
library(viridis)
library(stringr)
library(worms)

datPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

#-----------------------------------------------
# Read in data
#
#  info: This step is dependent on step 1 and step 2.
#-----------------------------------------------
data                                 <- readRDS(data, file=paste0(datPath, "data.rds"))
# data                                 <- readRDS(data, file=paste0(datPath, "data_AllScreened.rds"))

#-----------------------------------------------
# Split species input to individual rows 
#-----------------------------------------------
specs                                <- tstrsplit(data$Species.taxonomic.group.s., split="_")
dat1                                 <- data
dat1$Species.taxonomic.group.s.      <- specs[[1]] 
length(unique(dat1$Species.taxonomic.group.s.)) # 403 unique records
for(iL in c(2:length(specs))){
  dat2                               <- data
  dat2$Species.taxonomic.group.s.    <- specs[[iL]]
  dat2                               <- dat2[is.na(Species.taxonomic.group.s.)==F,,]
  dat1                               <- rbind(dat1, dat2)
  rm(dat2)
}# end iL loop. 
length(unique(dat1$Species.taxonomic.group.s.)) # 902 unique records

dat1$Species.taxonomic.group.s.      <- str_trim(dat1$Species.taxonomic.group.s., side="both"); length(unique(dat1$Species.taxonomic.group.s.)) # 668 unique records
dat1$Species.taxonomic.group.s.      <- str_to_lower(dat1$Species.taxonomic.group.s.); length(unique(dat1$Species.taxonomic.group.s.)) # 648 unique records
dat1$Species.taxonomic.group.s.      <- ifelse(dat1$Species.taxonomic.group.s. %in% c("", "unknown"), NA, dat1$Species.taxonomic.group.s.); length(unique(dat1$Species.taxonomic.group.s.)) # 646 unique records

saveRDS(dat1, file=paste0(outPath, "dat1a.rds"))

#-----------------------------------------------
# Run species over worms-database 
#-----------------------------------------------
specs                                <- data.table(Spec = unique(dat1$Species.taxonomic.group.s.))
specs                                <- specs[!is.na(Spec)==T,,]

specs2                               <- wormsbynames(specs$Spec)
specs$valid_name                     <- specs2$valid_name
specs$family                         <- specs2$family
specs$order                          <- specs2$order
specs$class                          <- specs2$class
specs$rank                           <- specs2$rank
specs$valid_AphiaID                  <- specs2$valid_AphiaID
specsIDed                            <- specs[is.na(valid_name) == FALSE,,]

## save matched/identified species
saveRDS(specsIDed, file=paste0(outPath, "specsIDed1.rds"))

#-----------------------------------------------
# Manually correct non-matches in excel and read in 
#-----------------------------------------------
## Write non-matches to excel for easy manual correction and later worms check
nospecs                              <- data.table(Spec = unique(subset(specs, is.na(valid_name) == T)$Spec))
# write.table(nospecs, file=paste0(outPath, "nospecs_4.csv"), col.names=TRUE, row.names=FALSE)

## Read in manually corrected table 
nospecs2                             <- read.table(paste0(outPath, "nospecs_identified_3.csv"), header=T, sep=";")
nospecs2                             <- data.table(nospecs2)
nospecs                              <- nospecs2[Spec %in% nospecs$Spec,,]
nospecs$Taxon                        <- str_to_lower(nospecs$Taxon)
nospecs$Taxon                        <- str_trim(nospecs$Taxon, side="both")
nospecs[nospecs$Spec %in% "auks",2]  <- "alcidae"

#-----------------------------------------------
# Run non-matches again over worms-database 
#-----------------------------------------------
nospecs3                             <- data.table(Taxon = sort(unique(nospecs$Taxon))) # 131 rows
nospecs3                             <- nospecs3[!Taxon %in% c("algae", "benthic community", "benthic fish", "benthos", "benthos_epifauna", "benthos_infauna", "fish", "fulica atra",
                                                               "gulosus aristotelis", "habitat", "ichthyaetus audouinii", "larus", 
                                                               "larus marinus", "mergini", "pelagic fish", "plankton", "rhodolith", "unknown")]
nospecs3$group                       <- c(rep(1:11, each=10) , rep(12, nrow(nospecs3)-110))
nospecs4                             <- wormsbymatchnames(subset(nospecs3, group==1)$Taxon)
for(iGr in 2:max(nospecs3$group)){
  print(iGr)
  nospecs5                           <- wormsbymatchnames(subset(nospecs3, group==iGr)$Taxon)
  nospecs4                           <- rbind(nospecs4, nospecs5)
  rm(nospecs5)
}
nospecs3$valid_name                  <- nospecs4$valid_name
nospecs3$family                      <- nospecs4$family
nospecs3$order                       <- nospecs4$order
nospecs3$class                       <- nospecs4$class
nospecs3$rank                        <- nospecs4$rank
nospecs3$valid_AphiaID               <- nospecs4$valid_AphiaID

## Add Worms-info to nospecs
nospecs$valid_name                   <- nospecs3$valid_name [match(nospecs$Taxon, nospecs3$Taxon)]
nospecs$family                       <- nospecs3$family [match(nospecs$Taxon, nospecs3$Taxon)]
nospecs$order                        <- nospecs3$order [match(nospecs$Taxon, nospecs3$Taxon)]
nospecs$class                        <- nospecs3$class [match(nospecs$Taxon, nospecs3$Taxon)]
nospecs$rank                         <- nospecs3$rank [match(nospecs$Taxon, nospecs3$Taxon)]
nospecs$valid_AphiaID                <- nospecs3$valid_AphiaID [match(nospecs$Taxon, nospecs3$Taxon)]

## Merge identified species to specsIDed
specsIDed2                           <- nospecs[is.na(valid_name) == FALSE,,]
specsIDed2$Taxon                     <- NULL
specsIDed                            <- rbind(specsIDed, specsIDed2)
saveRDS(specsIDed, file=paste0(outPath, "specsIDed2.rds"))

## Keep overview with non-identified species
stillnospecs                         <- data.table(Spec  = subset(nospecs, is.na(nospecs$valid_name) == TRUE)$Spec,
                                                   Taxon = subset(nospecs, is.na(nospecs$valid_name) == TRUE)$Taxon)

#-----------------------------------------------
# Create taxonomic data for non-worms records and merge to specsIDed 
#-----------------------------------------------
specinfocreated                      <- data.table(Taxon         = sort(unique(stillnospecs$Taxon)),
                                                   valid_name    = c("Algae", "Benthic community", "Fish_benthic", "Benthos", "Benthos_epifauna",
                                                                     "Benthos_infauna", "Fish", "Fulica atra", "Phalacrocorax aristotelis", "Habitat", 
                                                                     "Larus audouinii", "Larus marinus", "Larus", "Anatidae", "Fish_pelagic", 
                                                                     "Plankton", "Rhodoliths", "Unknown"),
                                                   family        = c("Algae", "Benthic community", "Fish", "Benthos", "Benthos", "Benthos", "Fish",
                                                                     "Rallidae", "Phalacrocoracidae", "Habitat", "Laridae", "Laridae", "Laridae", 
                                                                     "Anatidae", "Fish", "Plankton", "Algae", "Unknown"),
                                                   order         = c("Algae", "Benthic community", "Fish", "Benthos", "Benthos", "Benthos", "Fish",
                                                                     "Gruiformes", "Pelecaniformes", "Habitat", "Charadriiformes", "Charadriiformes",
                                                                     "Charadriiformes", "Anseriformes", "Fish", "Plankton", "Algae", "Unknown"),
                                                   class         = c("Algae", "Benthic community", "Fish", "Benthos", "Benthos", "Benthos", "Fish",
                                                                     "Aves", "Aves", "Habitat", "Aves", "Aves", "Aves", "Aves", "Fish", "Plankton", 
                                                                     "Algae", "Unknown"),
                                                   rank          = c(rep(NA, 7), "Species", "Species", NA, "Species", "Species", "Genus", "Subfamily", rep(NA, 4)),
                                                   valid_AphiaID = c(rep(NA, 7), 232054, 137178, NA, 137139, 137146, 137043, 136973, NA, NA, NA, NA))

stillnospecs                         <- merge(stillnospecs, specinfocreated, by="Taxon", all=T)
stillnospecs$Taxon                   <- NULL
specsIDed                            <- rbind(specsIDed, stillnospecs)
saveRDS(specsIDed, file=paste0(outPath, "specsIDed3.rds"))

#-----------------------------------------------
# Add (created) worms data to dat1.rds
#-----------------------------------------------
rm(list=ls())
datPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

dat1                                 <- readRDS(paste0(outPath, "dat1a.rds"))
specsIDed                            <- readRDS(paste0(outPath, "specsIDed3.rds"))
specsIDed$Species.taxonomic.group.s. <- specsIDed$Spec
specsIDed$Spec                       <- NULL

dat1                                 <- merge(dat1, specsIDed, by="Species.taxonomic.group.s.", all.x=TRUE)
saveRDS(dat1,  paste0(outPath, "dat1b.rds"))


#-----------------------------------------------
# Check consistency between taxonomy and ecosystem component level: Benthos
#-----------------------------------------------
rm(list=ls())
datPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

dat1                                 <- readRDS(paste0(outPath, "dat1b.rds"))
dat2                                 <- dat1[,c("Species.taxonomic.group.s.", "Ecosystem.component_level1", 
                                                "Ecosystem.component_level2", "Ecosystem.component_level3",
                                                "valid_name", "ROWID")]


## Is "Benthos" always benthos?
benthos                              <- dat2[Ecosystem.component_level1 == "Benthos",,]
## Interesting records in Species.taxonomic.group.s.: "fish", "fishery for (but not testing any effect on) solea solea"
paperstocheck_benthos                <- dat1[Ecosystem.component_level1 == "Benthos" & Species.taxonomic.group.s. %in% c("fish", "fishery for (but not testing any effect on) solea solea")]  

## Are "epifauna" en "infauna" (ECL2) always nested within "Benthos" (ECL1)? --> YES!
table(is.na(dat1[Ecosystem.component_level2 == "Benthic_epifauna"]$Ecosystem.component_level1))
unique(dat1[Ecosystem.component_level2 == "Benthic_epifauna"]$Ecosystem.component_level1)

table(is.na(dat1[Ecosystem.component_level2 == "Benthic_infauna"]$Ecosystem.component_level1))
unique(dat1[Ecosystem.component_level2 == "Benthic_infauna"]$Ecosystem.component_level1)

## Do benthos_epifauna (valid name) and benthic_epifauna (ECL2) match?
epibenthos                           <- dat1[Ecosystem.component_level2 == "Benthic_epifauna"]
sort(unique(epibenthos$valid_name))
# strange results: "Fish", "Benthos", "Benthos_infauna"
paperstocheck_benthos                <- rbind(paperstocheck_benthos,
                                              epibenthos[valid_name %in% c("Fish", "Benthos", "Benthos_infauna")])  


## Do benthos_epifauna (valid name) and benthic_epifauna (ECL2) match?
infauna                              <- dat1[Ecosystem.component_level2 == "Benthic_infauna"]
sort(unique(infauna$valid_name))
# strange results: "Habitat", "Benthos_epifauna", "Benthos"
paperstocheck_benthos                <- rbind(paperstocheck_benthos,
                                              infauna[valid_name %in% c("Habitat", "Benthos", "Benthos_epifauna")])  


## Is there "benthos" when ECL1 != benthos?
unique(dat1[Ecosystem.component_level1 != "Benthos"]$Ecosystem.component_level2) # This is all ok (but shows sediment types though! --> checked and correct)
sort(unique(dat1[Ecosystem.component_level1 != "Benthos"]$valid_name))  # Benthos and benthos_epifauna are listed
paperstocheck_benthos                <- rbind(paperstocheck_benthos,
                                              dat1[Ecosystem.component_level1 != "Benthos" & valid_name %in% c("Benthos", "Benthos_epifauna")])  

# saveRDS(paperstocheck_benthos, file=paste0(outPath, "paperstocheck_benthos.rds"))

#-----------------------------------------------
# Check consistency between taxonomy and ecosystem component level: focus on non-taxonomic descriptions
#-----------------------------------------------
rm(list=ls())
datPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

data                                 <- readRDS(data, file=paste0(datPath, "data.rds"))
dat1                                 <- readRDS(paste0(outPath, "dat1b.rds"))
dat2                                 <- dat1[,c("Species.taxonomic.group.s.", "Ecosystem.component_level1", 
                                                "Ecosystem.component_level2", "Ecosystem.component_level3",
                                                "valid_name", "ROWID", "SW.ID")]
dat3                                 <- dat2[valid_name %in% c("Algae", "Benthic community", "Benthos", "Benthos_epifauna", "Benthos_infauna",
                                                               "Fish", "Fish_benthic", "Fish_pelagic", "Habitat", "Plankton", "Rhodoliths", "Unknown")]
dat3$Ecosystem.component_level2[is.na(dat3$Ecosystem.component_level2)] <- "Not_provided"

## Check ECL (1-2) vs non-taxonomic description.  --> Emailed overview to Esther on 13-10-22
for(iVN in unique(dat3$valid_name)){
  dat4                               <- dat3[valid_name == iVN,,]
  print(iVN)
  print(table(dat4$Ecosystem.component_level1, dat4$Ecosystem.component_level2))
}

## Check abstract of deviating descriptions (compared to ECL)
ReadAbstract1                        <- unique(data[SW.ID %in% subset(dat3, valid_name == "Algae")$SW.ID]$Abstract) # benthos _benthic_epifauna is correct
RA2                                  <- dat3[valid_name == "Benthic community" & Ecosystem.component_level1 == "Benthos" & Ecosystem.component_level2 == "Not_provided",]
ReadAbstract2                        <- unique(data[SW.ID %in% RA2$SW.ID]$Abstract) # benthos_NA is correct
RA3                                  <- dat3[valid_name == "Benthic community" & Ecosystem.component_level1 == "Foodweb",]
ReadAbstract3                        <- unique(data[SW.ID %in% RA3$SW.ID]$Abstract) # Foodweb is correct
RA4                                  <- dat3[valid_name == "Benthos" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Demersal",]
ReadAbstract4                        <- unique(data[SW.ID %in% RA4$SW.ID]$Abstract) ## Hmmm, needs a proper investigation: SW.ID = "SW4_0805" by Thorpe # --> non-taxonomic descriptions can be deleted!
RA5                                  <- dat3[valid_name == "Benthos" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Pelagic",]
ReadAbstract5                        <- unique(data[SW.ID %in% RA5$SW.ID]$Abstract) ## SW.ID = "SW4_0805", so all good!
RA6                                  <- dat3[valid_name == "Benthos" & Ecosystem.component_level1 == "Foodweb",]
ReadAbstract6                        <- unique(data[SW.ID %in% RA6$SW.ID]$Abstract) ## Hmmm, 2 look off. SW4_1429 (checked: suggest to change to Benthos_NA) and SW4_1531 (checked: fine!)
RA7                                  <- dat3[valid_name == "Benthos" & Ecosystem.component_level1 == "Physical_habitats",]
ReadAbstract7                        <- unique(data[SW.ID %in% RA7$SW.ID]$Abstract) ## SW4_0851 Change to ECL: Benthos, Benthic_epifauna, corals. add Taxonomic names: Lophelia pertusa, Madrepora oculata, Funiculina quadrangularis, Isidella elongata
RA8                                  <- dat3[valid_name == "Benthos_epifauna" & Ecosystem.component_level1 == "Benthos" & Ecosystem.component_level2 == "Benthic_infauna",]
ReadAbstract8                        <- unique(data[SW.ID %in% RA8$SW.ID]$Abstract) ## Fine to remove VN
RA9                                  <- dat3[valid_name == "Benthos_epifauna" & Ecosystem.component_level1 == "Foodweb",]
ReadAbstract9                        <- unique(data[SW.ID %in% RA9$SW.ID]$Abstract) ## Change to ECL: Benthos, Benthic_epifauna.
RA10                                 <- dat3[valid_name == "Benthos_infauna" & Ecosystem.component_level1 == "Benthos" & Ecosystem.component_level2 == "Benthic_epifauna",]
ReadAbstract10                       <- unique(data[SW.ID %in% RA10$SW.ID]$Abstract) ## Fine to remove VN
RA11                                 <- dat3[valid_name == "Fish" & Ecosystem.component_level1 == "Benthos" & Ecosystem.component_level2 == "Benthic_epifauna",]
ReadAbstract11                       <- unique(data[SW.ID %in% RA11$SW.ID]$Abstract) ## Fine to remove VN
RA12                                 <- dat3[valid_name == "Fish" & Ecosystem.component_level1 == "Cephalopods",]
ReadAbstract12                       <- unique(data[SW.ID %in% RA12$SW.ID]$Abstract) ## Fine to remove VN
RA13                                 <- dat3[valid_name == "Fish" & Ecosystem.component_level1 == "Foodweb",]
ReadAbstract13                       <- unique(data[SW.ID %in% RA13$SW.ID]$Abstract) ## Fine to remove VN (SW4_1560, SW4_1531, SW4_1560), one results in Esther making changes in the data sheets (SW4_0621)
RA14                                 <- dat3[valid_name == "Fish_benthic" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Pelagic",] 
ReadAbstract14                       <- unique(data[SW.ID %in% RA14$SW.ID]$Abstract) ## SW.ID = "SW4_0805", so all good!
RA15                                 <- dat3[valid_name == "Fish_benthic" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Not_provided",] 
ReadAbstract15                       <- unique(data[SW.ID %in% RA15$SW.ID]$Abstract) ## Fine to remove VN (SW4_0738, SW4_0881, SW4_0504)
RA16                                 <- dat3[valid_name == "Fish_benthic" & Ecosystem.component_level1 == "Foodweb",] 
ReadAbstract16                       <- unique(data[SW.ID %in% RA16$SW.ID]$Abstract) ## Fine to remove VN (SW4_1952)
RA17                                 <- dat3[valid_name == "Fish_pelagic" & Ecosystem.component_level1 == "Foodweb",] 
ReadAbstract17                       <- unique(data[SW.ID %in% RA17$SW.ID]$Abstract) ## Fine to remove VN (SW4_1952)
RA18                                 <- dat3[valid_name == "Fish_pelagic" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Not_provided",] 
ReadAbstract18                       <- unique(data[SW.ID %in% RA18$SW.ID]$Abstract) ## Fine to remove VN (SW4_0738, SW4_0881, SW4_0504)
RA19                                 <- dat3[valid_name == "Fish_pelagic" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Demersal",] 
ReadAbstract19                       <- unique(data[SW.ID %in% RA19$SW.ID]$Abstract) ## SW.ID = "SW4_0805", so all good!
RA20                                 <- dat3[valid_name == "Habitat" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Demersal",] 
ReadAbstract20                       <- unique(data[SW.ID %in% RA20$SW.ID]$Abstract) ## SW.ID = "SW4_0805", so all good!
RA21                                 <- dat3[valid_name == "Habitat" & Ecosystem.component_level1 == "Fish_teleost" & Ecosystem.component_level2 == "Pelagic",] 
ReadAbstract21                       <- unique(data[SW.ID %in% RA20$SW.ID]$Abstract) ## SW.ID = "SW4_0805", so all good!
RA22                                 <- dat3[valid_name == "Habitat" & Ecosystem.component_level1 == "Benthos" & Ecosystem.component_level2 == "Benthic_infauna",] 
ReadAbstract22                       <- unique(data[SW.ID %in% RA20$SW.ID]$Abstract) ## Fine to remove VN (SW4_0675)

## Conclusions: Most non-taxonomic descriptions can be removed without a problem. However, there are 3 papers that require changes.
# SW4_0621: The classification of "foodweb" will be changed to "fish_teleost" and "fish_cartilaginous" manually by Esther.
# SW4_1429: This classification of "foodweb" should be changed to "benthos-NA".
# SW4_0851: This classification of Physical_habitats" should be changed to "Benthos - Benthic_epifauna - Corals" manually. Also taxonomic info should be added.



#-----------------------------------------------
# Update November 8 2022: Correct taxonomic descriptions in original dataset; remove non-taxonomic descriptions.
#-----------------------------------------------
rm(list=ls())
datPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                              <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"

# load original dataset
data                                 <- readRDS(file=paste0(datPath, "data.rds"))
dat1                                 <- readRDS(file=paste0(outPath, "dat1a.rds"))

# load list of correct taxonomic descriptions
specsIDed                           <- readRDS(file=paste0(outPath, "specsIDed3.rds"))
specsIDed                           <- specsIDed[is.na(valid_AphiaID) == F,,]

# Merge correct name to description; remove non-taxonomic descriptions
dat1$Valid_name                     <- specsIDed$valid_name [match(dat1$Species.taxonomic.group.s., specsIDed$Spec)]
dat1                                <- dat1[is.na(Valid_name)==F,,]
dat1                                <- dat1[order(dat1$ROWID),]

# Determine single and multiple species descriptions rows
Nrows                               <- dat1[, .N, by="ROWID"]
dat1$Nrows                          <- Nrows$N [match(dat1$ROWID, Nrows$ROWID)]
dat1sub                             <- dat1[Nrows > 1,,]

# first create datatable for rows with single-species descriptions
dt_taxondescr                       <- dat1[Nrows == 1,,]
dt_taxondescr                       <- dt_taxondescr[,c("ROWID", "Valid_name"),]

# Add pasted info for rows describing multiple species
for (iRow in unique(dat1sub$ROWID)){
  subdat                            <- subset(dat1sub, ROWID==iRow)
  dt                                <- data.table(ROWID  = iRow,
                                                  Valid_name = paste(unique(subdat$Valid_name), collapse = " _ "))
  dt_taxondescr                     <- rbind(dt_taxondescr, dt)
} # end iRow-loop

# format dt_taxondescr
dt_taxondescr                       <- dt_taxondescr[order(dt_taxondescr$ROWID),,]

## Double-check the 'missing' descriptions
a                                   <- setdiff(unique(data$ROWID), unique(dt_taxondescr$ROWID))
checks                              <- data[ROWID %in% a,] # Looks fine!

data$Species.taxonomic.group.s.     <- dt_taxondescr$Valid_name [match(data$ROWID, dt_taxondescr$ROWID)]
data$Species.taxonomic.group.s.[is.na(data$Species.taxonomic.group.s.)] <- ""

saveRDS(data, file=paste0(datPath, "data_correctTaxa.rds"))
# saveRDS(data, file=paste0(datPath, "data_AllScreened_correctTaxa.rds"))
