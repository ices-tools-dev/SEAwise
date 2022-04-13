#####################################################################################################################
#####################################################################################################################
#
#     Script to read, merge, and analyse data extraction files from SEAwise task 4.1
#     Step 3. Detailed look at species
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

outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------
# Read in data
#
#  info: This step is dependent on step 1.
#-----------------------------------------------
data                                  <- readRDS(data, file=paste0(outPath, "data.rds"))


#-----------------------------------------------
# Split species input to individual rows
#-----------------------------------------------
specs                                <- tstrsplit(data$Species.taxonomic.group.s., split="_")
dat1                                 <- data
dat1$Species.taxonomic.group.s.      <- specs[[1]] 
length(unique(dat1$Species.taxonomic.group.s.)) # 397 unique records
for(iL in c(2:length(specs))){
  dat2                               <- data
  dat2$Species.taxonomic.group.s.    <- specs[[iL]]
  dat2                               <- dat2[is.na(Species.taxonomic.group.s.)==F,,]
  dat1                               <- rbind(dat1, dat2)
  rm(dat2)
}# end iL loop. # 880 unique records.
dat1$Species.taxonomic.group.s.      <- str_trim(dat1$Species.taxonomic.group.s., side="both") # 665 unique records
dat1$Species.taxonomic.group.s.      <- str_to_lower(dat1$Species.taxonomic.group.s.) # 642 unique records
dat1$Species.taxonomic.group.s.      <- ifelse(dat1$Species.taxonomic.group.s. %in% c("", "unknown"), NA, dat1$Species.taxonomic.group.s.) # 640 unique records

#-----------------------------------------------
# Run species over worms-database
#-----------------------------------------------
specs                                <- data.table(Spec = unique(dat1$Species.taxonomic.group.s.))
specs                                <- specs[!is.na(Spec)==T,,]
specs$group                          <- c(rep(c(1:25), each=25), rep(26, times=(nrow(specs)-625)))
specs$Spec[276]                      <- "larus audouinii" #manual correction

specs2                               <- wormsbynames(subset(specs, group == 1)$Spec)
for(i in c(2:10,12:26)){ 
  print(i)
  specs3                             <- wormsbynames(subset(specs, group == i)$Spec)
  specs2                             <- rbind(specs2, specs3)
}
## Separate for group 11: some error - not really clue why. Manual correction bird species still needed.
for(i in c(11)){ 
  print(i)
  specs4                             <- subset(specs, group == i)
  specs4$Spec[4]                     <- "larus audouinii" 
  specs3                             <- wormsbynames(specs4$Spec[c(1:25)])
  specs2                             <- rbind(specs2, specs3)
  rm(specs3)
}
specs$valid_name                     <- specs2$valid_name
specs$family                         <- specs2$family
specs$order                          <- specs2$order
specs$class                          <- specs2$class
specs$rank                           <- specs2$rank
specs$valid_AphiaID                  <- specs2$valid_AphiaID


#-----------------------------------------------
# Manually correct non-matches in excel and read in
#-----------------------------------------------
## Write non-matches to excel for easy manual correction and later worms check
nospecs                              <- data.table(Spec = unique(subset(specs, is.na(valid_name) == T)$Spec))
#write.table(nospecs, file=paste0(outPath, "nospecs.csv"), col.names=TRUE, row.names=FALSE)

## Read in manually corrected table
nospecs                              <- read.table(paste0(outPath, "nospecs2.csv"), header=T, sep=";")
nospecs                              <- data.table(nospecs)
nospecs$Taxon                        <- str_to_lower(nospecs$Taxon)
nospecs$Taxon                        <- str_trim(nospecs$Taxon, side="both")

#-----------------------------------------------
# Run non-matches again over worms-database
#-----------------------------------------------
# Exclude 'species/records' that are not in worms
nospecs2                             <- nospecs[!Taxon %in% c("benthos", "habitat", "fish", "unknown", "benthos_epifauna",
                                                              "pelagic fish", "benthic fish", "community", "plankton",
                                                              "benthic community", "benthos_infauna", "mergini",
                                                              "gulosus aristotelis", "algae", "fulica atra", "rhodolith",
                                                              "ichthyaetus audouinii", "larus marinus",
                                                              "alcidae"),,]
nospecs2$group                       <- c(rep(1:14, each=10) , rep(15, nrow(nospecs2)-140))
nospecs3                             <- wormsbymatchnames(subset(nospecs2, group==1)$Taxon)
for(iGr in 2:15){
  print(iGr)
  nospecs4                           <- wormsbymatchnames(subset(nospecs2, group==iGr)$Taxon)
  nospecs3                           <- rbind(nospecs3, nospecs4)
  rm(nospecs4)
}
nospecs2$valid_name                  <- nospecs3$valid_name
nospecs2$family                      <- nospecs3$family
nospecs2$order                       <- nospecs3$order
nospecs2$class                       <- nospecs3$class
nospecs2$rank                        <- nospecs3$rank
nospecs2$valid_AphiaID               <- nospecs3$valid_AphiaID
nospecs2$Taxon                       <- NULL

species                              <- rbind(specs, nospecs2)
stillnospecs                         <- nospecs[Taxon %in% c("benthos", "habitat", "fish", "unknown", "benthos_epifauna",
                                                             "pelagic fish", "benthic fish", "community", "plankton",
                                                             "benthic community", "benthos_infauna", "mergini",
                                                             "gulosus aristotelis", "algae", "fulica atra", "rhodolith",
                                                             "ichthyaetus audouinii", "larus marinus",
                                                             "alcidae"),,]                             
rm(nospecs2, nospecs3, specs2, specs, nospecs)

#-----------------------------------------------
# Create taxonomic data for non-worms records and merge all
#-----------------------------------------------
specinfocreated                      <- data.table(Taxon = c("benthos", "habitat", "fish", "unknown", "benthos_epifauna",
                                                             "pelagic fish", "benthic fish", "community", "plankton",
                                                             "benthic community", "benthos_infauna", "mergini",
                                                             "gulosus aristotelis", "algae", "fulica atra", "rhodolith",
                                                             "ichthyaetus audouinii", "larus marinus", "alcidae"),
                                                   valid_name = c("Benthos", "Habitat", "Fish", "Unknown", "Benthos_epifauna",
                                                                  "Fish_pelagic", "Fish_benthic", "Unknown", "Plankton",
                                                                  "Benthic community", "Benthos_infauna", "Anatidae",
                                                                  "Gulosus aristotelis", "Algae", "Fulica atra", "Rhodoliths",
                                                                  "Ichthyaetus audouinii", "Larus marinus", "Alcidae"),
                                                   family = c("Benthos", "Habitat", "Fish", "Unknown", "Benthos",
                                                              "Fish", "Fish", "Unknown", "Plankton",
                                                              "Benthic community", "Benthos", "Anatidae",
                                                              "Phalacrocoracidae", "Algae", "Rallidae", "Algae",
                                                              "Laridae", "Laridae","Alcidae"),
                                                   order = c("Benthos", "Habitat", "Fish", "Unknown", "Benthos",
                                                             "Fish", "Fish", "Unknown", "Plankton",
                                                             "Benthic community", "Benthos", "Anseriformes",
                                                             "Suliformes", "Algae", "Gruiformes", "Algae",
                                                             "Charadriiformes", "Charadriiformes", "Charadriiformes"),
                                                   class = c("Benthos", "Habitat", "Fish", "Unknown", "Benthos",
                                                             "Fish", "Fish", "Unknown", "Plankton",
                                                             "Benthic community", "Benthos", "Aves", 
                                                             "Aves", "Algae", "Aves", "Algae",
                                                             "Aves", "Aves", "Aves"),
                                                   rank = c(rep(NA, 11), "Subfamily", "Species", NA, "Species", NA,
                                                            "Species", "Species", "Family"),
                                                   valid_AphiaID = c(rep(NA, 19)))
stillnospecs                         <- merge(stillnospecs, specinfocreated, by="Taxon", all=T)
stillnospecs$Taxon                   <- NULL
species$group                        <- NULL
species                              <- rbind(species, stillnospecs)       
species$Species.taxonomic.group.s.   <- species$Spec
species$Spec                         <- NULL
species$class                        <- ifelse(species$order == "Testudines", "Reptilia", species$class)
dat1                                 <- merge(dat1, species, by="Species.taxonomic.group.s.", all=TRUE)                                                   
dat1$class                           <- ifelse(is.na(dat1$class)==T, "not specified", dat1$class)

#-----------------------------------------------
# save dat1
#-----------------------------------------------
saveRDS(dat1, paste0(outPath, "dat1.rds"))
