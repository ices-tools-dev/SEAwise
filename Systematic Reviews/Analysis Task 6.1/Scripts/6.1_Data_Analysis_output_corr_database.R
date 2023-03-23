
#
##
### This file reads the outcomes of the data extraction and 
### makes corrections (e.g. spelling, grouping) and outputs 
### a corrected extraction database
#

library(wesanderson)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggraph)
library(igraph)
library(tidyverse)
library(treemap)
library(RColorBrewer)
library(data.table)
library(raster)
library(plotrix)
library(sf)
library(dplyr)
library(scales)
library(reshape2)
library(gridExtra)
library(ggsankey)

rm(list=ls())

###### LOAD THE DATA ####################################
datPath                               <- "datafiles/" 
outPath                               <- "Routput/"

tab <- read.csv(file=paste0(datPath,'WP6_SEAwise_DataExtraction_ALL_final.csv'),header = TRUE, na.strings=c("","NA"))
colnames(tab)

# rename columns 
colnames(tab) <- c(colnames(tab)[1:32], 'Model.name', 'Biological.component', 'Species', 
                   "Population.str", "Fleet.component", "Fleet.category", "Economic.comp", 
                   "Spatial.context", "Environmental.vars", "Process", "Management.meas",
                   "Used.in.managment", "Indicators.type", 'Indicators.name', "Implemented.in.soft", 
                   "Software.name", "Conclusive.com")

colors =  wes_palette(name = "Zissou1", 5, type="discrete")


# how many papers treated : 673 papers (out of 802)
treated_papers <- unique(tab$SW.ID)
length(treated_papers)
npapers <- length(treated_papers)


# Making a list of unique papers
basis <- tab[,c(1,18)]
basis <- basis[!duplicated(basis$SW.ID), ]

# how many retained :  687 papers
ExCriteria <- as.data.frame(table(basis$Exclusion.Criteria))

# how many rejected
excluded_2 <- subset(basis, !is.na(basis$Exclusion.Criteria))
dim(excluded_2)


# Select the retained papers
data  <- subset(tab, is.na(tab$Exclusion.Criteria))
nkept <- length(unique(data$SW.ID))
data2 <- data[!duplicated(data$SW.ID), ]


# Rejected papers
data3 <- subset(tab, !is.na(tab$Exclusion.Criteria))
data3 <- data3[!duplicated(data3$SW.ID), ]

# NOTE: data is the full database, data2 and data 3 are reduced (excluding multiple lines for same SW6s)

####################################################################
######### CREATE the tables and do plots by field ##################
####################################################################


# EXCLUDED papers #################


#------- Excluded in screening phase

Excluded_screening <- c(19, 158, 178, 422) 
Criteria           <-  c('Language', 'Article type', 'Not relevant to marine ecosystem', 
                         'Not relevant to fisheries management evaluation models')
  
names(Excluded_screening) <- Criteria

#-------- Excluded in extraction phase
x1 <- excluded_2$Exclusion.Criteria

# Other
x1[which(x1 %in% str_subset(x1, "Article type"))]   <- "Article type"
x1[which(x1 %in% str_subset(x1, "Other"))]          <- "Other (unable to find, non-existing)"

# CONTRIBUTORS #####################

contrib     <- tab[,c(1,2)]
contrib     <- contrib[!duplicated(contrib$SW.ID), ]
Contr.Table <- sort(table(contrib$SearchID))
Contr.Table <- as.data.frame(Contr.Table)

# --- authors order
Authors <- read.csv('../Contributors_order.csv')
Authors <- as.matrix(Authors)
Authors <- paste(Authors, sep="")
Authors <- toString(Authors)

# YEAR PUBLISHED ###################

year <- data2$Year
year <- table(year)

data2$Decade <- rep(NA, nrow(data2))
data2$Decade[which(data2$Year < 1990)] <- "1990s"
data2$Decade[which(data2$Year <2000 & data2$Year >= 1990)] <- "2000s"
data2$Decade[which(data2$Year <2010 & data2$Year >= 2000)] <- "2010s"
data2$Decade[which(data2$Year <2020 & data2$Year >= 2010)] <- "2020s"
data2$Decade[which(data2$Year <2030 & data2$Year >= 2020)] <- "2030s"

Year.tab   <- data.frame(year = data2$Year, SW.ID = data2$SW.ID)
Decade.tab <- data.frame(decade = data2$Decade, SW.ID = data2$SW.ID)

# TASK #############################

# split strings
x  <- tstrsplit(data2$WP6.task , split="_")
x1 <- unlist(x)

# identify tasks by subsetting string 6.

frq <- NA
frq[1] <- length(str_subset(x1, ".2"))
frq[2] <- length(str_subset(x1, ".3"))
frq[3] <- length(str_subset(x1, ".4"))
frq[4] <- length(str_subset(x1, ".5"))
frq[5] <- length(str_subset(x1, "6.6"))

task <- NA
task[1] <- "Task 6.2 - Climatic and ecological effects"
task[2] <- "Task 6.3 - Social and economic effects"    
task[3] <- "Task 6.4 - Targets and limits for indicators"    
task[4] <- "Task 6.5 - Management strategies" 
task[5] <- "Task 6.6 - Fisheries in the socio-ecological system" 

Task.tab  <- data.frame(Task = x1, 
                    SW.ID = rep(data2$SW.ID, length(x))
)

#Task.tab <- subset(Task.tab, !is.na(task))

# identify tasks by subsetting string 6.

Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.2'))]      <- task[1]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.3'))]      <- task[2]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.4'))]      <- task[3]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.5'))]      <- task[4]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.6'))]      <- task[5]

# TASK by UNIQUE paper #############################

# split strings
x  <- tstrsplit(data2$WP6.task , split="_")
x1 <- as.data.frame(x)

task <- NA
task[1] <- "6.2"
task[2] <- "6.3"    
task[3] <- "6.4"    
task[4] <- "6.5" 
task[5] <- "6.6" 

# change names 
for (i in 1:ncol(x1)){
  x1[which(x1[,i] %in% str_subset(x1[,i],'.2')),i]      <- task[1]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.3')),i]      <- task[2]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.4')),i]      <- task[3]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.5')),i]      <- task[4]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.6')),i]      <- task[5]
}

x2 <- apply(x1, 1, FUN=function(x) toString(na.omit(x)))
#x2 <- subset(x2, x2!="")

# corrections
x2[which(x2 =="6.5, 6.2")]   <- "6.2, 6.5"
x2[which(x2 =="6.2, 6.3, Task, 6.4, 6.5")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.2, 6.3, 6.5, management strategies")]   <- "6.2, 6.3, 6.5"
x2[which(x2 =="6.4, 6.5, 6.2, 6.3")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.4, 6.3, 6.5, 6.2")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.4, 6.3")]             <- "6.3, 6.4"
x2[which(x2 =="6.3, 6.2")]             <- "6.2, 6.3"
x2[which(x2 =="6.4, 6.5, 6.2")]         <- "6.2, 6.4, 6.5"
x2[which(x2 =="6.4, 6.5, 6.3")]         <- "6.3, 6.4, 6.5"

Task2.tab  <- data.frame(Tasks = x2, 
                        SW.ID = rep(data2$SW.ID)
)

## REGION ######################

# BE CAREFULL unique SW6 and area (use data2)

#Region.table <- as.data.frame(table(data2$Region))
#Region.table <- Region.table[order(-Region.table$Freq),]
#problem <- subset(data, data$Region %in% c("NE Atlantic", "CS-Western Waters; Mediterranenan - non CS"))

# Break into broad categories (Europe, North America, South America, Asia, Africa, Australia)

x1  <- data2$Continent

Region.tab <- data.frame(Region = x1, 
                         SW.ID = data2$SW.ID)

## POPULATION STRUCTURE ################
# split strings
x  <- tstrsplit(data2$Population.str , split="_")
x1 <- unlist(x)
#x1 <- as.data.frame(x)

x1[which(x1 %in% str_subset(x1,'iomass'))]      <- "Biomass aggregated"
x1[which(x1 %in% str_subset(x1,'stage') | x1 %in% str_subset(x1,'Stage'))]          <- "Stage structured"
x1[which(x1 %in% str_subset(x1,'size')| x1 %in% str_subset(x1,'Size'))]             <- "Size structured"
x1[which(x1 %in% str_subset(x1,'length') | x1 %in% str_subset(x1,'Length') | x1 %in% str_subset(x1,'lenght'))]        <- "Size structured"
x1[which(x1 %in% str_subset(x1,'sex') | x1 %in% str_subset(x1,'Sex'))]              <- "Sex structured"
x1[which(x1 %in% str_subset(x1,'abundance') | x1 %in% str_subset(x1,'Abundance'))]  <- "Abundance aggregated"
x1[which(x1 %in% str_subset(x1,'ndividual') )]                                      <- "Individual based"
x1[which(x1 =='age structured' | x1 =='age' | x1 %in% str_subset(x1,'Age') | x1 == 'age structure')]  <- "Age structured"
x1[which(x1 %in% str_subset(x1,'age structured for') | x1 == 'age structured (rest of species)')]   <- "Age structured"

x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"

x1[which(x1 %in% str_subset(x1, "Not clear"))]       <- "Not specified"
x1[which(x1 %in% str_subset(x1, "not clear"))]       <- "Not specified"
x1[which(x1 %in% str_subset(x1, "CPUE"))]            <- "Not specified"
x1[which(x1 %in% str_subset(x1, "dynamics model"))]  <- "Not specified"
x1[which(x1 %in% str_subset(x1, "Not specified"))]   <- "Not specified"
x1[which(x1 %in% str_subset(x1, "not specified"))]   <- "Not specified"
x1[which(x1 %in% str_subset(x1, "No specified"))]    <- "Not specified"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]    <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "not relevant"))]    <- "Not relevant"
x1[which(x1 =="structured")]    <- "Not specified"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Population.tab <- data.frame(Population.structure = x6, 
                             SW.ID = rep(data2$SW.ID))

## MODEL name ##########################
# split strings
x  <- tstrsplit(data2$Model.name.abbreviation , split="_")
x1 <- unlist(x)

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"   # take care here to not remove other models??

# Not specified
x1[which(x1 == "not specified")]   <- "Not specified"

# No model
x1[which(x1 == "none"|x1 == "Not relevant" |x1 == "NOT RELEVANT" |x1 == "None"  )]   <- "No model"
x1[which(x1 == "none"|x1 %in% str_subset(x1,"no model") |x1 == "NOT RELEVANT" |x1 == "None"  )]       <- "No model"

# Other corrections
x1[which(x1 == "Ecospace" | x1 == "ecospace")]    <- "ECOSPACE"
x1[which(x1 == "ISIS-Fish" | x1 == "ISIS-fish")]  <- "ISIS-FISH"
x1[which(x1 %in% str_subset(x1, "ATLANTIS")| x1=='Atlantis')]     <- "ATLANTIS"
x1[which(x1 %in% str_subset(x1, "OSMOSE"))]       <- "OSMOSE"
x1[which(x1 %in% str_subset(x1, "Bioeconomic model"))]     <- "Other"
x1[which(x1 %in% str_subset(x1, "othe"))]         <- "Other"
x1[which(x1 %in% str_subset(x1, "SS-DBEM"))]      <- "SS-DBEM"
x1[which(x1 %in% str_subset(x1, "Fishrent"))]     <- "FISHRENT"
x1[which(x1 == "ELFSim")]                         <- "ELFSIM"
x1[which(x1 == "ROMs")]                           <- "ROMS"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Model.tab <- data.frame(Model = x6, 
                             SW.ID = rep(data2$SW.ID))

## MODEL name 1 (remove Other for models with specific name) ##########################
# split strings
x  <- tstrsplit(data2$Model.name , split="_")
x1 <- unlist(x)

# Other / Not specified
x1[which(x1 == "not specified")]   <- "Not specified"
x1[which(x1 == "Other (specify)")] <- "Not specified"
x1[which(x1 == "Other (no name)")]  <- "Not specified"   # take care here to not remove other models??
x1[which(x1 == "Other (no name given)")]  <- "Not specified" 
x1[which(x1 == "Other (No name given)")]  <- "Not specified" 
x1[which(x1 == "Other (No name specified)")]  <- "Not specified" 
x1[which(x1 == "Other (noname)")]  <- "Not specified" 
x1[which(x1 == "Other")] <- "Not specified"

# No model
x1[which(x1 == "none"|x1 == "Not relevant" |x1 == "NOT RELEVANT" |x1 == "None"  )]   <- "No model"
x1[which(x1 == "none"|x1 %in% str_subset(x1,"no model") |x1 == "NOT RELEVANT" |x1 == "None"  )]       <- "No model"
x1[which(x1 == "Other (No model)")] <- "No model"

# Other corrections
x1[which(x1 == "Ecospace" | x1 == "ecospace")]    <- "ECOSPACE"
x1[which(x1 == "ISIS-Fish" | x1 == "ISIS-fish")]  <- "ISIS-FISH"
x1[which(x1 %in% str_subset(x1, "ATLANTIS")| x1=='Atlantis')]     <- "ATLANTIS"
x1[which(x1 %in% str_subset(x1, "OSMOSE"))]       <- "OSMOSE"
#x1[which(x1 %in% str_subset(x1, "Bioeconomic model"))]     <- "Other"
#x1[which(x1 %in% str_subset(x1, "othe"))]         <- "Other"
x1[which(x1 %in% str_subset(x1, "SS-DBEM"))]      <- "SS-DBEM"
x1[which(x1 %in% str_subset(x1, "Fishrent"))]     <- "FISHRENT"
x1[which(x1 == "ELFSim")]                         <- "ELFSIM"
x1[which(x1 == "ROMs")]                           <- "ROMS"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Model.full.tab <- data.frame(Model.name = x6, 
                        SW.ID = rep(data2$SW.ID))

## BIOLOGICAL COMPONENT #################
# split strings
x  <- tstrsplit(data2$Biological.component , split="_")
x1 <- unlist(x)

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1== "Mixed stock")]    <- "Other"
x1[which(x1== "Multi-stock")]    <- "Other"

# Not relevant
x1[which(x1 %in% str_subset(x1, "not relevant"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Not specified"

# single stock
x1[which(x1== "single stock")]   <- "Single stock"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Bio.tab <- data.frame(Biological.component = x6, 
                        SW.ID = rep(data2$SW.ID))


## INDICATORS TYPE #####################

# split strings
x1  <- data$Indicators.type

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Not specified"

# Not relevant
x1[which(x1=="NOT RELEVANT")]   <- "Not relevant"
x1[which(x1 == "biological")]   <- "Biological"
x1[which(x1 == "Economic ")]    <- "Economic"

Indicator.tab <- data.frame(Indicator.type = x1, 
                        SW.ID = rep(data$SW.ID))

## INDICATORS NAME #############

# split strings
x   <- tstrsplit(data$Indicators.name , split="_")
x1  <- unlist(x)

# Unlist and then combine again!!
# corrections (biomass, abundance, catch)
x1[which(x1=="biomass" | x1=="stock biomass" | x1=="biomass level" |x1=="Total biomass" | x1=="total biomass" | x1%in% str_subset(x1,'Biomass of target') | x1=='Biomass level' | x1=='mean biomass')]     <- "Biomass"
x1[which(x1=="biomasses" | x1=="Stock biomass" | x1 == "Population biomass" | x1=='biomasss' | x1=='biomass of stocks' | x1%in% str_subset(x1,'Biomass of all') | x1=='Biomass ' | x1=='B' | x1=='Biomass')]               <- "Biomass"
x1[which(x1 %in% str_subset(x1, "F/F")| x1 %in% str_subset(x1, "F rel"))]       <- "F/Fmsy"
x1[which(x1 %in% str_subset(x1, "SPR")| x1 %in% str_subset(x1, "per recrui"))]   <- "SPR"

x1[which(x1 %in% str_subset(x1, "abundance") | x1 %in% str_subset(x1, "Abundance") |  x1 %in% str_subset(x1, "adundance") | x1 %in% str_subset(x1,"stock size") | x1 %in% str_subset(x1,"Stock size") | x1 %in% str_subset(x1,"opulation") ) ]       <- "Abundance"
x1[which(x1 %in% str_subset(x1, "MSY")| x1 %in% str_subset(x1, "sustainable yield"))]       <- "MSY"
x1[which(x1=="Yearly catch" | x1=="yield" | x1=="yields" | x1=="Total catch" | x1=="total catch" | x1=="catches" | x1=="catch level" )]             <- "Catch"
x1[which(x1=="Landings" | x1=="landings" | x1=="total catches" | x1=="total landings" | x1=="Yield" | x1=="Catches" | x1=="Catch by species")]          <- "Catch"
x1[which(x1 %in% str_subset(x1, "harvest") | x1 %in% str_subset(x1, "Harvest") | x1== "catch")]   <- "Catch"

x1[which(x1 %in% str_subset(x1, "spawning stock") | x1=='spawning SSB' | x1 %in% str_subset(x1, "SSB"))]   <- "SSB"
x1[which(x1 %in% str_subset(x1, "Spawning stock"))]   <- "SSB"
x1[which(x1 == "Spawning SSB" | x1 == "spawning SSB")]                   <- "SSB"
x1[which(x1 %in% str_subset(x1, "fishing mortality") | x1 %in% str_subset(x1, "Fishing mortality") | x1=="F")]        <- "Fishing mortality"

x1[which(x1=="recruitment" | x1=="recruits" | x1=="Recruits" | x1=="recruitment regimes" | x1 %in% str_subset(x1, "recrutis"))]              <- "Recruitment"
x1[which(x1 %in% str_subset(x1, "ycatch") | x1 %in% str_subset(x1, "by-catch") | x1=="By-catch")]      <- "Bycatch"
x1[which(x1 %in% str_subset(x1, "discard") | x1 %in% str_subset(x1, "Discard"))]                        <- "Discards"
x1[which(x1 %in% str_subset(x1, "relative biomass") | x1 %in% str_subset(x1, "SSB vs") | x1 %in% str_subset(x1, "SSB/") | x1 %in% str_subset(x1, "relative to status") | x1 %in% str_subset(x1, "relative to target") | x1 %in% str_subset(x1, "iomass/car"))]   <- "Relative biomass"
x1[which(x1 %in% str_subset(x1, "relative biomass") | x1 %in% str_subset(x1, "SSB vs") | x1 %in% str_subset(x1, "SSB/") )]   <- "Relative biomass"
x1[which(x1 %in% str_subset(x1, "relative abundance"))] <- "Relative abundance"

x1[which(x1 %in% str_subset(x1, "Multiple") | x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Mutiple"))]   <- "Multiple"
x1[which(x1 %in% str_subset(x1, "mortality") | x1 %in% str_subset(x1, "Mortality") | x1=='Fishing morality')] <- "Mortality"
x1[which(x1 == "Multispecies Maximum Sustainable Yield")]                    <- "MMSY"
x1[which(x1 %in% str_subset(x1, "exploitation") | x1 %in% str_subset(x1, "Exploitation")  | x1 %in% str_subset(x1, "explotation"))]   <- "Expoitation rate"

x1[which(x1 %in% str_subset(x1, "ffort"))]                     <- "Effort"
x1[which(x1 %in% str_subset(x1, "overfis"))]                   <- "Overfishing"
x1[which(x1 %in% str_subset(x1, "Depletion") | x1 %in% str_subset(x1, "depletion"))] <- 'Depletion level'

x1[which(x1 =="Yield" | x1 == "yield" | x1 == "catch" | x1=="Catch" | x1=="total catch" | x1=="Total catch" | x1=="total annual catch" | x1== "Annual catch" | x1== " catch")]   <- "Yield"
x1[which(x1 %in% str_subset(x1, "harvest") | x1 %in% str_subset(x1, "Harvest") | x1== "landings"  | x1== "Landings" | x1== "catches" | x1== "Catches" | x1== "catch levels")]                      <- 'Yield'
x1[which(x1 %in% str_subset(x1, "Cost") | x1 %in% str_subset(x1, "costs") | x1 %in% str_subset(x1, "cost") | x1 %in% str_subset(x1, "Costs"))]                 <- "Costs"
x1[which(x1 %in% str_subset(x1, "revenue") | x1 %in% str_subset(x1, "Revenue") | x1 %in% str_subset(x1, "Revenues") | x1 %in% str_subset(x1, "revenues"))]     <- "Revenues"
x1[which(x1 %in% str_subset(x1, "NPV") | x1 %in% str_subset(x1, "net present value") | x1 %in% str_subset(x1, "Net Present Value") )]                          <- "NPV"
x1[which(x1 %in% str_subset(x1, "rent") | x1 %in% str_subset(x1, "Rent"))]       <- "Rent"
x1[which(x1 %in% str_subset(x1, "vessel") | x1 %in% str_subset(x1, "boat"))]     <- "Number of vessels"

x1[which(x1 %in% str_subset(x1, "Landed value") | x1 %in% str_subset(x1, "Catch value") | x1 %in% str_subset(x1, "catch value"))]         <- "Catch value"
x1[which(x1=="value of catch" | x1=="value of landings"  | x1=="Landing value" | x1=="landed value")]                                     <- "Catch value"
x1[which(x1 %in% str_subset(x1, "Income") | x1 %in% str_subset(x1, "income"))]                                                            <- "Income"
x1[which(x1 %in% str_subset(x1, "gross value") | x1 %in% str_subset(x1, "Gross Value") | x1 %in% str_subset(x1, "Gross value") | x1=="GAV" | x1=="value added")]     <- "GVA"
x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") | x1=="Mutiple")]      <- "Multiple"
x1[which(x1 %in% str_subset(x1, "MEY") )]     <- "MEY"
x1[which(x1 %in% str_subset(x1, "rice") )]     <- "Prices"
x1[which(x1 %in% str_subset(x1, "ffort") )]  <- "Fishing effort"
x1[which(x1 %in% str_subset(x1, "profit") | x1 %in% str_subset(x1, "Profit"))]     <- "Profit"

x1[which(x1 %in% str_subset(x1, "consumer surplus") )]         <- "Consumer surplus"
x1[which(x1 %in% str_subset(x1, "profitability") )]            <- "Profitability"

x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") )]    <- "Multiple"
x1[which(x1 %in% str_subset(x1, "catch") | x1 %in% str_subset(x1, "Catch") )]          <- "Catch"
x1[which(x1 %in% str_subset(x1, "mployment") | x1 %in% str_subset(x1, "mployement") | x1=="unemployment"  | x1=="labour" | x1=="Labour" | x1=="Jobs" |x1 %in% str_subset(x1, "jobs"))]    <- "Employment"
x1[which(x1 %in% str_subset(x1, "wage") | x1 %in% str_subset(x1, "salaries") | x1 =="crew remuneration")]    <- "Salaries"
x1[which(x1 %in% str_subset(x1, "FTE") )]    <- "FTE"
x1[which(x1 %in% str_subset(x1, "social benefit") | x1 %in% str_subset(x1, "Social benefit") | x1=='?benefits')]    <- "Social benefits"
x1[which(x1 %in% str_subset(x1, "number of vessels") | x1=='capacity')]    <- "Fleet size"
x1[which(x1 %in% str_subset(x1, "ncome") )]    <- "Income"
x1[which(x1 %in% str_subset(x1, "elfare") | x1 %in% str_subset(x1, "ellbeing") )]    <- "Community welfare"

x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") )]    <- "Multiple"
x1[which(x1 %in% str_subset(x1, "eystonene") )]          <- "Keystoneness"
x1[which(x1 %in% str_subset(x1, "rophic level of community") | x1 %in% str_subset(x1, "rophic level of the community") | x1 =="trophic level of the system" | x1 =="trophic level" | x1 =="Trophic level" | x1 =="mean trophic level")]    <- "Trophic level of community"
x1[which(x1 %in% str_subset(x1, "rophic level of catch") | x1 %in% str_subset(x1, "rophic level of the catch") | x1 =="catch trophic level")]    <- "Trophic level of catch"
x1[which(x1 %in% str_subset(x1, "Finn") | x1 %in% str_subset(x1, "finn") )]    <- "Finn's cycling index"
x1[which(x1 %in% str_subset(x1, "ishing in balance") )]    <- "Fishing in Balance (FIB) index"
x1[which(x1 %in% str_subset(x1, "ishing mortality") )]    <- "Fishing mortality"
x1[which(x1 %in% str_subset(x1, "Kempton") )]             <- "Kemptons Q"
x1[which(x1 %in% str_subset(x1, "arge fish index") | x1 %in% str_subset(x1, "arge fish indicator") )]    <- "Large fish indicator"
x1[which(x1 %in% str_subset(x1, "arine trophic index") )]             <- "Marine trophic index"

x1[which(x1=='Mixed Trophic Impact')]    <- "Mixed trophic impact"
x1[which(x1=='not specified')]           <- "Not specified"
x1[which(x1 %in% str_subset(x1, "Pelagic/demersal") | x1 %in% str_subset(x1, "demersal fish biomass to pelagic") | x1 %in% str_subset(x1, "total demersal to total pelagic") | x1 %in% str_subset(x1, "pelagic vs benthic")  | x1 %in% str_subset(x1, "Pelagic:Demersal"))]    <- "Pelagic over demersal biomass"
x1[which(x1 %in% str_subset(x1, "biomass to primary") | x1 %in% str_subset(x1, "biomass to total primary")  | x1 %in% str_subset(x1, "biomass to primary production" ) | x1 %in% str_subset(x1, "biomass relative to primary" ))]    <- "Total biomass to primary production"
x1[which(x1 %in% str_subset(x1, "hannon") | x1 =="species diversity" | x1 =="species biodiversity") ]    <- "Shannon diversity"
x1[which(x1 %in% str_subset(x1, "lope of the community") )]    <- "Slope of community size spectrum"
x1[which(x1== "total biomass" |x1== "total biomass" |x1== "total system biomass"| x1== "Total system biomass" | x1== "total population biomass" | x1== "Biomass" )]    <- "Total biomass"
x1[which(x1== "total catch" |x1== "total catch" |x1== "total system catch"| x1== "Total system catch" )]    <- "Total catch"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Indicator.name.tab <- data.frame(indicator = x6, 
                                SW.ID = rep(data$SW.ID))

## MANAGEMENT MEASURES CONSIDERED #############

# split strings
x  <- tstrsplit(data2$Management.meas , split="_")
x1 <- unlist(x)

# Corrections
x1[which(x1 %in% str_subset(x1, "not relevant"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1=="No management measures")]            <- "Not relevant"

x1[which(x1 %in% str_subset(x1, "nput"))]            <- "Input control"
x1[which(x1 %in% str_subset(x1, "ffort"))]           <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing press"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing rate"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing Seas"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "harvest time"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "utput"))]      <- "Output control"
x1[which(x1 %in% str_subset(x1, "ITQ"))]        <- "Output control"
x1[which(x1 %in% str_subset(x1, "patial"))]     <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "losure"))]     <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "no take"))]    <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "echnical"))]   <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "electivity"))] <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "gear"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "Gear"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "length"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "esh size"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "nhance"))]     <- "Enhancement study"
x1[which(x1 %in% str_subset(x1, "nutrient"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "nitrogen"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "trophication"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "Water quality"))]     <- "Water nutrient control"

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "conservation level"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "SQ"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Sex"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Promotion"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "morphology"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "efficiency"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Other"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Man.tab <- data.frame(Management.type = x6, 
                      SW.ID = rep(data2$SW.ID))

## USED IN MANAGEMENT ################# 

x  <- tstrsplit(data2$Used.in.man, split="_")
x1 <- unlist(x)

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 =="no")]   <- "No"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)
x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

UsedMan.tab <- data.frame(Used.in.management = x6, 
                      SW.ID = rep(data2$SW.ID))

## IMPLEMENTED IN SOFT ################ 
x1  <- data2$Implemented.in.soft

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 =="no")]   <- "No"
x1[which(x1 =="yes")]  <- "Yes"
x1[which(x1 =="yes ")]  <- "Yes"

Soft.tab <- data.frame(Software = x1, 
                          SW.ID = rep(data2$SW.ID))

## ENVIRONMENTAL VARS AND PROCESS ################# (YES/NO)

x1  <- data2$Environmental.vars

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "Yes"))]        <- "Yes"
x1[which(x1 =="no")]    <- "No"
x1[which(x1 =="yes")]   <- "Yes"
x1[which(x1 =="NOT RELEVANT")]  <- "Not relevant"
x1[which(x1 =="not relevant")]  <- "Not relevant"

Env.tab <- data.frame(Environmental.vars = x1, 
                       SW.ID = rep(data2$SW.ID))

# Process ###########################

# split to correct and combine again
x  <- tstrsplit(data2$Process, split="_")
x1 <- unlist(x)

#x1 <- data2$Process

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "various") | x1 %in% str_subset(x1, "ultiple") | x1 %in% str_subset(x1, "many"))]        <- "Multiple"
x1[which(x1 %in% str_subset(x1, "rowth"))]       <- "Growth"
x1[which(x1 %in% str_subset(x1, "ortality"))]    <- "Mortality"
x1[which(x1 %in% str_subset(x1, "ecruitment") |x1 %in% str_subset(x1, "recru"))]  <- "Recruitment"
x1[which(x1 %in% str_subset(x1, "atchability"))] <- "Catchability"
x1[which(x1 %in% str_subset(x1, "urviva"))]      <- "Survival"
x1[which(x1 %in% str_subset(x1, "istribution"))]      <- "Distribution"
x1[which(x1 %in% str_subset(x1, "eproduct"))]      <- "Reproduction"
x1[which(x1 %in% str_subset(x1, "roduct"))]      <- "Productivity"
x1[which(x1 %in% str_subset(x1, "predat"))]      <- "Predation"
x1[which(x1 %in% str_subset(x1, "ovement")| x1 %in% str_subset(x1, "igration") | x1=='Dispersal')]      <- "Migration"
x1[which(x1 %in% str_subset(x1, "abitat"))]      <- "Habitat"
x1[which(x1 %in% str_subset(x1, "ishing"))]      <- "Fishing effort"
x1[which(x1 =="consumption/biomass ratio")]      <- "consumption/biomass rate (Q/B)" 
x1[which(x1 %in% str_subset(x1, "dvection"))]      <- "Advection"
x1[which(x1 %in% str_subset(x1, "capacity"))]      <- "Carrying capacity"
x1[which(x1 %in% str_subset(x1, "biomass"))]      <- "Biomass"
x1[which(x1 %in% str_subset(x1, "tolerance"))]      <- "Tolerance"
x1[which(x1 %in% str_subset(x1, "feeding"))]      <- "Feeding"

# In many cases this was misundestood and env variables were inserted instead
x1[which(x1 %in% str_subset(x1, "emperature")| x1 %in% str_subset(x1, "alinity") | x1 %in% str_subset(x1, "itrogen")| x1 %in% str_subset(x1, "phorus") | x1=='rainfall' | x1 %in% str_subset(x1, "xygen"))]      <- "Not clear"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Process.tab <- data.frame(Process = x6, 
                          SW.ID = rep(data2$SW.ID))

## SPATIAL CONTEXT ####################

x  <- tstrsplit(data2$Spatial.cont, split="_")
x1 <- unlist(x)

#x1  <- data2$Spatial.cont

# Corrections
x1[which(x1 %in% str_subset(x1, "ot clear")| x1 %in% str_subset(x1,"ot specified") | x1 %in% str_subset(x1,"unclear")| x1 =="Yes")]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "other")| x1 %in% str_subset(x1, "Other") )]   <- "Other"
x1[which(x1 %in% str_subset(x1, "ot relevant"))]   <- "Not relevant"
x1[which(x1 =="NOT RELEVANT")]   <- "Not relevant"
x1[which(x1 =="No")]             <- "No spatial context"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

Spatial.tab <- data.frame(Spatial.context= x6, 
                          SW.ID = rep(data2$SW.ID))

## SCALES temporal ####################
x                    <- tstrsplit(data2$Scale...Temporal, split="_")
x1                   <- unlist(x)

#x1                  <- data2$Scale...Temporal

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "Equilibrium"))]    <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

ScaleTemp.tab <- data.frame(TemporalScale = x6, 
                        SW.ID = rep(data2$SW.ID))

## SCALE spatial #####################
x                    <- tstrsplit(data2$Scale...Spatial..m., split="_")
x1                   <- unlist(x)

#x1                    <- data2$Scale...Spatial..m.

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

ScaleSpat.tab <- data.frame(SpatialScale = x6, 
                            SW.ID = rep(data2$SW.ID))

## RESOLUTION spatial. ##################
x                    <- tstrsplit(data2$Resolution...Spatial..m., split="_")
x1                   <- unlist(x)

#x1                    <- data2$Resolution...Spatial..m.

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "ot specified"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "in degrees"))]     <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"
x1[which(x1=="?")]                                  <- "Not clear"
x1[which(x1 == "Not provided")]                     <- "Not clear"
x1[which(x1 == "not clear")]                        <- "Not clear"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

ResolSpat.tab <- data.frame(SpatialResolution = x6, 
                            SW.ID = rep(data2$SW.ID))

## RESOLUTION temporal ##################
x                    <- tstrsplit(data2$Resolution...Temporal, split="_")
x1                   <- unlist(x)

#x1                    <- data2$Resolution...Temporal

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "snapshot"))]       <- "Not relevant"
x1[which(x1 == "not clear")]                        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"

# combine again
x2 <- matrix(x1,  nrow=(length(x1)/length(x)), ncol=length(x), byrow=F)
x2 <- as.data.frame(x2)

x4 <- list()

for (i in 1: nrow(x2)){
  x4[[i]] <- x2[i,!is.na(x2[i,])]
}

x5 <- lapply(x4,  function (x) paste(x, collapse="_"))
x6 <- unlist(x5)

ResolTemp.tab <- data.frame(TemporalResolution = x6, 
                            SW.ID = rep(data2$SW.ID))

# QUALITY METHODS ##################

#---- quality spatial
x <- data2$Quality...Spatial..relative.1.3.

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"

QualSpat.tab <- data.frame(QualitySpatial = x, 
                           SW.ID = rep(data2$SW.ID))

#---- quality temporal
x <- data2$Quality...Temporal

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"


QualTemp.tab <- data.frame(QualityTemporal = x, 
                           SW.ID = rep(data2$SW.ID))

#---- quality method
x <- data2$Quality...Methods

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"

QMethod.tab <- data.frame(QualityMethod = x, 
                         SW.ID = data2$SW.ID)

####################################################
#  Output combined csv 
####################################################

tab1 <- data2[1:20]
tab2 <- merge(tab1, ScaleSpat.tab, by='SW.ID', all.y=T)
tab3 <- merge(tab2, ScaleTemp.tab, by='SW.ID', all.y=T)
tab4 <- merge(tab3, ResolSpat.tab, by='SW.ID', all.y=T)
tab5 <- merge(tab4, ResolTemp.tab, by='SW.ID', all.y=T)
tab6 <- merge(tab5, data2[,c(1,25)], by='SW.ID')
tab7 <- merge(tab6, QualSpat.tab, by='SW.ID', all.y=T)
tab8 <- merge(tab7, QualTemp.tab, by='SW.ID', all.y=T)
tab9 <- merge(tab8, QMethod.tab, by='SW.ID', all.y=T)
tab10 <- merge(tab9, data2[,c(1,29,30)], by='SW.ID', all.y=T)
tab11 <- merge(tab10, Task2.tab, by='SW.ID', all.y=T)
tab12 <- merge(tab11, Model.full.tab, by='SW.ID', all.y=T)    # add the tab with the full model name
tab13 <- merge(tab12, Bio.tab, by='SW.ID', all.y=T)
tab14 <- merge(tab13, data2[,c(1,35)], by='SW.ID', all.y=T)   # no corrections for this field (too much work)
tab15 <- merge(tab14, Population.tab, by='SW.ID', all.y=T)   
tab16 <- merge(tab15, data2[,c(1,37,38,39)], by='SW.ID', all.y=T)   
tab17 <- merge(tab16, Spatial.tab, by='SW.ID', all.y=T)
tab18 <- merge(tab17, data2[,c(1,41)], by='SW.ID', all.y=T)
tab19 <- merge(tab18, Process.tab, by='SW.ID', all.y=T)
tab20 <- merge(tab19, Man.tab, by='SW.ID', all.y=T)
tab21 <- merge(tab20, UsedMan.tab, by='SW.ID', all.y=T)
# now relate/combine Indicator names with corresponding Indicator type (these are related by both SW.ID and Indicator type)
# make sure the order of rows has not changed during the previous manipulation
temp <- cbind(Indicator.tab, Indicator.name=Indicator.name.tab$indicator)
tab22 <- merge(tab21, temp, by='SW.ID', all.y=T)
tab23 <- merge(tab22, Soft.tab, by='SW.ID', all.y=T)
tab24 <- merge(tab23, data2[,c(1,48,49)], by='SW.ID', all.y=T)

write.csv(tab24, 'WP6_SEAwise_DataExtraction_All_papers.csv', row.names=F)
