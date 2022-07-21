
#####################################################################################################################
#####################################################################################################################
##
### This file reads the outcomes of the data extraction, performs tests and clean the database 
##
#     By Marie Savina
#     March 2022
#
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

library(xlsx) 


setwd("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction")

scriptsandobjects <- "C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/scripts/"
  
################################################
#-----------------------------------------------
# Read in data
#-----------------------------------------------


readers <- c("Brown", "Carbonara", "Chust", "Depestele", "Einberg", "Fincham", "Garcia", "Halouani", "Ibaibarriaga","Melia", "missing", "Munschy", "Neuenfeldt", "O'Connor", "Ojaveer","Papantoniou","Petitgas", "Pierucci", "Politikos", "Savina", "Spedicato", "Sys", "Taboada", "Thuenen", "Tsagarakis", "Uhlmann", "Ustups", "Valavanis", "van_Deurs", "Vansteenbrugge", "Voss","woillez","zambonino")

people <- "Brown"
tab <- read.xlsx(file=paste0("3.1_dataextraction_",people,".xlsx"),startRow = 2, header = TRUE, sheetIndex = 1)
# colnames(tab)
tab$reader <- people

for(people in readers[2:33]) {
  print(people)
  tab1 <- read.xlsx(file=paste0("3.1_dataextraction_",people,".xlsx"),startRow = 2, header = TRUE, sheetIndex = 1)
  tab1$reader <- people
  tab <- rbind(tab,tab1)
}

# how many papers treated : 802 papers (out of 802)
treated_papers <- unique(tab$SW.ID)
# check who did how many papers
contributors <- tab[,c(1, 39)]
contributors <- contributors[!duplicated(contributors$SW.ID), ]
table(contributors$reader)

# Various tests
# tab[tab$SW.ID == "SW_b3_sco.102",]
# tab[279,]

# check the missing papers
# institute <- c("Ifremer","DTUAqua" ,"AZTI", "MI", "COISPA", "HCMR", "TI", "ILVO", "CEFAS", "CAU", "UTARTU", "BIOR", "POLIMI")
# people <- "Ifremer"
# todo <- read.xlsx(file=paste0("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/",people,"_paperstoread.xls"),sheetIndex = 1)
# todo <- todo[,1:18]
# todo2 <- read.xlsx(file=paste0("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/",people,"_paperstoread2.xls"),sheetIndex = 1)
# todo <- rbind(todo,todo2)
# for(people in institute[2:13]) {
#   print(people)
#   todo1 <- read.xlsx(file=paste0("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/",people,"_paperstoread.xls"),sheetIndex = 1)
#   todo2 <- read.xlsx(file=paste0("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/",people,"_paperstoread2.xls"),sheetIndex = 1)
#   todo <- rbind(todo,todo1,todo2)
# }
# save(todo, file="allpaperstoread.Rdata")
load(paste0(scriptsandobjects,"/allpaperstoread.Rdata"))
notdone <- todo[!(todo$SW_ID %in% treated_papers),]
# add <- treated_papers[!(treated_papers %in% todo$SW_ID)]

# test
# tab[tab$SW.ID == "b3_sco.581",]

# Making a list of unique papers
basis <- tab[,c(1, 18)]
basis <- basis[!duplicated(basis$SW.ID), ]

# how many retained :  516 papers
table(basis$Exclusion.Criteria)

# how many rejected: 286
excluded_2 <- subset(basis, !(basis$Exclusion.Criteria == "K"))

# space for various checking
# tab[tab$SW.ID == "n3_sco.715",]
# check <- excluded[duplicated(excluded$SW.ID), ]

################################################
#-----------------------------------------------
# Produce the retained paper database and clean it
#-----------------------------------------------

# Select the retained papers
data <- subset(tab, tab$Exclusion.Criteria == "K")
length(unique(data$SW.ID))

# spotting the mislabellings to clean the corresponding files

table(data$Species)
# problem <- subset(data, data$Species %in% c("adult", "adults", "early life stages (eggs, larvae)", "juveniles"))
# problem <- subset(data, data$Species %in% c("Cod", "Herring", "Sole", "Plaice", "Flounder", "Whiting"))
# problem <- subset(data, data$Species %in% c("Neogobius melanostomus "))
# problem <- subset(data, data$Species %in% c("Clupea harengus membras"))
problem <- subset(data, is.na(data$Species) )

table(data$Region)
ddply(data, .(Region), nrow)
problem <- subset(data, data$Region %in% c(""))
problem <- subset(data, is.na(data$Region) )


table(data$Process)
problem <- subset(data, data$Process %in% c("Growth rate")) 
#                 "Condition factor", "growth","density-dependence acting on recruitment", "diet", "Distribution")
problem <- subset(data, is.na(data$Process) )


table(data$Environmental.Drivers.)
problem <- subset(data, data$Environmental.Drivers. %in% c("temperature "))
problem <- subset(data, is.na(data$Environmental.Drivers.) )

table(data$Scale...Temporal)
table(data$Scale...Spatial..m.)

verif <- subset(data, data$Scale...Temporal %in% c("decadal", "Decadal","Decade", "decades", "four year", "Multidecadal", "Three year", "two-month", "two weeks", "two years", "week, two year data"))

verif <- subset(data, data$Scale...Spatial..m. %in% c(">100,001", "10,000","200000-500000", "50-101", "500-1000", "50000"))



################################################
#-----------------------------------------------
## Save the final database
#-----------------------------------------------

save(data, file=paste0(scriptsandobjects,"finaldataextracted.Rdata"))

################################################
#-----------------------------------------------
## Improve labelling consistency
#-----------------------------------------------

data2 <- data

table(data2$Process)
data2$Process[data2$Process == "Mortality"] <-  "Survival"
data2$Process[data2$Process == "Size"] <-  "Growth"
data2$Process[data2$Process == "Weight"] <-  "Growth"
data2$Process[data2$Process == "Fecundity"] <-  "Reproduction"
data2$Process[data2$Process == "Spawning"] <-  "Reproduction"
data2$Process[data2$Process == "Recruitment"] <-  "Reproduction"

table(data2$Environmental.Drivers.)
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "birds"] <-  "Birds"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "carbon"] <-  "Carbon"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "climate"] <-  "Climate"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "colour"] <-  "Colour"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "competition"] <-  "Competition"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "currents"] <-  "Hydrodynamics & winds"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "wind"] <-  "Hydrodynamics & winds"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "density dependence"] <-  "Density dependence"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "inflow"] <-  "River inputs"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "invertebrates"] <-  "Invertebrates"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "mammals"] <-  "Mammals"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "nutrients"] <-  "Eutrophication & Nutrients"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "eutrophication"] <-  "Eutrophication & Nutrients"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "oxygen"] <-  "Oxygen"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "particulate matter"] <-  "Particulate matter"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "phytoplankton"] <-  "Plankton"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "pollution"] <-  "Contaminants & Pollution"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "Contaminants"] <-  "Contaminants & Pollution"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "predator pressure"] <-  "Predator pressure"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "prey availability"] <-  "Prey availability"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "salinity"] <-  "Salinity"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "sound"] <-  "Sound"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "temperature"] <-  "Temperature"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "turbidity"] <-  "Turbidity"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "zooplankton"] <-  "Plankton"

verif <- subset(data, data$Environmental.Drivers. %in% c("colour", "mammals","particulate matter")) 

data2$Environmental.Drivers.[data2$Environmental.Drivers. == "Colour"] <-  "Plankton"
data2$Environmental.Drivers.[data2$Environmental.Drivers. == "Mammals"] <-  "Predator pressure"


data2$Region[data2$Region == "CS - Baltic Sea"] <-  "Baltic Sea - CS"
data2$Region[data2$Region == "CS - North Sea"] <-  "North Sea - CS"
data2$Region[data2$Region == "CS - Western Waters"] <-  "Western Waters - CS"
data2$Region[data2$Region == "CS - Mediterranean"] <-  "Mediterranean - CS"



save(data2, file=paste0(scriptsandobjects,"finaldataextracted_2.Rdata"))

###################################################
data3 <- data2
table(data3$Environmental.Drivers.)
data3$Driverscategory <- data3$Environmental.Drivers.
data3$Driverscategory[data3$Driverscategory == "birds"] <-  "PREDATOR"
data3$Driverscategory[data3$Driverscategory == "Predator pressure"] <-  "PREDATOR"
data3$Driverscategory[data3$Driverscategory == "Invertebrates"] <-  "FOOD"
data3$Driverscategory[data3$Driverscategory == "Plankton"] <-  "FOOD"
data3$Driverscategory[data3$Driverscategory == "Prey availability"] <-  "FOOD"
data3$Driverscategory[data3$Driverscategory == "Carbon"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Climate"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Hydrodynamics & winds"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Oxygen"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "pH"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "River inputs"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Salinity"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Turbidity"] <-  "CLIMATE & OCEANOGRAPHY"
data3$Driverscategory[data3$Driverscategory == "Competition"] <-  "COMPETITION & DENSITY DEPENDENCE"
data3$Driverscategory[data3$Driverscategory == "Contaminants & Pollution"] <-  "ABIOTIC ANTHROPIC"
data3$Driverscategory[data3$Driverscategory == "Eutrophication & Nutrients"] <-  "ABIOTIC ANTHROPIC"
data3$Driverscategory[data3$Driverscategory == "Sound"] <-  "ABIOTIC ANTHROPIC"
data3$Driverscategory[data3$Driverscategory == "Density dependence"] <-  "COMPETITION & DENSITY DEPENDENCE"
data3$Driverscategory[data3$Driverscategory == "Disease"] <-  "BIOTIC"
data3$Driverscategory[data3$Driverscategory == "Parasite"] <-  "BIOTIC"
data3$Driverscategory[data3$Driverscategory == "Temperature"] <-  "TEMPERATURE"

save(data3, file=paste0(scriptsandobjects,"finaldataextracted_3.Rdata"))









