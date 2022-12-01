
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


## CORRECTIONS AND CLEANING  
##=========================
# check and correct wrong region choice 
test <- subset(data,data$Region == "Western Waters - non CS")
# test <- data[data$Region == "Western Waters - non CS",]
write.xlsx(test, file = "test.xls")
subset(data, data$SW.ID == "n3_sco.396")
data$Region[data$SW.ID == "w3_sco.379"] <- "CS - Western Waters"
data$Region[data$SW.ID == "w3_sco.380"] <- "CS - Western Waters"
data$Region[data$SW.ID == "m3_sco.160"] <- "Mediterranean - non CS"

test <- subset(data,data$Region == "NE-Atlantic")
write.xlsx(test, file = "test.xls")
data$Region[data$SW.ID == "n3_sco.396"] <- "CS - North Sea"

# check and clean Environmental drivers  
test <- subset(data,data$Environmental.Drivers. == "climate")
write.xlsx(test, file = "test.xls")
subset(data,data$SW.ID =="n3_sco.74")
data$Environmental.Drivers.[data$SW.ID == "b3_sco.190"] <- "pH" # checked only one obs
data$Environmental.Drivers.[data$SW.ID == "n3_sco.74"] <- "Prey availability" # checked, all need to be relocated into prey avail
data$Environmental.Drivers.[data$Environmental.Drivers. == "climate"] <-  "Climate indices"

#check carbon
test <- subset(data,data$Environmental.Drivers. == "carbon")
#Actually stable isotopes ratio
data$Environmental.Drivers.[data$Environmental.Drivers. == "carbon"] <-  "Other"

# Redistribute the Others
data$Environmental.Drivers.[data$SW.ID %in% c("n3_sco.492", "m3_sco.132", "w3_sco.313","b3_sco.100","n3_sco.52","b3_sco.398") & data$Environmental.Drivers. == "Other"] <- "Climate indices"
data$Environmental.Drivers.[data$SW.ID %in% c("n3_sco.582", "w3_sco.333", "w3_sco.333","n3_sco.377","b3_sco.777","w3_sco.240") & data$Environmental.Drivers. == "Other"] <-"Hydrodynamics & winds"
test <- subset(data[data$SW.ID %in% c("w3_sco.350","n3_sco.674","w3_sco.193", "n3_sco.396","b3_sco.737","b3_sco.744","b3_sco.772"),])
data$Environmental.Drivers.[data$SW.ID %in% c("w3_sco.350","n3_sco.674","w3_sco.193", "n3_sco.396","b3_sco.737","b3_sco.744","b3_sco.772") & data$Environmental.Drivers. == "Other"]<-"OUT"
data$Environmental.Drivers.[data$SW.ID %in% c("b3_sco.79")]<-"River inputs"
# remove observations that should not have been kept - Environmental drivers controlling the spatial distribution of a given life stage of a species rather than impact productivity
data <- subset(data, !(data$Environmental.Drivers. == "OUT"))
dim(subset(data, (data$Environmental.Drivers. == "OUT")))
test <- subset(data, data$Environmental.Drivers. == "Other")
write.xlsx(test, file = "test.xls")         ### STILL SOME CLEANING TO DO

# cleaning in the response
table(data$Direction.of.relationship)
data$Direction.of.relationship[data$Direction.of.relationship == "Mixed reponse"] <-  "Mixed response"
data$Direction.of.relationship[data$Direction.of.relationship == "mixed response"] <-  "Mixed response"
data$Direction.of.relationship[data$Direction.of.relationship == "negative"] <-  "Overall negative response"
data$Direction.of.relationship[data$Direction.of.relationship == "overall negative"] <-  "Overall negative response"
data$Direction.of.relationship[data$Direction.of.relationship == "Overall negative"] <-  "Overall negative response"
data$Direction.of.relationship[data$Direction.of.relationship == "overall negative response"] <-  "Overall negative response"
data$Direction.of.relationship[data$Direction.of.relationship == "no response"] <-  "No response"
data$Direction.of.relationship[data$Direction.of.relationship == "Positive"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "overall positive response"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "overall positive"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "Overall positive"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "positive relationship between potential fecundity and length, positive relationship between realized fecundity and temperature, negative with growth anomaly"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "Overall postitive response"] <-  "Overall positive response"
data$Direction.of.relationship[data$Direction.of.relationship == "Mixed-response"] <-  "Mixed response"
data$Direction.of.relationship[data$Direction.of.relationship == "None"] <-  "No response"
data$Direction.of.relationship[data$Direction.of.relationship == "Optimal-type response"] <-  "Optimal type response"
data$Direction.of.relationship[data$Direction.of.relationship == "NA"] <-  "No response"
subset(data,data$Direction.of.relationship=="N/A")
data$Direction.of.relationship[data$Direction.of.relationship == "N/A"] <-  "No response"
#data$Direction.of.relationship
data[163,]
test <- subset(data,is.na(data$Direction.of.relationship))
write.xlsx(test, file = "test.xls") # 18 lines removed - no actual relationships described

data <- subset(data,!(is.na(data$Direction.of.relationship)))

length(unique(data$SW.ID))


# remain 506 papers and 1252 observations
################################################
#-----------------------------------------------
## Save the final database
#-----------------------------------------------

save(data, file=paste0(scriptsandobjects,"finaldataextracted.Rdata"))

