#####################################################################################################################-
#####################################################################################################################-
#
#     Script to create task-specific output for SEAwise task 4.1
#     Step 4. Detailed look at tasks
#
#     By Karin van der Reijden & Esther Beukhof
#     April 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())
#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#

# library(data.table)
# library(RColorBrewer)
# library(raster)
# library(plotrix)
# library(sf)
library(viridis)
library(ggplot2)
library(splitstackshape)

outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 1 and processed species file in step 3.
#  Figures are based on general plots produced in step 2.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(outPath, "data.rds"))
dat1                                  <- readRDS(file=paste0(outPath, "dat1.rds"))


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Split rows for papers labelled with multiple tasks
#-----------------------------------------------#

# Check rows
table(data$WP4.task, useNA = "always")

# Split if multiple tasks and multiply those rows
Tasks <- cSplit(data, "WP4.task", " _ ", "long")


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Task 4.2 Bycatch of PET species ----
#-----------------------------------------------#

#-----------------------------------------------#
# Barplot with ecosystem components impacted by catch
#-----------------------------------------------#

# Select papers with Pressure Type 'Catch and bycatch'
Catch                                 <- subset(data, Pressure.type %in% "Catch_and_bycatch")

EcoComp                               <- Catch[,.(NrPaps = length(unique(SW.ID))),
                                               by = c("Ecosystem.component_level1","Pressure_level")]
names(EcoComp)                        <- c("EcosysComp", "Pressure", "NrPaps")
EcoComp[is.na(EcoComp)]                 <- "Not specified"

Ecoorder                              <- data.frame(EcoComp = unique(Catch$Ecosystem.component_level1),
                                                    Sord    = c(1,3,2,7,8,6,4,5,9,10))
Ecoorder                              <- Ecoorder[order(Ecoorder$Sord),]
Pressorder                            <- data.frame(Pressure = c(unique(Catch$Pressure_level)),
                                                    Sord    = c(1,3,4,2))
Pressorder                            <- Pressorder[order(Pressorder$Sord),]

EcoComp2                              <- matrix(nrow=4, ncol=10, dimnames=list(Pressorder$Pressure,Ecoorder$EcoComp))

for(iRow in c(1:nrow(EcoComp))){
  subset                              <- EcoComp[iRow,]
  EcoComp2[subset$Pressure,subset$EcosysComp] <- subset$NrPaps
}
EcoComp2[is.na(EcoComp2)]             <- 0

tiff(paste0(outPath, "Task 4.2_EcoCatch.tiff"), width = 800, height = 700, res=100)
par(mar=c(10,5,5,0))
b <- barplot(EcoComp2, ylim=c(0,180), axes=F, names.arg=rep("", 10), width=1, xlim=c(0,13.2), col=viridis(4), main=NULL)
axis(2, at=seq(0,180,10), cex.axis=1.2, las=1, pos=0)
axis(1, at=b, colnames(EcoComp2), las=3)
axis(2, at=90, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
legend(x=8, y=170, cex=1.2, fill=viridis(4), legend=Pressorder$Pressure)
dev.off()


#-----------------------------------------------#
# Subset data to WP task 4.2
#-----------------------------------------------#

# First select all rows labelled with task 4.2
Task42                                <- subset(Tasks, WP4.task %in% "4.2")

length(unique(Task42$SW.ID)) #115 papers

# Check under which Pressure Type they fall
table(Task42$Pressure.type) #most indeed under 'Catch and bycatch' but alsoq quite some under 'Discarding'
table(Task42$Pressure_level, useNA = "always") #most indeed under 'Bycatch'

# Check whether there are other papers not labelled as 4.2 that involve bycatch?
TaskOther                             <- subset(Tasks, !WP4.task %in% "4.2" & Pressure_level %in% "Bycatch")

table(TaskOther$WP4.task) #quite some other papers looking at bycatch. 
# "None" most likely deals with bycatch of non-PET species.
# 4.3 probably  bycatch of benthos. 
# 4.4 are food web papers that study many things, potentially including bycatch of PET and non-PET species
table(TaskOther$Ecosystem.component_level1, TaskOther$WP4.task)

# Add papers with Pressure level 'Bycatch' and for elasmobranchs, marine mammals, seabirds and reptiles 
# also to 4.2 task data
Task42                                <- rbind(Task42, TaskOther[TaskOther$Ecosystem.component_level1 %in% 
                                                                 c("Fish_cartilaginous","Marine_mammals",
                                                                 "Reptiles","Seabirds"),])

#-----------------------------------------------#
# Provide taxa list for this task
#-----------------------------------------------#

# Subset dataset with species/taxonomic group per row (from Step 3)
Task42spec                            <- dat1[dat1$WP4.task %in% c("4.2","4.2 _ 4.3","4.2 _ 4.4","4.4 _ 4.2") |
                                                dat1$Pressure_level %in% "Bycatch" & dat1$Ecosystem.component_level1 %in%
                                                c("Fish_cartilaginous","Marine_mammals","Reptiles","Seabirds"),]

table(Task42spec$Ecosystem.component_level1, Task42spec$WP4.task) #check

# Unique taxa?
length(unique(Task42spec$Species.taxonomic.group.s.)) #150 - that's a lot
unique(Task42spec$Species.taxonomic.group.s.) #includes also clearly non-PET species (e.g. plaice? Asterias rubens?)
# Probably because some studies include both PET and non-PET species, yet those species should not have been reported
# then as bycatch, but as target or non-target. Other example: pressure 'Discarding' and task 4.2, while this should
# have been 'Catch and bycatch' & 'Non-target' and task 'None'.

# This taxa list and papers that reported them need further checking!

# What we could do for now is to take the PET list and check which ones on there are reported here by CS region.
PETlist                               <- openxlsx::read.xlsx("../T4.1/Screening/PET_list.xlsx", sheet=1)

## Which are on the PET list
PETlist$Scientific.name               <- tolower(PETlist$Scientific.name)
Task42spec$onPETlist                  <- with(Task42spec, ifelse(Species.taxonomic.group.s. %in% PETlist$Scientific.name,
                                                                 "yes","no"))
table(Task42spec$onPETlist)

## Subset to unique ones and add relevant data
specPET                               <- data.frame(Species.taxonomic.group.s. = unique(Task42spec$Species.taxonomic.group.s.[Task42spec$onPETlist %in% "yes"]))
specPET                               <- merge(specPET, Task42spec[,c("Species.taxonomic.group.s.","Region",
                                                                      "Ecosystem.component_level1")], 
                                               by = "Species.taxonomic.group.s.")
specPet                               <- specPET[!duplicated(specPET),]

## Adjust and shuffle around so that we get a nice table for in report
specPET$Species.taxonomic.group.s.    <- stringr::str_to_sentence(specPET$Species.taxonomic.group.s.)
specPET2                              <- data.frame(Species.taxonomic.group.s. = NA, Region = NA)
specList                              <- unique(specPET$Species.taxonomic.group.s.)

for(iSpecies in c(1:length(specList))){
  specPET3    <- subset(specPET, Species.taxonomic.group.s. %in% specList[iSpecies])
  specPET3$Ecosystem.component_level1 <- NULL
  
  if(length(unique(specPET3$Region)) == 1){
    specPET2  <- rbind(specPET2, specPET3[1,])
  } else{
    regions    <- sort(unique(specPET3$Region))
    specPET4 <- data.frame(Species.taxonomic.group.s. = specList[iSpecies],
                           Region = paste(regions, collapse = ", "))
    specPET2  <- rbind(specPET2, specPET4)
  }
}

specPET2                         <- specPET2[-1,] 
specPET2                         <- merge(specPET2, specPET[,c("Species.taxonomic.group.s.","Ecosystem.component_level1")],
                                          by="Species.taxonomic.group.s.", all.x=TRUE)
specPET2                         <- specPET2[!duplicated(specPET2),]
specPET2                         <- subset(specPET2, !(Species.taxonomic.group.s. %in% c("Raja clavata",
                                                                                       "Squalus acanthias") &
                                             Ecosystem.component_level1 %in% "Marine_mammals"))
specPET2                         <- specPET2[order(specPET2$Ecosystem.component_level1),]

write.csv(specPET2, paste0(outPath, "Task 4.2_reported PET species by region.csv"), row.names = FALSE)



#-----------------------------------------------#
# Barplot for ecosystem component (level 1) by Case Study region for Task 4.2
#-----------------------------------------------#

EcoComp                               <- Task42[, .(NrPaps = length(unique(SW.ID))),
                                              by = c("Region","Ecosystem.component_level1")]
EcoComp                               <- EcoComp[order(NrPaps),,]

EcoComp$CS                            <- with(EcoComp, ifelse(Region %in% c("CS - North Sea",
                                                                            "CS - Baltic Sea",
                                                                            "CS - Western Waters",
                                                                            "CS - Mediterranean"),"CS","non CS"))
EcoComp$Area                          <- with(EcoComp, ifelse(Region %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                              ifelse(Region %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                     ifelse(Region %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                            ifelse(Region %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", "Other")))))
EcoComp$Area                          <- factor(EcoComp$Area, levels = c("Mediterranean Sea","Western Waters","North Sea","Baltic Sea","Other"))

p <- ggplot(EcoComp, aes(NrPaps, Ecosystem.component_level1, fill=CS)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values = viridis(3)) +
  labs(x="Number of unique retained papers", y="Ecosystem component") +
  theme_bw() +
  # guides(x = guide_axis(angle = 90)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  facet_wrap(~Area)
print(p)
ggsave("Task 4.2_EcoRegion.tiff", p, path=outPath)


#-----------------------------------------------#
# Barplot for Response Variable for Task 4.2
#-----------------------------------------------#

ResVar                                <- Task42[, .(NrPaps = length(unique(SW.ID))),
                                                by = c("Response.variable_category","Ecosystem.component_level1")]
names(ResVar)                         <- c("RespVar", "EcosysComp", "NrPaps")

Resorder                              <- data.frame(RespVar = unique(Task42$Response.variable_category),
                                                    Sord    = c(1,4,2,9,5,6,3,7,8,10))
Resorder                              <- Resorder[order(Resorder$Sord),]

Ecoorder                              <- data.frame(EcoComp = unique(Task42$Ecosystem.component_level1),
                                                    Sord    = c(5,2,3,4,1,6,7,8))
Ecoorder                              <- Ecoorder[order(Ecoorder$Sord),]

ResVar2                               <- matrix(nrow=8, ncol=10, dimnames=list(Ecoorder$EcoComp,Resorder$RespVar))

for(iRow in c(1:nrow(ResVar))){
  subset                              <- ResVar[iRow,]
  ResVar2[subset$EcosysComp,subset$RespVar] <- subset$NrPaps
}
ResVar2[is.na(ResVar2)]             <- 0

tiff(paste0(outPath, "Task 4.2_ResVarEco.tiff"), width = 800, height = 800, res=100)
par(mar=c(13,5,5,0))
b <- barplot(ResVar2, ylim=c(0,70), axes=F, names.arg=rep("", 10), width=1, xlim=c(0,13.2), col=viridis(8), main=NULL)
axis(2, at=seq(0,70,10), cex.axis=1.2, las=1, pos=0)
axis(1, at=b, colnames(ResVar2), las=3)
axis(2, at=90, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
legend(x=9, y=70, cex=1.2, fill=viridis(8), legend=Ecoorder$EcoComp)
dev.off()

