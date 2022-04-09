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

#-----------------------------------------------
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 1 and processed species file in step 3.
#  Figures are based on general plots produced in step 2.
#-----------------------------------------------
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

# Select papers with Pressure Type 'Catch and bycatch
Catch                                 <- subset(data, Pressure.type %in% "Catch_and_bycatch")

EcoComp                               <- Catch[,.(NrPaps = length(unique(SW.ID))),
                                               by = c("Ecosystem.component_level1","Pressure_level")]
names(EcoComp)                        <- c("EcosysComp", "Pressure", "NrPaps")
EcoComp[is.na(EcoComp)]                 <- "Not specified"

Ecoorder                              <- data.frame(EcoComp = c(unique(Catch$Ecosystem.component_level1)),
                                                    Sord    = c(1,3,2,4,8,7,6,5,9,10))
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

tiff(paste0(outPath, "EcoCatch.tiff"), width = 800, height = 700, res=100)
par(mar=c(10,5,5,0))
b <- barplot(EcoComp2, ylim=c(0,170), axes=F, names.arg=rep("", 10), width=1, xlim=c(0,13.2), col=viridis(4), main=NULL)
axis(2, at=seq(0,170,10), cex.axis=1.2, las=1, pos=0)
axis(1, at=b, colnames(smallScale), las=3)
axis(2, at=90, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
legend(x=8, y=170, cex=1.2, fill=viridis(4), legend=Pressorder$Pressure)
dev.off()


#-----------------------------------------------#
# Subset data to WP task 4.2
#-----------------------------------------------#

# First select all rows labelled with task 4.2
Task42 <- subset(Tasks, WP4.task %in% "4.2")

length(unique(Task42$SW.ID)) #111 papers

# Check under which Pressure Type they fall
table(Task42$Pressure.type) #most indeed under 'Catch and bycatch' but alsoq quite some under 'Discarding'
table(Task42$Pressure_level, useNA = "always") #most indeed under 'Bycatch'

# Check whether there are other papers not labelled as 4.2 that involve bycatch?
TaskOther <- subset(Tasks, !WP4.task %in% "4.2" & Pressure_level %in% "Bycatch")

table(TaskOther$WP4.task)
table(TaskOther$Ecosystem.component_level1)
