
#####################################################################################################################
#####################################################################################################################
##
### This file reads the database from the data extraction and produces maps for the report 
##
##   By Karin van der Reijden adapted by Marie Savina
##    March 2022
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

library(data.table)
library(RColorBrewer)
library(raster)
library(plotrix)
library(sf)
library(viridis)
library(plyr)

setwd("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/scripts/")
outPath <- "C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/figures/"

################################################
#-----------------------------------------------
# Load the data
#-----------------------------------------------

load("finaldataextracted.Rdata")
load("SEAwise4.1_regions.Rdata")


#-----------------------------------------------
## Creating map with Region specific info
#-----------------------------------------------
Regions1 <- ddply(data, .(SW.ID, Region), nrow)
Regions <- ddply(Regions1, .(Region), nrow)
Regions <- Regions[order(Regions$V1),,]
RegCol                               <- data.frame(colcode = viridis(max(Regions$V1)),
                                                   value = c(1:max(Regions$V1)))
Regions$Colcode                      <- RegCol$colcode [match(Regions$V1, RegCol$value)]
SEAwise4.1_regions$colcode           <- Regions$Colcode [match(SEAwise4.1_regions$Region, Regions$Region)]
Centerpoints                         <- st_coordinates(st_centroid(SEAwise4.1_regions))
SEAwise4.1_regions$Xloc              <- Centerpoints[,1]
SEAwise4.1_regions$Yloc              <- Centerpoints[,2]
SEAwise4.1_regions$NrPaps            <- Regions$V1 [match(SEAwise4.1_regions$Region, Regions$Region)]
SEAwise4.1_regions$textcol           <- ifelse(SEAwise4.1_regions$NrPaps >50, "black", "white")

## Change some points to a better location
SEAwise4.1_regions$Xloc              <- ifelse(SEAwise4.1_regions$Region == "North Sea - non CS", 4280000,
                                               ifelse(SEAwise4.1_regions$Region == "Baltic Sea - non CS", 4800000, 
                                                      ifelse(SEAwise4.1_regions$Region == "CS - Mediterranean", 5059132, 
                                                             ifelse(SEAwise4.1_regions$Region == "CS - Baltic Sea", 4851499, SEAwise4.1_regions$Xloc))))
SEAwise4.1_regions$Yloc              <- ifelse(SEAwise4.1_regions$Region == "North Sea - non CS", 3885000,
                                               ifelse(SEAwise4.1_regions$Region == "Mediterranean - non CS", 1248183, 
                                                      ifelse(SEAwise4.1_regions$Region == "CS - Baltic Sea", 3728215, SEAwise4.1_regions$Yloc)))

## Create plot
tiff(paste0(outPath, "RegMap.tiff"), width = 1000, height = 1000, res = 100)
par(mar=c(1,1,1,1))
plot(st_geometry(SEAwise4.1_regions), col=SEAwise4.1_regions$colcode, border=F)
text(SEAwise4.1_regions$NrPaps, x= SEAwise4.1_regions$Xloc, y=SEAwise4.1_regions$Yloc, col=SEAwise4.1_regions$textcol, font=2)
text(x= 1E6, y=6E6, "Global studies:", font=3, cex=1.2)
text(x=2e6, y=6e6, paste0(subset(Regions, Region == "Global")$V1), font=2)
gradient.rect(xleft=0, xright=1E6, ytop=1.5e6, ybottom=1.25E6, col=RegCol$colcode, gradient="x")
text(x=0, y=1.18e6, "1")
text(x=1E6, y=1.18e6, max(Regions$V1))
text(x=0.9e6, y=1e6, "Number of papers x regions", font=4, cex=1.2)
dev.off()
