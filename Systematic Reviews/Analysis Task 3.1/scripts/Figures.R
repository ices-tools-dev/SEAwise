
#####################################################################################################################
#####################################################################################################################
##
### This file reads the database from the data extraction and produces figures for the report 
##
##   By Karin van der Reijden and Marie Savina
##    March 2022
##
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

library(wesanderson)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggraph)
library(igraph)
library(tidyverse)
library(treemap)
library(plyr)

setwd("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/scripts/")
outPath <- "C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/figures/"


################################################
#-----------------------------------------------
# Load the data
#-----------------------------------------------

load("finaldataextracted_2.Rdata")

data <- data2
Articles <- unique(data$SW.ID)

colbarplot <- viridis(n=10)

################################################
#-----------------------------------------------
# Produce barplots by (articles x elements)
#-----------------------------------------------

## YEAR
Year1 <- ddply(data, .(SW.ID, Year), nrow)  # = to number of unique paper (should be)
#data.frame(duplicated(Year1$SW.ID))
Year <- ddply(Year1, .(Year), nrow)
#data[data$SW.ID == "n3_sco.667",]

tiff(paste0(outPath, "Year.tiff"), width=1200, height=700, res=100)
par(mar=c(2,2,2,2))
b                                     <- barplot(Year$V1, horiz=FALSE, axes=F, ylim=c(0,30),col=colbarplot[6])
box()
axis(1, at=b, labels=Year$Year, las=1, cex.axis=0.8)
axis(2, at= seq(0,30, 10), labels=seq(0, 30, 10), cex.axis=0.8)
axis(1, at=90, tick=F, line=2, label="Number of papers", cex.axis=1)
dev.off()

## YEAR BY CATEGORIES OF DRIVERS
# produces a table per year and drivers 
load(file = "finaldataextracted_3.Rdata")
Year1_ED <- ddply(data3, .(SW.ID, Year, Driverscategory), nrow)
Year_ED <- ddply(Year1_ED, .(Year,Driverscategory), nrow)
  
rCols <- viridis(n=length(unique(data3$Driverscategory)))   

tiff(paste0(outPath,"Drivers per year.tiff"), width=1000, height=750, res=100)
par(mar=c(2,10,2,2))
ggplot(Year_ED, aes(fill=Driverscategory, y=V1, x=Year)) + 
  geom_bar(position='stack', stat='identity') +
  theme_minimal() + 
  labs(x='Year', y='Number of papers x Categories of drivers') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'), legend.text=element_text(size=8), legend.position = "bottom") +
  scale_fill_manual('Drivers', values=rCols) 
dev.off()

## COVERAGE
coverage_T1 <- ddply(data, .(SW.ID, Scale...Temporal), nrow)  # checked for duplicates - exist because of different timescales of different analysis in a same paper
# data.frame(duplicated(coverage_T$SW.ID))
#data[data$SW.ID == "w3_sco.365",]
coverage_T <- ddply(coverage_T1, .(Scale...Temporal), nrow)
coverage_T <- coverage_T[c(14,7,1,12,10,5,9,8,4,13,11,3,2,6),]

tiff(paste0(outPath, "Time Coverage.tiff"), width=1000, height=750, res=100)
par(mar=c(5,5,2,1))
b                                     <- barplot(coverage_T$V1, horiz=TRUE, axes=F, xlim=c(0,220), col=colbarplot[6])
box()
axis(2, at=b, labels=coverage_T$Scale...Temporal, las=1.5, cex.axis=0.8)
axis(1, at= seq(0,220, 20), labels=seq(0, 220, 20), cex.axis=0.8)
axis(1, at=90, tick=F, line=2, label="Temporal scale x articles", cex.axis=1)
text(x=110, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3, cex.axis=0.8)
text(x=110, y=2, paste0("Total number of papers x T.S : ", nrow(coverage_T1)), pos=3, cex.axis=0.8)
text(x=coverage_T$V1 + 8, y=b, coverage_T$V1, cex.axis=0.6)
dev.off()


## REGION
# produces a table per Region and unique paper (as some papers addresses several regions)
Regions1 <- ddply(data, .(SW.ID, Region), nrow)
Regions <- ddply(Regions1, .(Region), nrow)
Regions <- Regions[order(Regions$V1),,]

tiff(paste0(outPath,"Regions.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 12, 4, 8))
b                                     <- barplot(Regions$V1, horiz=TRUE, axes=F, xlim=c(0,200),col=colbarplot[6])
box()
axis(2, at=b, labels=Regions$Region, las=1.5, cex.axis=1)
axis(1, at= seq(0,200, 20), labels=seq(0, 200, 20), cex.axis=1)
text(x=80, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3)
text(x=80, y=2, paste0("Total number of papers x regions : ", nrow(Regions1)), pos=3)

text(x=Regions$V1 + 5, y=b, Regions$V1)
axis(1, at=90, tick=F, line=2, label="Number of papers x regions", cex.axis=1)
dev.off()

## Spatial coverage
coverage_S1 <- ddply(data, .(SW.ID, Scale...Spatial..m.), nrow)  # checked for duplicates - exist because of different timescales of different analysis in a same paper
# data.frame(duplicated(coverage_T$SW.ID))
#data[data$SW.ID == "w3_sco.365",]
coverage_S <- ddply(coverage_S1, .(Scale...Spatial..m.), nrow)
coverage_S <- coverage_S[c(12,2,7,4,9,6,11,3,8,5,10,1),]

tiff(paste0(outPath, "Space Coverage.tiff"), width=1000, height=750, res=100)
par(mar=c(5,6,2,1))
b                                     <- barplot(coverage_S$V1, horiz=TRUE, axes=F, xlim=c(0,350), col=colbarplot[6])
box()
axis(2, at=b, labels=coverage_S$Scale...Spatial..m., las=1.5, cex.axis=0.8)
axis(1, at= seq(0,350, 20), labels=seq(0, 350, 20), cex.axis=0.8)
axis(1, at=90, tick=F, line=2, label="Space scale x articles", cex.axis=1)
text(x=200, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3)
text(x=200, y=2, paste0("Total number of papers x regions : ", nrow(coverage_S1)), pos=3)
text(x=coverage_S$V1 + 6, y=b, coverage_S$V1)
dev.off()

## SPECIES
# produces a table per Region and unique paper (as some papers adresses several species)
Species1 <- ddply(data, .(SW.ID, Species), nrow)
Species <- ddply(Species1, .(Species), nrow)
Species <- Species[order(Species$V1),,]

tiff(paste0(outPath,"Species.tiff"), width=1000, height=750, res=100)
par(mar=c(2,10,2,2))
b                                     <- barplot(Species$V1, horiz=TRUE, axes=F, xlim=c(0,180),col=colbarplot[6])
box()
axis(2, at=b, labels=Species$Species, las=1.5, cex.axis=0.8)
axis(1, at= seq(0,200, 20), labels=seq(0, 200, 20), cex.axis=1)
text(x=80, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3)
text(x=80, y=2, paste0("Total number of papers x species : ", nrow(Species1)), pos=3)

text(x=Species$V1 + 4, y=b, Species$V1)
axis(1, at=90, tick=F, line=2, label="Number of papers x species", cex.axis=1)
dev.off()

## SPECIES per CS
# produces a table per Region and unique paper (as some papers adresses several species)
Species1 <- ddply(data, .(SW.ID, Species, Region), nrow)
Species <- ddply(Species1, .(Species,Region), nrow)
Species <- Species[order(Species$V1),,]

rCols <- viridis(n=13)   

tiff(paste0(outPath,"SpeciesperCS.tiff"), width=1000, height=750, res=100)
par(mar=c(2,10,2,2))
ggplot(Species, aes(fill=Region, y=V1, x=Species)) + 
  geom_bar(position='stack', stat='identity') +
  theme_minimal() + 
  labs(x='Species', y='Number of papers x Species x Region') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  scale_fill_manual('Regions', values=rCols) + coord_flip()
dev.off()

## PROCESS
# produces a table per process and unique paper (as some papers address several processes)
Process1 <- ddply(data, .(SW.ID, Process), nrow)
Process <- ddply(Process1, .(Process), nrow)
Process <- Process[order(Process$V1),,]

tiff(paste0(outPath,"Process.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 12, 4, 8))
b                                     <- barplot(Process$V1, horiz=TRUE, axes=F, xlim=c(0,140),col=colbarplot[6])
box()
axis(2, at=b, labels=Process$Process, las=1.5, cex.axis=1)
axis(1, at= seq(0,140, 20), labels=seq(0, 140, 20), cex.axis=1)
text(x=80, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3)
text(x=80, y=1, paste0("Total number of papers x process : ", nrow(Process1)), pos=3)

text(x=Process$V1 + 4, y=b, Process$V1)
axis(1, at=90, tick=F, line=2, label="Number of papers x process", cex.axis=1)
dev.off()

## DRIVERS
# produces a table per drivers and unique paper (as some papers address several drivers)
Drivers1 <- ddply(data, .(SW.ID, Environmental.Drivers.), nrow)
Drivers <- ddply(Drivers1, .(Environmental.Drivers.), nrow)
Drivers <- Drivers[order(Drivers$V1),,]

tiff(paste0(outPath,"Drivers.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 12, 4, 8))
b                                     <- barplot(Drivers$V1, horiz=TRUE, axes=F, xlim=c(0,220),col=colbarplot[6])
box()
axis(2, at=b, labels=Drivers$Environmental.Drivers., las=1.5, cex.axis=1)
axis(1, at= seq(0,200, 20), labels=seq(0, 200, 20), cex.axis=1)
text(x=100, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=3)
text(x=100, y=2, paste0("Total number of papers x drivers : ", nrow(Drivers1)), pos=3)

text(x=Drivers$V1 + 4, y=b, Drivers$V1)
axis(1, at=90, tick=F, line=2, label="Number of papers x drivers", cex.axis=1)
dev.off()

test <- subset(data, data$Environmental.Drivers. == "Other")

################################################
#-----------------------------------------------
# Produce heatmaps
#-----------------------------------------------


# Processes over driver 
processoverdriver <- table(data$Process,data$Environmental.Drivers.)
# heatmap(processoverdriver)
processoverdriver_2 <- as.data.frame(processoverdriver)
tiff(paste0(outPath,"Processoverdrivers.tiff"), width=1000, height=750, res=100)
ggplot(processoverdriver_2, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() + scale_fill_viridis() + theme(axis.text=element_text(size=8), axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# Species over regions
speciesoverCS <- as.data.frame(table(data$Region,data$Species))
table(data$Region,data$Species)

tiff(paste0(outPath,"Speciesoverregions.tiff"), width=1700, height=900, res=100)
ggplot(speciesoverCS, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() + scale_fill_viridis() + theme(axis.text=element_text(size=8), axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# Species over processes
speciesoverprocesses <- as.data.frame(table(data$Process,data$Species))
tiff(paste0(outPath,"Speciesoverprocesses.tiff"), width=1000, height=750, res=100)
ggplot(speciesoverprocesses, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() + scale_fill_viridis() + theme(axis.text=element_text(size=8), axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# Species over Drivers
speciesoverdrivers <- as.data.frame(table(data$Environmental.Drivers.,data$Species))
tiff(paste0(outPath,"Speciesoverdrivers.tiff"), width=1900, height=750, res=100)
ggplot(speciesoverdrivers, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() + scale_fill_viridis() + theme(axis.text=element_text(size=6), axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# Drivers over Regions
Driversoverregions <- as.data.frame(table(data$Region, data$Environmental.Drivers.))
tiff(paste0(outPath,"Driversoverregions.tiff"), width=1200, height=750, res=100)
ggplot(Driversoverregions, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() + scale_fill_viridis() + theme(axis.text=element_text(size=6), axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

## HIERARCHY

# Custom labels:
treemap(speciesoverCS_2, index=c("Var1","Var2"),     vSize="Freq", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)


################################################
#-----------------------------------------------
# Produce barplots by lines
#-----------------------------------------------

################################################
#-----------------------------------------------
# Produce various simple plots 
#-----------------------------------------------

dotchart(table(data$Species), col=colors)
pie(table(data$Species), col=colors)
packcircles(table(data$Species), col=colors)
barplot(table(data$Species))
pie(table(data$Region),col=colors)
dotchart(table(data$Species),col=colors)
dotchart(table(data$Environmental.Drivers.),col=colors)
pie(table(data$Environmental.Drivers.),col=colors)

pie(table(data$Process),col=colors)

################################################
#-----------------------------------------------
# Produce circle plots
#-----------------------------------------------

## Distribution of lines over species

colors =  wes_palette(name = "Zissou1", 5, type="discrete")

# Generate the layout
packing <- circleProgressiveLayout(table(data$Species), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(data$Species), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=Freq, label = Var1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()

## Distribution of lines over case studies

# Generate the layout
packing <- circleProgressiveLayout(table(data$Region), sizetype='area')
packing$radius <- 0.95*packing$radius
Region <- cbind(table(data$Region), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = Region, aes(x, y, size=Freq, label = Var1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()

## Distribution of lines over processes

# Generate the layout
packing <- circleProgressiveLayout(table(data$Process), sizetype='area')
packing$radius <- 0.95*packing$radius
Processes <- cbind(table(data$Process), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = Processes, aes(x, y, size=Freq, label = Var1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()

## Distribution of lines over drivers

# Generate the layout
packing <- circleProgressiveLayout(table(data$Environmental.Drivers.), sizetype='area')
packing$radius <- 0.95*packing$radius
Driver <- cbind(table(data$Environmental.Drivers.), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = Driver, aes(x, y, size=Freq, label = Var1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()



