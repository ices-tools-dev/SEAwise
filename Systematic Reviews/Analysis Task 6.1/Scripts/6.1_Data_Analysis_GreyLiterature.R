
#
##
### This file reads the outcomes of the data extraction and analyse it
##
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

tab <- read.csv(file=paste0(datPath,'WP6_SEAwise_DataExtraction.Template_Grey_literature.csv'),header = TRUE, na.strings=c("","NA"))
colnames(tab)

# rename columns 
colnames(tab) <- c(colnames(tab)[1:31], 'Biological.component', 'Species', 
                   "Population.str", "Fleet.comp", "Fleet.categ", "Economic.comp", 
                   "Spatial.cont", "Environmental.vars", "Process", "Management.meas",
                   "Used.in.man", "Indicators.type", 'Indicators.name', "Implemented.in.soft", 
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
data2 <- data[!duplicated(data[c("SW.ID", "Model.name.abbreviation") ] ), ]
# unique SW.ID and model combinations

# NOTE: data is the full database, data2 is reduced (excluding multiple lines for same SW6s)

####################################################################
######### CREATE the tables and do plots by field ##################
####################################################################


# YEAR PUBLISHED ###################

year <- data2$Year
year <- table(year)
plot(year, type='l', xlab='Publication year', ylab='Number of retained papers')
Year.tab   <- data.frame(year = data2$Year, SW.ID = data2$SW.ID)

# TASK #############################

# split strings
x  <- tstrsplit(data2$WP6.task , split="_")
x1 <- unlist(x)

# All referring to tasks 6.3_6.5

## REGION ######################

# BE CAREFULL unique SW6 and area (use data2)

data4 <- data[!duplicated(data[c("SW.ID", "Model.name.abbreviation", "Region..free.text.") ] ), ]

# Break into broad categories (Europe, North America, South America, Asia, Africa, Australia)

x1  <- data4$Region
x1[which(x1=="Soythern Adriatic")] <- 'South Adriatic'
x1[which(x1=="Northenr Adriatic")] <- 'North Adriatic'
x1[which(x1=="Lingurian, Tyrrhenian and Sardinia")] <- 'Lingurian, Tyrrhenian, Sardinia'
x1[which(x1=="Lingurian and NorthTyrrhenian ")] <- 'Lingurian, N Tyrrhenian'

Region.table <- as.data.frame(table(x1))
Region.table <- Region.table[order(-Region.table$Freq),]

Region.tab <- data.frame(region = x1, 
                         SW.ID = data4$SW.ID)
Region.tab <- subset(Region.tab, !is.na(region))

x1 <- x1[order(x1)]
# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_Area.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## POPULATION STRUCTURE ################

# All are age structured

## SPECIES #############################

x  <- tstrsplit(data4$Species , split="_")
x1 <- unlist(x)


x1[which(x1%in% str_subset(x1, "eep"))]       <- "Deep water rose shrimp"
x1[which(x1 %in% str_subset(x1, "mantis"))]   <- "Spottail mantis shrimp"
x1[which(x1 %in% str_subset(x1, "hake"))]     <- "European hake"
x1[which(x1 %in% str_subset(x1, "Hake"))]     <- "European hake"
x1[which(x1 %in% str_subset(x1, "and red shrimp"))]   <- "Blue and red shrimp"
x1[which(x1 %in% str_subset(x1, "lobster"))]         <- "Norway lobster"
x1[which(x1 %in% str_subset(x1, "whiting"))]          <- "Blue whiting"
x1[which(x1 =="red mullet ")]                        <- "Red mullet"
x1[which(x1 =="red mullet")]                         <- "Red mullet"


Species.tab <- data.frame(Species = x1, 
                        SW.ID = rep(data4$SW.ID, length(x)))
Species.tab <- subset(Species.tab, !is.na(Species))

x1 <- subset(x1, !is.na(x1))

Species.table <- as.data.frame(table(x1))
Species.table <- Species.table[order(-Species.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_Species.png'), width=30, height=25, unit='cm', res=300)
dev.off()

## MODEL name ##########################

# split strings
x  <- tstrsplit(data4$Model.name.abbreviation , split="_")
x1 <- unlist(x)

Model.tab <- data.frame(model = x1, 
                             SW.ID = rep(data4$SW.ID, length(x)))
Model.tab <- subset(Model.tab, !is.na(model))

x1 <- subset(x1, !is.na(x1))
Model.table <- as.data.frame(table(x1))
Model.table <- Model.table[order(-Model.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_Model.png'), width=30, height=25, unit='cm', res=300)
dev.off()

## BIOLOGICAL COMPONENT #################

# split strings
x  <- tstrsplit(data4$Biological.component , split="_")
x1 <- unlist(x)

Bio.tab <- data.frame(bio = x1, 
                        SW.ID = rep(data4$SW.ID, length(x)))
Bio.tab <- subset(Bio.tab, !is.na(bio))

x1 <- subset(x1, !is.na(x1))
Bio.table <- as.data.frame(table(x1))
Bio.table <- Bio.table[order(-Bio.table$Freq),]

print(paste0(round(100*20/26, 1), " % Multistock witout interactions and ", round(100*6/26, 1), " % Single stock"))

## INDICATORS TYPE #####################

# split strings

x1  <- data$Indicators.type
x1[which(x1 == "Other")]   <- "Utility"

Indicator.tab <- data.frame(indicator = x1, 
                        SW.ID = rep(data$SW.ID))
Indicator.tab <- subset(Indicator.tab, !is.na(indicator))

x1 <- subset(x1, !is.na(x1))
Indicator.table <- as.data.frame(table(x1))
Indicator.table <- Indicator.table[order(-Indicator.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.9, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_IndicType.png'), width=20, height=15, unit='cm', res=300)
dev.off()

## INDICATORS NAME #############

# split strings
x  <- tstrsplit(data4$Indicators.name , split="_")
x1 <- unlist(x)

x1[which(x1 == "Landings")]   <- "Catch"
x1[which(x1 %in% str_subset(x1, "evenue"))]   <- "Revenue"
x1[which(x1 %in% str_subset(x1, "rofit"))]   <- "Profit"
x1[which(x1 %in% str_subset(x1, "alar"))]   <- "Salaries"
x1[which(x1 %in% str_subset(x1, "osts"))]   <- "Costs"

Indicator.name.tab <- data.frame(indicatorName = x1, 
                            SW.ID = rep(data4$SW.ID, length(x)))
Indicator.name.tab <- subset(Indicator.name.tab, !is.na(indicatorName))

x1 <- subset(x1, !is.na(x1))
Indicator.name.table <- as.data.frame(table(x1))
Indicator.name.table <- Indicator.name.table[order(-Indicator.name.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_IndicName.png'), width=25, height=20, unit='cm', res=300)
dev.off()


## MANAGEMENT MEASURES CONSIDERED #############
# split strings
x  <- tstrsplit(data4$Management.meas , split="_")
x1 <- unlist(x)

Man.tab <- data.frame(man = x1, 
                      SW.ID = rep(data4$SW.ID, length(x)))
Man.tab <- subset(Man.tab, !is.na(man))

x1 <- subset(x1, !is.na(x1))
Man.table <- as.data.frame(table(x1))
Man.table <- Man.table[order(-Man.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'GREY_Management.png'), width=20, height=15, unit='cm', res=300)
dev.off()


## USED IN MANAGEMENT ################# 

x  <- tstrsplit(data4$Used.in.man, split="_")
x1 <- unlist(x)

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 =="no")]   <- "No"

UsedMan.tab <- data.frame(Useman = x1, 
                      SW.ID = rep(data4$SW.ID, length(x)))
UsedMan.tab <- subset(UsedMan.tab, !is.na(Useman))

x1 <- subset(x1, !is.na(x1))
Manag.table <- as.data.frame(table(x1))
Manag.table <- Manag.table[order(-Manag.table$Freq),]

print(paste0(round(100*18/26, 1), " % Used in MAN ", round(100*8/26, 1), " % Not used"))


## IMPLEMENTED IN SOFT ################ 

# ALL

## ENVIRONMENTAL VARS AND PROCESS ################# (YES/NO)

# None

## SPATIAL CONTEXT ####################

x  <- tstrsplit(data4$Spatial.cont, split="_")
x1 <- unlist(x)
x1 <- subset(x1, !is.na(x1))
x1 <- x1[order(x1)]

# Corrections
x1[which(x1 %in% str_subset(x1, "ot clear")| x1 %in% str_subset(x1,"ot specified") | x1 %in% str_subset(x1,"unclear")| x1 =="Yes")]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "other")| x1 %in% str_subset(x1, "Other") )]   <- "Other"
x1[which(x1 %in% str_subset(x1, "ot relevant"))]   <- "Not relevant"
x1[which(x1 =="NOT RELEVANT")]   <- "Not relevant"
x1[which(x1 =="No")]             <- "No spatial context"

Spat.table <- as.data.frame(table(x1))
Spat.table <- Spat.table[order(-Spat.table$Freq),]

print(paste0(round(100*18/26, 1), " % No spatial context ", round(100*8/26, 1), " % Spatially explicit"))

## SCALES temporal ####################
x                    <- tstrsplit(data4$Scale...Temporal, split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "Equilibrium"))]    <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"

ScaleTemp.tab <- data.frame(TemporalScale = x1, 
                        SW.ID = rep(data4$SW.ID, length(x)))
ScaleTemp.tab <- subset(ScaleTemp.tab, !is.na(TemporalScale))

x1                   <- subset(x1, !is.na(x1))
Scale.Temporal.table <- as.data.frame(table(x1))
Scale.Temporal.table <- Scale.Temporal.table[order(-Scale.Temporal.table$Freq),]
Scale.Temporal.table$prop <- round((Scale.Temporal.table$Freq/sum(Scale.Temporal.table$Freq))*100,1)
colnames(Scale.Temporal.table) <- c('Temporal scale', 'Number of papers', '% of studies')

p <- tableGrob(Scale.Temporal.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'GREY_ScaleTemporal.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Scale.Temporal.table, file=paste0(outPath, 'GREY_ScaleTemporal.csv'), row.names=F)

## SCALE spatial #####################
x                    <- tstrsplit(data4$Scale...Spatial..m., split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"

ScaleSpat.tab <- data.frame(SpatialScale = x1, 
                            SW.ID = rep(data4$SW.ID, length(x)))
ScaleSpat.tab <- subset(ScaleSpat.tab, !is.na(SpatialScale))

x1                  <- subset(x1, !is.na(x1))
Scale.Spatial.table <- as.data.frame(table(x1))
Scale.Spatial.table <- Scale.Spatial.table[order(-Scale.Spatial.table$Freq),]
Scale.Spatial.table$prop <- round((Scale.Spatial.table$Freq/sum(Scale.Spatial.table$Freq))*100,1)
colnames(Scale.Spatial.table) <- c('Spatial scale', 'Number of papers', '% of studies')

p <- tableGrob(Scale.Spatial.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'GREY_ScaleSpatial.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Scale.Spatial.table, file=paste0(outPath, 'GREY_ScaleSpatial.csv'), row.names=F)

## RESOLUTION spatial. ##################
x                    <- tstrsplit(data4$Resolution...Spatial..m., split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "ot specified"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "in degrees"))]     <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"
x1[which(x1=="?")]                                  <- "Not clear"
x1[which(x1 == "Not provided")]                     <- "Not clear"
x1[which(x1 == "not clear")]                        <- "Not clear"

ResolSpat.tab <- data.frame(SpatialResolution = x1, 
                            SW.ID = rep(data4$SW.ID, length(x)))
ResolSpat.tab <- subset(ResolSpat.tab, !is.na(SpatialResolution))

x1                  <- subset(x1, !is.na(x1))
Resolution.Spatial.table <- as.data.frame(table(x1))
Resolution.Spatial.table <- Resolution.Spatial.table[order(-Resolution.Spatial.table$Freq),]
Resolution.Spatial.table$prop <- round((Resolution.Spatial.table$Freq/sum(Resolution.Spatial.table$Freq))*100,1)
colnames(Resolution.Spatial.table) <- c('Spatial resolution', 'Number of papers', '% of studies')

p <- tableGrob(Resolution.Spatial.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'GREY_ResolutionSpatial.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Resolution.Spatial.table, file=paste0(outPath, 'GREY_ResolutionSpatial.csv'), row.names=F)

## RESOLUTION temporal ##################
x                    <- tstrsplit(data4$Resolution...Temporal, split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "snapshot"))]       <- "Not relevant"
x1[which(x1 == "not clear")]                        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"

ResolTemp.tab <- data.frame(TemporalResolution = x1, 
                            SW.ID = rep(data4$SW.ID, length(x)))
ResolTemp.tab <- subset(ResolTemp.tab, !is.na(TemporalResolution))

x1                  <- subset(x1, !is.na(x1))
Resolution.Temporal.table <- as.data.frame(table(x1))
Resolution.Temporal.table <- Resolution.Temporal.table[order(-Resolution.Temporal.table$Freq),]
Resolution.Temporal.table$prop <- round((Resolution.Temporal.table$Freq/sum(Resolution.Temporal.table$Freq))*100,1)
colnames(Resolution.Temporal.table) <- c('Temporal resolution', 'Number of papers', '% of studies')

p <- tableGrob(Resolution.Temporal.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'GREY_ResolutionTemporal.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Resolution.Temporal.table, file=paste0(outPath, 'GREY_ResolutionTemporal.csv'), row.names=F)


# QUALITY METHODS ##################

## All 3

####################################################
#  Make Relational plots
####################################################

# First merge tabs by SW.ID

ModelIndicator  <- merge(Model.tab, Indicator.tab, all.x=T, all.y=T)
TempResScale    <- merge(ResolTemp.tab, ScaleTemp.tab, all.x=T, all.y=T)
SpatResScale    <- merge(ResolSpat.tab, ScaleSpat.tab, all.x=T, all.y=T)
ModelScale      <- merge(ScaleSpat.tab, Model.tab, all.x=T, all.y=T)
UmanRegion      <- merge(Region.tab, UsedMan.tab, all.x=T, all.y=T)
ManRegion       <- merge(Region.tab, Man.tab, all.x=T, all.y=T)
ManIndicator    <- merge(Man.tab, Indicator.tab, all.x=T, all.y=T)
ManModel        <- merge(Man.tab, Model.tab, all.x=T, all.y=T)
UmanModel       <- merge(UsedMan.tab, Model.tab, all.x=T, all.y=T)
SpeciesRegion   <- merge(Species.tab, Region.tab, all.x=T, all.y=T)
IndNameType     <- merge(Indicator.tab, Indicator.name.tab, all.x=T, all.y=T)
ManIndName      <- merge(Man.tab, Indicator.name.tab, all.x=T, all.y=T)

# ------- Used in Management by region

data.new <- as.data.table(UmanRegion)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, Useman!='Not clear')
colnames(data.new) <- c('SW.ID', 'Region', 'Used_in_Management')
data.new5 <- data.new %>%
  make_long(Region, Used_in_Management)

ggplot(data.new5, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_UsemanRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# -------Management by region

data.new <- as.data.table(ManRegion)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
colnames(data.new) <- c('SW.ID', 'Region', 'Management')
data.new6 <- data.new %>%
  make_long(Region, Management)

ggplot(data.new6, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_ManRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# -------Management by Indicator

data.new <- as.data.table(ManIndicator)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, indicator!='Not specified')
data.new <- subset(data.new, indicator!='Not relevant')
colnames(data.new) <- c('SW.ID', 'Management', 'Indicator')
data.new7 <- data.new %>%
  make_long(Management, Indicator)

ggplot(data.new7, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_ManIndicator.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# -------Management by Model

data.new <- as.data.table(ManModel)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Management', 'Model')
data.new8 <- data.new %>%
  make_long(Management, Model)

ggplot(data.new8, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_ManModel.png'), width=18, height=18, unit='cm', res=300)
dev.off()  


# ------ Used in Man, Model, Management, Indicator
#df1 <- merge(UmanModel, ManModel, all.x=T, all.y=T)
#df2 <- merge(df1, ManIndicator, all.x=T, all.y=T)
 
df1 <-  merge(UmanModel, ManModel, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Model', "Used_in_Management", 'Management')
data.new8 <- data.new %>%
  make_long(Used_in_Management, Model, Management)

ggplot(data.new8, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_UsedMan_ManModel.png'), width=18, height=18, unit='cm', res=300)
dev.off()  


# ------  Model, Management, Indicator
#df1 <- merge(UmanModel, ManModel, all.x=T, all.y=T)
#df2 <- merge(df1, ManIndicator, all.x=T, all.y=T)

df1 <-  merge(ManModel, ManIndName, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, region!='Not relevant')
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Region', 'Used_in_Man', "Model")
data.new16 <- data.new %>%
  make_long( Management, Model, Indicator)

ggplot(data.new16, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_Man_Model_Indicator.png'), width=22, height=18, unit='cm', res=300)
dev.off()  

# ------  Used Management, Region, Management

df1 <-  merge(UmanRegion, ManRegion, all.x=T, all.y=T)

data.new <- as.data.table(df1)
colnames(data.new) <- c('SW.ID', 'Region', 'Used_in_Management', "Management")
data.new8 <- data.new %>%
  make_long(Used_in_Management, Region, Management)

ggplot(data.new8, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'GREY_UsedMan_Region_Management.png'), width=20, height=16, unit='cm', res=300)
dev.off()  


# ----- Spatial and temporal resolution with scale

### Spatial scale vs resolution


### Temporal scale vs resolution
x  <- as.data.frame(table(TempResScale[,c(2:3)]))
x  <- x[order(-x$Freq),] 

# with ggplot
ggplot(x, aes(fill=TemporalResolution, y=Freq, x=reorder(TemporalScale, +Freq) )) + 
  geom_bar(position="stack", stat="identity")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('Number of retained papers')+
  xlab('Temporal scale')+
  ggtitle ('Temporal scale and resolution of retained papers')+
  scale_fill_viridis_d()
dev.copy(png, file=paste0(outPath, "GREY_Temporalscale&resolution.png") , width = 18, height = 13, unit='cm', res=100)
dev.off()

# with simple plot
#x1 <- dcast(x, SpatialResolution~SpatialScale)
#rownames(x1) <-  x1[,1]
#x1 <- x1[,-1]
#x1 <- as.matrix(x1)

# with simple plot
#tiff(paste0(outPath, "Temporalscale&resolution6.tiff"), width = 1000, height = 1000, res=100)
#layout(mat = (matrix(nrow=1, ncol=2, data=c(1,2))), widths = c(6,4), heights = c(1,1))
# plot 1: small scales
#par(mar=c(10,5,8,0))
#b <- barplot(x1)
#b <- barplot(x1, ylim=c(0,450), axes=F, names.arg=rep("", 10), width=1, xlim=c(0,13), col=viridis(15))
#axis(2, at=seq(0,450,100), cex.axis=1.2, las=1, pos=0)
#axis(1, at=b, colnames(x1), las=3)
#axis(2, at=250, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
#abline(v=9.7, lty=3, col="dimgrey")
#dev.copy(png, file=paste0(outPath, "Temporalscale&resolution.png") , width = 20, height = 20, unit='cm', res=100)
#dev.off()



### TREEMAPS #######################################



# -------- species with region

x <- table(SpeciesRegion$Species, SpeciesRegion$region)
x <- as.data.frame(x)

x1 <- subset(x, Var2!='Other')
x1 <- subset(x1, Var1!='Not relevant')
x1 <- subset(x1, Var2!='Not specified')
treemap(x1, index=c("Var2","Var1"), vSize="Freq", type="index",
        
        fontsize.labels=c(14,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels= 0,              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.7,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)
dev.copy(png, file=paste0(outPath, "GREYTreemap_species_region.png") , width = 18, height = 15, unit='cm', res=100)
dev.off()

### HEATMAPS #######################################




