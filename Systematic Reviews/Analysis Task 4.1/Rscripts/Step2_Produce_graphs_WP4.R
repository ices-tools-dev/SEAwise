#####################################################################################################################
#####################################################################################################################
#
#     Script to read, merge, and analyse data extraction files from SEAwise task 4.1
#     Step 2. Produce figures for the report.
#
#     By Karin van der Reijden, additions by Elliot J. Brown
#     March 2022
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
library(plotly)
library(splitstackshape)
library(ggthemes)
library(ggsankey)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Data_Extraction_Files/" 
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS/"

################################################
#-----------------------------------------------
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 1.
#-----------------------------------------------
data                                  <- readRDS(file=paste0(outPath, "data.rds"))
data_allScreened                      <- readRDS(file=paste0(outPath, "data_allScreened.rds"))
load(file = paste0(outPath, "FatePapers.Rdata"))

################################################
#-----------------------------------------------
# Plot the fate of all records returned from searches
#
#  info:
#  This section is relatively stand-alone.  It creates sankey diagrams illustrating the fate of all records from search to extraction
#-----------------------------------------------
#===
# Data cleaning
#====
## Combine exclusion and inclusion reasons to integrated fate column
FatePapers$Extraction.Code <- NA
FatePapers[is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Code"] <- FatePapers[is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.WP4.task"]
FatePapers[!is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Code"] <- FatePapers[!is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Exclusion.Code"]

## Create long versions of both screening and full-text fates for each record
screenFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Screening.Code")],
                         splitCols = "Screening.Code",
                         sep = " _ ",
                         direction = "long")

ExtractFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Extraction.Code")],
                         splitCols = "Extraction.Code",
                         sep = " _ ",
                         direction = "long")
## Merge screening and extraction fates
fatelong <- merge(screenFatelong, ExtractFatelong, by = "SW.ID", all = TRUE)


## Rename some levels for consistency across screening and extraction phases
fatelong$Screening.Code[grepl("4.2", fatelong$Screening.Code)] <- "4.2"
fatelong$Screening.Code[grepl("4.3", fatelong$Screening.Code)] <- "4.3"
fatelong$Screening.Code[grepl("4.4", fatelong$Screening.Code)] <- "4.4"
fatelong$Screening.Code[grepl("4.5", fatelong$Screening.Code)] <- "4.5"
fatelong$Screening.Code[grepl("INCLUDE on title", fatelong$Screening.Code)] <- "4.general"

fatelong$Extraction.Code[grepl("None", fatelong$Extraction.Code)] <- "4.general"

fatelong$SearchResults <- rep("Deduplicated Search Results", nrow(fatelong))

fatelong$UltimateFate <- ifelse(grepl("4.", fatelong$Extraction.Code), "Data Extracted", "Excluded from Review")
fatelong[!grepl("4.", fatelong$Screening.Code), "UltimateFate"] <- "Excluded from Review"
fatelong[!grepl("4.", fatelong$Screening.Code), "Extraction.Code"] <- "Excluded from Review"

sfate <- make_long(fatelong, SearchResults, Screening.Code, Extraction.Code, UltimateFate)

#===
# Create sankey diagram
#====
## Build sankey
sankey <- ggplot(sfate,
                 mapping = aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(node),
                               label = node)) +
  geom_sankey() +
  geom_sankey_label(size=7, colour="white") +
  scale_fill_manual(values=viridis(16)[-16]) +
  theme_few()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=20),
        legend.position = "none")

## View Sankey
sankey

## Save sankey
ggsave("Sankey.tiff", sankey, path=outPath,
       width = 400,
       height = 200,
       units = "mm")

## View interactive sankey (needs work)
ggplotly(sankey)
#=====

#===
# Temp visualisations, can be deleted
#====
# ggplot(fatelong[fatelong$Screening.Code %in% c("4.2", "4.3", "4.4", "4.5", "4.general"),]) +
#   geom_bar(mapping = aes(x = Extraction.Code)) +
#   theme_clean() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# 
# ggplot(fatelong) +
#   geom_bar(mapping = aes(x = Screening.Code)) +
#   theme_clean() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# 
# ggplot(fatelong) +
#   geom_tile(mapping = aes(x = Screening.Code,
#                           y = Extraction.Code)) +
#   theme_clean() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#=====
#-----------------------------------------------


#-----------------------------------------------
################################################
#-----------------------------------------------
# Produce some plots
#
#  info:
#  This section aims for a quick analysis of the retained papers, for the report.
#  It summarizes the number of unique papers (SW.ID) per (combination of) ecosystem component, fishing type, gear level1, region, response variable. 
#-----------------------------------------------

#-----------------------------------------------
## Creating shapefile with Regions
#-----------------------------------------------
# ## Load ICES regions as downloaded from https://gis.ices.dk/sf/ and mediterranean GSA's from https://www.fao.org/gfcm/data/maps/gsas/en/
# ICESareas                            <- st_read(paste0(GISpath, "ICES_Areas_20160601_cut_dense_3857.shp"))
# ICESareas                            <- st_transform(ICESareas, crs=3035)
# ICESEcors                            <- st_read(paste0(GISpath, "ICES_ecoregions_20171207_erase_ESRI.shp"))
# ICESEcors                            <- st_transform(ICESEcors, crs=3035)
# GSAs                                 <- st_read(paste0(GISpath, "GSAs_simplified.shp"))
# GSAs                                 <- st_transform(GSAs, crs=3035)
# 
# 
# ICESEcors$Region                     <- ifelse(ICESEcors$Ecoregion %in% c("Western Mediterranean Sea", "Adriatic Sea",
#                                                                    "Ionian Sea and the Central Mediterranean Sea",
#                                                                    "Aegean-Levantine Sea"), "Mediterranean - non CS", 
#                                                ifelse(ICESEcors$Ecoregion == "Greater North Sea", "North Sea - non CS", 
#                                                       ifelse(ICESEcors$Ecoregion %in% c("Black Sea", "Norwegian Sea","Barents Sea"), ICESEcors$Ecoregion,
#                                                              ifelse(ICESEcors$Ecoregion == "Baltic Sea", "Baltic Sea - non CS", 
#                                                                     ifelse(ICESEcors$Ecoregion %in% c("Azores", "Oceanic Northeast Atlantic", "Greenland Sea",
#                                                                                                       "Icelandic Waters", "Faroes", "Celtic Seas"), "NE-Atlantic", NA)))))
# 
# ICESareas$Region                     <- ifelse(ICESareas$Area_Full %in% c("27.4.c", "27.4.b", "27.4.a", "27.7.d"), "CS - North Sea",
#                                                ifelse(ICESareas$Area_Full %in% c("27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.26","27.3.d.27","27.3.d.28.1","27.3.d.28.2","27.3.d.29"), "CS - Baltic Sea",
#                                                       ifelse(ICESareas$Area_Full %in% c("27.7.a", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.8.a","27.8.b","27.8.c", "27.8.d.2"), "CS - Western Waters", 
#                                                              ifelse(ICESareas$Area_Full %in% c("27.9.a", "27.9.b.1", "27.9.b.2", "27.8.e.1", "27.8.e.2", "27.6.a", "27.7.c.2", 
#                                                                                                "27.6.b.2", "27.7.b", "27.7.k.2", "27.7.j.2", "27.7.b", "27.7.j.1"), "NE-Atlantic", NA))))
# GSAs$Region                          <- ifelse(GSAs$SECT_COD %in% c("GSA17", "GSA18", "GSA19", "GSA20", "GSA22"), "CS - Mediterranean", NA)
# 
# Regions                              <- rbind(subset(ICESEcors[,c("Region", "geometry")], is.na(Region)==F), subset(ICESareas[,c("Region", "geometry")], is.na(Region)==F))
# Regions                              <- rbind(Regions, subset(GSAs[,c("Region", "geometry")], is.na(Region)==F))
# 
# Regs                                 <- subset(Regions, Region %in% c("Baltic Sea - non CS", "Barents Sea", "Black Sea", "North Sea - non CS", "Norwegian Sea"))
# 
# for (iReg in c("CS - Mediterranean", "CS - North Sea","CS - Western Waters", "Mediterranean - non CS", "CS - Baltic Sea", "NE-Atlantic")){
#   subdat                             <- subset(Regions, Region == iReg)
#   a                                  <- st_sf(st_union(subdat))
#   b                                  <- data.frame(Region = iReg)
#   b                                  <- st_set_geometry(b, st_geometry(a)) 
#   Regs                               <- rbind(Regs, b)
# } # end iReg loop
# 
# ## Fix some overlaps that should not be there
# ## North Sea
# NS                                   <- st_intersection(subset(Regs, Region == "CS - North Sea"), subset(Regs, Region == "North Sea - non CS"))
# NS$Region.1                          <- NULL
# 
# ## NE-Atlantic
# WW                                   <- st_difference(subset(Regs, Region == "NE-Atlantic"), subset(Regs, Region == "CS - Western Waters"))
# WW$Region.1                          <- NULL
# 
# ## Mediterranean non CS
# MED                                  <- st_difference(subset(Regs, Region == "Mediterranean - non CS"), subset(Regs, Region == "CS - Mediterranean"))
# MED$Region.1                         <- NULL
# 
# ## Baltic non CS
# BALT                                 <- st_difference(subset(Regs, Region == "Baltic Sea - non CS"), subset(Regs, Region == "CS - Baltic Sea"))
# BALT$Region.1                        <- NULL
# 
# ## Update the fixes
# Regs2                                <- subset(Regs, !Region %in% c("CS - North Sea", "NE-Atlantic", "Mediterranean - non CS", "Baltic Sea - non CS"))
# Regs2                                <- rbind(Regs2, NS, WW, MED, BALT)
# 
# SEAwise4.1_regions                   <- Regs2
# st_write(SEAwise4.1_regions, paste0(GISpath, "SEAwise4.1_regions.shp"))
# save(SEAwise4.1_regions, file=paste0(GISpath, "SEAwise4.1_regions.Rdata"))


#-----------------------------------------------
## Creating map with Region specific info
#-----------------------------------------------
Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), 
                                              by = Region]
Regions                               <- Regions[order(NrPaps),,]
RegCol                               <- data.frame(colcode = viridis(max(Regions$NrPaps)),
                                                   value = c(1:max(Regions$NrPaps)))
Regions$Colcode                      <- RegCol$colcode [match(Regions$NrPaps, RegCol$value)]
load(paste0(GISpath, "SEAwise4.1_regions.Rdata"))
SEAwise4.1_regions$colcode           <- Regions$Colcode [match(SEAwise4.1_regions$Region, Regions$Region)]
Centerpoints                         <- st_coordinates(st_centroid(SEAwise4.1_regions))
SEAwise4.1_regions$Xloc              <- Centerpoints[,1]
SEAwise4.1_regions$Yloc              <- Centerpoints[,2]
SEAwise4.1_regions$NrPaps            <- Regions$NrPaps [match(SEAwise4.1_regions$Region, Regions$Region)]
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
par(mar=c(1,1,4,1))
plot(st_geometry(SEAwise4.1_regions), col=SEAwise4.1_regions$colcode, border=F)
title(main = "Retained papers per region", cex.main=2, font.main=2)
text(SEAwise4.1_regions$NrPaps, x= SEAwise4.1_regions$Xloc, y=SEAwise4.1_regions$Yloc, col=SEAwise4.1_regions$textcol, font=2, cex=1.4)
text(x= 1E6, y=6.01E6, "Global studies:", font=3, cex=1.4)
text(x=1e6, y=5.85e6, paste0(subset(Regions, Region == "Global")$NrPaps), font=2, cex=1.4)
gradient.rect(xleft=0, xright=1E6, ytop=1.5e6, ybottom=1.25E6, col=RegCol$colcode, gradient="x")
text(x=0, y=1.18e6, "1")
text(x=1E6, y=1.18e6, max(Regions$NrPaps))
text(x=0.5e6, y=1.72e6, "Number of \n papers retained", font=4, cex=1.2)
dev.off()

#-----------------------------------------------
## Barplot for regions
#-----------------------------------------------
Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), 
                                              by = Region]
Regions                               <- Regions[order(NrPaps),,]

tiff(paste0(outPath, "Regions.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(Regions$NrPaps, horiz=TRUE, axes=F, xlim=c(0,183))
box()
axis(2, at=b, labels=Regions$Region, las=1, cex.axis=1.2)
axis(1, at= seq(0,180, 20), labels=seq(0, 180, 20), cex.axis=1.2)
title(main="Number of unique papers per study region", cex.main=1.5, font.main=2)
text(x=100, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=4)
text(x=Regions$NrPaps + 4, y=b, Regions$NrPaps)
axis(1, at=90, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

#-----------------------------------------------
## Barplot for Response variable categories
#-----------------------------------------------
ResVarCats                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,260))
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=1.2)
axis(1, at= seq(0,250, 20), labels=seq(0, 250, 20), cex.axis=1.2)
title(main="Number of unique papers per response variable category", cex.main=1.5, font.main=2)
text(x=140, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+6, y=b, ResVarCats$NrPaps)
axis(1, at=125, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

#-----------------------------------------------
## Barplot for ecosystem component (level 1)
#-----------------------------------------------
EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Ecosystem.component_level1]
EcoComp                               <- EcoComp[order(NrPaps),,]

tiff(paste0(outPath, "EcoComp.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(EcoComp$NrPaps, horiz=TRUE, axes=F, xlim=c(0,230))
box()
axis(2, at=b, labels=EcoComp$Ecosystem.component_level1 , las=1, cex.axis=1.2)
axis(1, at= seq(0,220, 20), labels=seq(0, 220, 20), cex.axis=1.2)
title(main="Number of unique papers per ecosystem component (level 1)", cex.main=1.5, font.main=2)
text(x=160, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))))
text(x=EcoComp$NrPaps+6, y=b, EcoComp$NrPaps)
axis(1, at=110, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()


#-----------------------------------------------
## Barplot for ecosystem component (level 1) by Case Study region
#-----------------------------------------------

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))),
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
        panel.grid = element_blank()) +
  facet_wrap(~Area)
print(p)
ggsave("EcoRegion.tiff", p, path=outPath, width = 8, height = 5)

#-----------------------------------------------
## Barplot for WP4 task
#-----------------------------------------------
## Split the double-input papers
Tasks                                 <- tstrsplit(data$WP4.task, split=" _ ")
Tasks2                                <- data.table(SW.ID = rep(data$SW.ID, 3),
                                                    Task  = c(Tasks[[1]], Tasks[[2]], Tasks[[3]]))
Tasks2                                <- na.omit(Tasks2)

## Determine number of papers retained per task
RetTask                               <- Tasks2[, .(NrPaps = length(unique(SW.ID))),
                                                by = c("Task")]
RetTask                               <- RetTask[order(Task, decreasing=T),,]

tiff(paste0(outPath, "RetTask.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 10, 4, 2))
b                                     <- barplot(RetTask$NrPaps, horiz=TRUE, axes=F, xlim=c(0,180))
box()
axis(2, at=b, labels=RetTask$Task , las=1, cex.axis=1.2)
axis(1, at= seq(0,180, 20), labels=seq(0, 180, 20), cex.axis=1.2)
axis(2, at=max(b)/2, tick=F, line=3, "Work package 4 subquestion", cex.axis=1.5)
title(main="Number of unique papers per work package 4 subquestion", cex.main=1.5, font.main=2)
text(x=145, y=0.2, paste0("Total number of papers retained: ", length(unique(data$SW.ID))))
text(x=RetTask$NrPaps+4, y=b, RetTask$NrPaps)
axis(1, at=90, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

#-----------------------------------------------
## Barplots of fishing gears studied
#-----------------------------------------------
Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))

tiff(paste0(outPath, "GearsStudied.tiff"), width= 2000, height = 1000, res = 100)
par(mfrow=c(1,4))
par(mar=c(15,8,15,1))
for(iType in unique(Gears$Fishery.type)){
  subdat                             <- Gears[Fishery.type == iType,,]
  subdat                             <- subdat[order(NrPaps, decreasing=T)]
  b                                  <- barplot(subdat$NrPaps, axes=F, ylim=c(0, max(subdat$NrPaps*1.05)))
  title(iType, cex.main=2, font=2, line=3)
  title(paste0("(n = ", sum(subdat$NrPaps), ")"), font.main=3, cex.main=1.5, line=1.5)
  axis(1, at=b, labels=subdat$Gear_level1, las=3, cex.axis=2)
  axis(2, las=1, cex.axis=2)
  text(x=b, y=max(subdat$NrPaps)*0.025, labels=subdat$NrPaps, cex=1.5, font=3)
  box()
  if(iType=="Commercial"){
    axis(2, tick=F, at=(max(subdat$NrPaps)*1.05)/2, labels="Number of unique papers retained",
         line=4.5, cex.axis=2)}
}
par(fig=c(0,1,0,1), new=TRUE)
par(mar=c(2,2,6,2))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title("The different fishing gears studied", line=0, cex.main=3, font.main=2)
dev.off()


#-----------------------------------------------
## Barplots of fishing gears studied - Comm + Recr only
#-----------------------------------------------
Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears                                <- subset(Gears, Fishery.type %in% c("Commercial","Recreational"))
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))

tiff(paste0(outPath, "GearsStudiedCommRecr.tiff"), width= 1500, height = 1000, res = 100)
par(mfrow=c(1,2))
par(mar=c(15,8,10,1))
for(iType in unique(Gears$Fishery.type)){
  subdat                             <- Gears[Fishery.type == iType,,]
  subdat                             <- subdat[order(NrPaps, decreasing=T)]
  b                                  <- barplot(subdat$NrPaps, axes=F, ylim=c(0, max(subdat$NrPaps*1.05)))
  title(iType, cex.main=2, font=2, line=3)
  title(paste0("(n = ", sum(subdat$NrPaps), ")"), font.main=3, cex.main=1.5, line=1.5)
  axis(1, at=b, labels=subdat$Gear_level1, las=3, cex.axis=2)
  axis(2, las=1, cex.axis=2)
  text(x=b, y=max(subdat$NrPaps)*0.035, labels=subdat$NrPaps, cex=1.5, font=3)
  box()
  if(iType=="Commercial"){
    axis(2, tick=F, at=(max(subdat$NrPaps)*1.05)/2, labels="Number of unique papers retained",
         line=4.5, cex.axis=2)}
}
par(fig=c(0,1,0,1), new=TRUE)
par(mar=c(2,2,6,2))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title("The different fishing gears studied", line=0, cex.main=3, font.main=2)
dev.off()



#-----------------------------------------------
## Barplots of Spatial scale & resolution
#-----------------------------------------------
Scales                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                              by = c("Scale...Spatial..m.", "Resolution...Spatial..m.")]
names(Scales)                         <- c("Scale", "Resolution", "NrPaps")
Scales[is.na(Scales)]                 <- "Not specified"

Scaleorder                            <- data.frame(Scale = c(unique(data$Scale...Spatial..m.), "10-50"),
                                                    Sord  = c(12,8,10,9,11,7,1,5,6,2,3,4)) # 12
Scaleorder                            <- Scaleorder[order(Scaleorder$Sord),]
Scaleorder[is.na(Scaleorder)]         <- "Not specified"

Scales2                               <- matrix(nrow=12, ncol=12, dimnames=list(Scaleorder$Scale, Scaleorder$Scale))

for(iRow in c(1:nrow(Scales))){
  subset                              <- Scales[iRow,]
  Scales2[subset$Resolution, subset$Scale] <- subset$NrPaps
}
Scales2[is.na(Scales2)]               <- 0
largeScale                            <- as.matrix(Scales2[,12])
smallScale                            <- Scales2[,c(1:11)]

## Create plot
tiff(paste0(outPath, "Spatialscale&resolution.tiff"), width = 1000, height = 1000, res=100)
graphics::layout(mat = (matrix(nrow=1, ncol=2, data=c(1,2))), widths = c(5,1), heights = c(1,1))
# plot 1: small scales
par(mar=c(10,5,5,0))
b <- barplot(smallScale, ylim=c(0,90), axes=F, names.arg=rep("", 11), width=1, xlim=c(0,13.2), col=viridis(12))
axis(2, at=seq(0,90,10), cex.axis=1.2, las=1, pos=0)
axis(1, at=b, colnames(smallScale), las=3)
axis(2, at=45, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
abline(v=13.6, lty=3, col="dimgrey")
# plot 2: large scale
par(mar=c(10,0,5,5))
b2 <- barplot(largeScale, ylim=c(0,350), axes=F, names.arg="", width=1, xlim=c(0,1.2), col=viridis(12))
axis(1, at=b2, Scaleorder$Scale[12], las=3)
axis(4, at=seq(0, 350, 50), las=1, cex.axis=1.2, pos=1.4)
par(fig=c(0,1,0,1), new=T)
par(mar=c(5,5,5,5))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
axis(1, at=0.5, tick=F, line=2, "Spatial scale", cex.axis=1.5)
title("Spatial scale and resolution \n of retained papers", cex.main=2, font.main=2)
legend(x=0, y=1, cex=1.2, fill=rev(viridis(12)), title="Spatial resolution", legend=rev(Scaleorder$Scale), bty="n")
dev.off()

#-----------------------------------------------
## Barplots of Temporal scale & resolution
#-----------------------------------------------
TempSc                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                              by = c("Scale...Temporal", "Resolution...Temporal")]
names(TempSc)                         <- c("Scale", "Resolution", "NrPaps")
TempSc[is.na(TempSc)]                 <- "not specified"
TempSc$Resolution                     <- ifelse(TempSc$Resolution == "snapshot/no repeat sampling", "snapshot", TempSc$Resolution)

Tempsorder                            <- data.frame(Temp = c(unique(data$Resolution...Temporal)),
                                                    Tord  = c(7,3,5,9,2,15,11,4,6,10,8,14,13,12,1)) # 15
Tempsorder                            <- Tempsorder[order(Tempsorder$Tord),]
Tempsorder[is.na(Tempsorder)]         <- "not specified"
Tempsorder$Temp                       <- ifelse(Tempsorder$Temp == "snapshot/no repeat sampling", "snapshot", Tempsorder$Temp)

TempSc2                               <- matrix(nrow=15, ncol=14, dimnames=list(Tempsorder$Temp, Tempsorder$Temp[c(1,3:15)]))

for(iRow in c(1:nrow(TempSc))){
  subset                              <- TempSc[iRow,]
  TempSc2[subset$Resolution, subset$Scale] <- subset$NrPaps
}
TempSc2[is.na(TempSc2)]               <- 0
largeTemp                             <- TempSc2[,c(10:14)]
smallTemp                             <- TempSc2[,c(1:9)]

## Create plot
tiff(paste0(outPath, "Temporalscale&resolution.tiff"), width = 1000, height = 1000, res=100)
graphics::layout(mat = (matrix(nrow=1, ncol=2, data=c(1,2))), widths = c(6,4), heights = c(1,1))
# plot 1: small scales
par(mar=c(10,5,8,0))
b <- barplot(smallTemp, ylim=c(0,40), axes=F, names.arg=rep("", 9), width=1, xlim=c(0,11), col=viridis(15))
axis(2, at=seq(0,40,5), cex.axis=1.2, las=1, pos=0)
axis(1, at=b, colnames(smallTemp), las=3)
axis(2, at=20, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
abline(v=11.25, lty=3, col="dimgrey")
# plot 2: large scale
par(mar=c(10,0,8,5))
b2 <- barplot(largeTemp, ylim=c(0,120), axes=F, names.arg=rep("",5), width=1, xlim=c(0.2,6.2), col=viridis(15))
axis(1, at=b2, colnames(largeTemp), las=3)
axis(4, at=seq(0, 120, 20), las=1, cex.axis=1.2, pos=6.2)
par(fig=c(0,1,0,1), new=T)
par(mar=c(5,5,5,5))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
axis(1, at=0.5, tick=F, line=2, "Temporal scale", cex.axis=1.5)
title("Temporal scale and resolution \n of retained papers", cex.main=2, font.main=2)
legend(x=0, y=1, cex=1.2, fill=rev(viridis(15)), title="Temporal resolution", ncol=2, legend=rev(Tempsorder$Temp), bty="n")
dev.off()


#-----------------------------------------------
## Heatmap of ecosystem component level 1 vs pressure type level 1
#-----------------------------------------------
EcoPress                             <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Pressure.type")]
EcoPressmat                          <- matrix(nrow = length(unique(data$Ecosystem.component_level1)),
                                               ncol = length(unique(data$Pressure.type)))
colnames(EcoPressmat)                <- sort(unique(data$Pressure.type))  
rownames(EcoPressmat)                <- sort(unique(data$Ecosystem.component_level1))

for(iRow in c(1:nrow(EcoPress))){
  subdat                             <- EcoPress[iRow,]
  EcoPressmat[subdat$Ecosystem.component_level1, subdat$Pressure.type] <- subdat$NrPaps
}
r                                    <- raster(EcoPressmat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(EcoPressmat, na.rm=T))   


tiff(paste0(outPath, "EcoPress_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F, col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
         y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/14, 13/14, length.out=7), labels= c("Catch & bycatch", "Discarding", "Electromagnetic input", "Input of litter", "Noise", "Physical disturbance", "Visual disturbance"), las=3, cex.axis=1.5)
axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(EcoPressmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing pressure effects on ecosystem components", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("140", x=0.97, y=0, font=3)
text("70", x=0.97, y=0.15, font=3)
dev.off()


#-----------------------------------------------
## Heatmap of ecosystem component vs fishery type
#-----------------------------------------------
EcoFish                              <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type")]
EcoFishmat                           <- matrix(nrow = length(unique(data$Ecosystem.component_level1)),
                                               ncol = length(unique(data$Fishery.type)))
colnames(EcoFishmat)                 <- sort(unique(data$Fishery.type))  
rownames(EcoFishmat)                 <- sort(unique(data$Ecosystem.component_level1))

for(iRow in c(1:nrow(EcoFish))){
  subdat                             <- EcoFish[iRow,]
  EcoFishmat[subdat$Ecosystem.component_level1, subdat$Fishery.type] <- subdat$NrPaps
}
r                                    <- raster(EcoFishmat)
#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))


tiff(paste0(outPath, "EcoFish_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/4, 2/4, 3/4), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
         y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(0.125, 0.875, length.out=4), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing type studied per ecosystem components", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("190", x=0.97, y=0, font=3)
text("95", x=0.97, y=0.15, font=3)
dev.off()


#-----------------------------------------------
## Heatmap of ecosystem component vs fishery type / gear_level 1
#-----------------------------------------------
EcoFishGear                          <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified", 
                                               ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1)) 
rCols                                <- data.table(colcode = viridis(max(EcoFishGear$NrPaps)),
                                                   value = c(1:max(EcoFishGear$NrPaps)))

tiff(paste0(outPath, "EcoGears_heatmap.tiff"), width= 1500, height = 500, res = 100)
par(mfrow=c(1,6))
par(mar=c(13, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=12)), axes=F, box=F)
axis(2, tick=F, at=seq(1/22, 21/22, length.out=11), labels=rev(sort(unique(EcoFishGear$Ecosystem.component_level1))), las=1, pos=1.2, cex.axis=1.5)

for(iType in unique(EcoFishGear$Fishery.type)){
  subdat                             <- EcoFishGear[Fishery.type == iType,,]
  
  EcoFishGM                          <- matrix(nrow = 11,
                                               ncol = length(unique(subdat$Gear_level1)))
  colnames(EcoFishGM)                <- sort(unique(subdat$Gear_level1))  
  rownames(EcoFishGM)                <- sort(unique(EcoFishGear$Ecosystem.component_level1))
  
  for(iRow in c(1:nrow(subdat))){
    subdat2                          <- subdat[iRow,]
    EcoFishGM[subdat2$Ecosystem.component_level1, subdat2$Gear_level1] <- subdat2$NrPaps
  }
  r                                  <- raster(EcoFishGM)
  
  par(mar=c(13, 1, 10, 1))
  plot(r, axes=F,
       col=subset(rCols, value %in% values(r))$colcode, legend=F, box=F)
  title(iType, font.main=2, cex.main=2, line=1.5)
  title(paste0("(n = ", sum(subdat$NrPaps), ")"), font.main=3, cex.main=1, line=0.5)
  segments(x0=seq(0, 1, length.out=ncol(EcoFishGM)+1), x1=seq(0, 1, length.out=ncol(EcoFishGM)+1), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
  segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
           y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
  segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
  segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
  axis(1, at=seq((1/ncol(EcoFishGM))/2, 1-((1/ncol(EcoFishGM))/2), length.out=ncol(EcoFishGM)), labels= colnames(EcoFishGM), las=3, cex.axis=1.5, pos=0)
  if(iType == "Scientific"){axis(4, at=seq(0.04166666, 0.9583333, length.out=12), labels= rep("", 12), pos=1)}
  if(iType == "Commercial"){axis(2, at=seq(0.04166666, 0.9583333, length.out=12), labels= rep("", 12), pos=0)}
}

par(mar=c(13, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=12)), axes=F, box=F)
axis(4, tick=F, at=seq(1/22, 21/22, length.out=11), labels=rev(rownames(EcoFishGM)), las=1, pos=-0.1, cex.axis=1.5)

par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,0))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing gear studied per fishing type and ecosystem components", cex.main=2, font.main=2, line=0)
gradient.rect(xleft=0.9, xright=1.01, ybottom=0.05, ytop=0.15, col=rCols$colcode, gradient="x")
text("Number of \n papers retained", x=0.955, y=0.2, font=4, cex=1.3)
text("1", x=0.89, y=0.1, font=3)
text("146", x=1.02, y=0.1, font=3)
dev.off()

#-----------------------------------------------
## Heatmap of ecosystem component vs gear_level 1 for Commercial
#-----------------------------------------------

EcoFishGear                          <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
EcoFishGear                          <- subset(EcoFishGear, Fishery.type %in% "Commercial")
EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified", 
                                               ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1)) 
rCols                                <- data.table(colcode = viridis(max(EcoFishGear$NrPaps)),
                                                   value = c(1:max(EcoFishGear$NrPaps)))

EcoFishmat                           <- matrix(nrow = length(unique(EcoFishGear$Ecosystem.component_level1)),
                                               ncol = length(unique(EcoFishGear$Gear_level1)))
colnames(EcoFishmat)                 <- sort(unique(EcoFishGear$Gear_level1))  
rownames(EcoFishmat)                 <- sort(unique(EcoFishGear$Ecosystem.component_level1))

for(iRow in c(1:nrow(EcoFishGear))){
  subdat                             <- EcoFishGear[iRow,]
  EcoFishmat[subdat$Ecosystem.component_level1, subdat$Gear_level1] <- subdat$NrPaps
}
r                                    <- raster(EcoFishmat)
#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))


tiff(paste0(outPath, "EcoFishComm_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F, col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/9, 2/9, 3/9, 4/9, 5/9, 6/9, 7/9, 8/9), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
         y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(0.05, 0.95, length.out=9), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Commercial fishing gears studied per ecosystem component", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("152", x=0.97, y=0, font=3)
text("76", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Heatmap of ecosystem component vs gear_level 1 for Recreational
#-----------------------------------------------

EcoFishGear                          <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
EcoFishGear                          <- subset(EcoFishGear, Fishery.type %in% "Recreational")
EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified", 
                                               ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1)) 
rCols                                <- data.table(colcode = viridis(max(EcoFishGear$NrPaps)),
                                                   value = c(1:max(EcoFishGear$NrPaps)))

EcoFishmat                           <- matrix(nrow = length(unique(EcoFishGear$Ecosystem.component_level1)),
                                               ncol = length(unique(EcoFishGear$Gear_level1)))
colnames(EcoFishmat)                 <- sort(unique(EcoFishGear$Gear_level1))  
rownames(EcoFishmat)                 <- sort(unique(EcoFishGear$Ecosystem.component_level1))

for(iRow in c(1:nrow(EcoFishGear))){
  subdat                             <- EcoFishGear[iRow,]
  EcoFishmat[subdat$Ecosystem.component_level1, subdat$Gear_level1] <- subdat$NrPaps
}
r                                    <- raster(EcoFishmat)
#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))


tiff(paste0(outPath, "EcoFishRecr_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F, col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/6, 2/6, 3/6, 4/6, 5/6), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 7/8), 
         y1=c(1/8, 2/8, 3/8, 4/8, 5/8, 6/8, 7/8), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(0.05, 0.95, length.out=6), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
axis(4, at=seq(1/22, 21/22, length.out=8), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Recreational fishing gears studied per ecosystem component", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("15", x=0.97, y=0, font=3)
text("7", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Region faceted heat maps
#-----------------------------------------------
RegEcoPress                          <- data[, .(NrPaps= length(unique(SW.ID))),
                                             by=c("Region", "Ecosystem.component_level1", "Pressure.type")]
# Leave out the Norwegian Sea, as it only is 1 study.
RegEcoPress                          <- RegEcoPress[!Region == "Norwegian Sea",,]
rCols                                <- data.table(colcode = viridis(max(RegEcoPress$NrPaps)),
                                                   value = c(1: max(RegEcoPress$NrPaps)))

tiff(paste0(outPath, "regEcoPress_heatmaps.tiff"), width=2000, height=2000, res=100)
par(mfrow=c(5,4))
par(mar=c(1,1,4,1))

for(iLoc in c("empty", "CS - Mediterranean", "Mediterranean - non CS", "Black Sea", 
              "empty", "CS - North Sea", "North Sea - non CS", "Barents Sea",
              "empty", "CS - Western Waters", "Western Waters - non CS", "NE-Atlantic", 
              "empty", "CS - Baltic Sea", "Baltic Sea - non CS", "Global", 
              "empty2", "empty3", "empty3", "empty3")){
  
  subdat                             <- RegEcoPress[Region == iLoc,,]
  EcoPress                           <- matrix(ncol=7, nrow=11)
  colnames(EcoPress)                 <- sort(unique(RegEcoPress$Pressure.type))
  rownames(EcoPress)                 <- sort(unique(RegEcoPress$Ecosystem.component_level1))
  
  if(iLoc == "empty") {
    plot(raster(matrix(NA, 1, 1)), axes=F, ann=F, legend=F, box=F)
    axis(2, tick=F, at=seq(1/22, 21/22, length.out=11), 
         labels=rev(sort(unique(RegEcoPress$Ecosystem.component_level1))), las=1, pos=1.45, cex.axis=2.3)}
  if(iLoc == "empty2"){
    plot(c(0,1), c(0,1), axes=F, type="n", ann=F)
    gradient.rect(xleft=0.45, xright=0.55, ybottom=0.1, ytop=0.7, col=rev(rCols$colcode), gradient="y")
    text(x=0.5, y=0.85, "Number of \n retained papers", font=2, cex=3)
    text("1", x=0.59, y=0.7, font=3, cex=2)
    text("50", x=0.59, y=0.1, font=3, cex=2)}
  if(iLoc == "empty3"){
    plot(raster(matrix(NA, 1, 1)), axes=F, ann=F, legend=F, box=F)
    axis(1, tick=F, at=seq(0.07142857, 0.9285714, length.out=7), 
         labels= c("Catch & bycatch", "Discarding", "Electromagnetic input", "Input of litter", "Noise", "Physical disturbance", "Visual disturbance"), 
         las=3, pos=1.15, cex.axis=3)}
  if(iLoc %in% c("CS - Mediterranean", "Mediterranean - non CS", "Black Sea",
                 "CS - North Sea", "North Sea - non CS", "Barents Sea",
                 "CS - Western Waters", "Western Waters - non CS", "NE-Atlantic", 
                 "CS - Baltic Sea", "Baltic Sea - non CS", "Global")) {
    for(iRow in c(1:nrow(subdat))){
      subdat2                        <- subdat[iRow,]
      EcoPress[subdat2$Ecosystem.component_level1, subdat2$Pressure.type] <- subdat2$NrPaps}
    r                                <- raster(EcoPress)
    
    plot(r, axes=F, legend=F, col=subset(rCols, value %in% values(r))$colcode, box=F)
    segments(x0=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), x1=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
    segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
             y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
    segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
    segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
    title(iLoc, font.main=2, cex.main=2, line=0.5)
    axis(1, at=seq(0.07142857, 0.9285714, length.out=7), labels=rep("", 7), pos=0)
    axis(2, at=seq(1/22, 21/22, length.out=11), labels=rep("", 11), pos=0)
  } # end iLoc if loop
} # end iLoc for loop
dev.off()


#-----------------------------------------------
## Time series of retained papers
#-----------------------------------------------
YearRet                              <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year")]
YearRet2                             <- matrix(nrow=1,
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearRet2)                   <- as.character(seq(from=min(data$Year), to= max(data$Year), 1)) 
rownames(YearRet2)                   <- "Retained"

YearRet2["Retained", as.character(YearRet$Year)] <- YearRet$NrPaps
YearRet2[is.na(YearRet2)]            <- 0

tiff(paste0(outPath, "YearRet.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearRet$Year), max(YearRet$Year)), y=c(0, max(YearRet$NrPaps)), type="n", ann=F, axes=F)
lines(x=as.numeric(colnames(YearRet2)), y=YearRet2["Retained",], type="o", pch=16, lwd=2)
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearRet$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
## Time series for Ecocomp
#-----------------------------------------------
YearEco                              <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Ecosystem.component_level1")]
YearEco2                             <- matrix(nrow=length(unique(data$Ecosystem.component_level1)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearEco2)                   <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearEco2)                   <- sort(unique(data$Ecosystem.component_level1))
EcoCols                              <- data.frame(Eco = sort(unique(data$Ecosystem.component_level1)),
                                                   col = turbo(11))

for(iRow in c(1:nrow(YearEco))){
  subdat                             <- YearEco[iRow,]
  YearEco2[subdat$Ecosystem.component_level1, as.character(subdat$Year)] <- subdat$NrPaps
}
YearEco2[is.na(YearEco2)]            <- 0

tiff(paste0(outPath, "YearEco.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearEco$Year), max(YearEco$Year)), y=c(0, max(YearEco$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearEco2)){
  lines(x=as.numeric(colnames(YearEco2)), y=YearEco2[iRow,], type="o", pch=16, lwd=2, col=subset(EcoCols, Eco == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=EcoCols$col, legend=rownames(YearEco2), bty="n", title="Ecosystem component")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per Ecosystem component", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearEco$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
## Time series for Region
#-----------------------------------------------
YearReg                              <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Region")]
YearReg2                             <- matrix(nrow=length(unique(data$Region)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearReg2)                   <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearReg2)                   <- sort(unique(data$Region))
RegCols                              <- data.frame(Reg = sort(unique(data$Region)),
                                                   col = turbo(length(unique(data$Region))))

for(iRow in c(1:nrow(YearReg))){
  subdat                             <- YearReg[iRow,]
  YearReg2[subdat$Region, as.character(subdat$Year)] <- subdat$NrPaps
}
YearReg2[is.na(YearReg2)] <- 0

tiff(paste0(outPath, "YearReg.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearReg$Year), max(YearReg$Year)), y=c(0, max(YearReg$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearReg2)){
  lines(x=as.numeric(colnames(YearReg2)), y=YearReg2[iRow,], type="o", pch=16, lwd=2, col=subset(RegCols, Reg == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=RegCols$col, legend=rownames(YearReg2), bty="n", title="Region")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per Region", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearReg$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
## Time series for Pressure type
#-----------------------------------------------
YearPres                             <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Pressure.type")]
YearPres2                            <- matrix(nrow=length(unique(data$Pressure.type)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearPres2)                  <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearPres2)                  <- sort(unique(data$Pressure.type))
PresCols                             <- data.frame(Pres = sort(unique(data$Pressure.type)),
                                                   col = turbo(length(unique(data$Pressure.type))))

for(iRow in c(1:nrow(YearPres))){
  subdat                             <- YearPres[iRow,]
  YearPres2[subdat$Pressure.type, as.character(subdat$Year)] <- subdat$NrPaps
}
YearPres2[is.na(YearPres2)] <- 0

tiff(paste0(outPath, "YearPres.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearPres$Year), max(YearPres$Year)), y=c(0, max(YearPres$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearPres2)){
  lines(x=as.numeric(colnames(YearPres2)), y=YearPres2[iRow,], type="o", pch=16, lwd=2, col=subset(PresCols, Pres == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=PresCols$col, legend=rownames(YearPres2), bty="n", title="Pressure types")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per pressure type", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearPres$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()


#-----------------------------------------------
## Heatmap of relation direction for ecosystem components 
#-----------------------------------------------
RelDirEco                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Ecosystem.component_level1")]
RelDirEcomat                         <- matrix(nrow = length(unique(data$Ecosystem.component_level1)),
                                               ncol = length(unique(data$Direction.of.relationship)))
colnames(RelDirEcomat)               <- sort(unique(data$Direction.of.relationship))  
rownames(RelDirEcomat)               <- sort(unique(data$Ecosystem.component_level1))

for(iRow in c(1:nrow(RelDirEco))){
  subdat                             <- RelDirEco[iRow,]
  RelDirEcomat[subdat$Ecosystem.component_level1, subdat$Direction.of.relationship] <- subdat$NrPaps
}
r                                    <- raster(RelDirEcomat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(RelDirEcomat, na.rm=T))   

tiff(paste0(outPath, "RelDirEco_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/5, 2/5, 3/5, 4/5), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
         y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/10, 9/10, length.out=5), labels= colnames(RelDirEcomat), las=3, cex.axis=1.5)
axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(RelDirEcomat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relation per ecosystem component", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("120", x=0.97, y=0, font=3)
text("60", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Heatmap of relation direction for pressure types
#-----------------------------------------------
RelDirPres                           <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Pressure.type")]
RelDirPresmat                        <- matrix(nrow = length(unique(data$Pressure.type)),
                                               ncol = length(unique(data$Direction.of.relationship)))
colnames(RelDirPresmat)               <- sort(unique(data$Direction.of.relationship))  
rownames(RelDirPresmat)               <- sort(unique(data$Pressure.type))

for(iRow in c(1:nrow(RelDirPres))){
  subdat                             <- RelDirPres[iRow,]
  RelDirPresmat[subdat$Pressure.type, subdat$Direction.of.relationship] <- subdat$NrPaps
}
r                                    <- raster(RelDirPresmat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(RelDirPresmat, na.rm=T))   

tiff(paste0(outPath, "RelDirPres_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/5, 2/5, 3/5, 4/5), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), 
         y1=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/10, 9/10, length.out=5), labels= colnames(RelDirEcomat), las=3, cex.axis=1.5)
axis(4, at=seq(1/14, 13/14, length.out=7), labels= rev(rownames(RelDirPresmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relation per pressure type", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("170", x=0.97, y=0, font=3)
text("85", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Heatmap of relation direction for Region
#-----------------------------------------------
RelDirReg                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Region")]
RelDirRegmat                         <- matrix(nrow = length(unique(data$Region)),
                                               ncol = length(unique(data$Direction.of.relationship)))
colnames(RelDirRegmat)               <- sort(unique(data$Direction.of.relationship))  
rownames(RelDirRegmat)               <- sort(unique(data$Region))

for(iRow in c(1:nrow(RelDirReg))){
  subdat                             <- RelDirReg[iRow,]
  RelDirRegmat[subdat$Region, subdat$Direction.of.relationship] <- subdat$NrPaps
}
r                                    <- raster(RelDirRegmat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(RelDirRegmat, na.rm=T))   

tiff(paste0(outPath, "RelDirReg_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 8))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/5, 2/5, 3/5, 4/5), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/13, 2/13, 3/13, 4/13, 5/13, 6/13, 7/13, 8/13, 9/13, 10/13, 11/13, 12/13), 
         y1=c(1/13, 2/13, 3/13, 4/13, 5/13, 6/13, 7/13, 8/13, 9/13, 10/13, 11/13, 12/13), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/10, 9/10, length.out=5), labels= colnames(RelDirRegmat), las=3, cex.axis=1.5)
axis(4, at=seq(1/26, 25/26, length.out=13), labels= rev(rownames(RelDirRegmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relation per Region", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("85", x=0.97, y=0, font=3)
text("42", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Heatmap of relation direction for Fishery type
#-----------------------------------------------
RelDirFish                           <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Fishery.type")]
RelDirFishmat                        <- matrix(nrow = length(unique(data$Fishery.type)),
                                               ncol = length(unique(data$Direction.of.relationship)))
colnames(RelDirFishmat)               <- sort(unique(data$Direction.of.relationship))  
rownames(RelDirFishmat)               <- sort(unique(data$Fishery.type))

for(iRow in c(1:nrow(RelDirFish))){
  subdat                             <- RelDirFish[iRow,]
  RelDirFishmat[subdat$Fishery.type, subdat$Direction.of.relationship] <- subdat$NrPaps
}
r                                    <- raster(RelDirFishmat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(RelDirFishmat, na.rm=T))   

tiff(paste0(outPath, "RelDirFish_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/5, 2/5, 3/5, 4/5), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/4, 2/4, 3/4), 
         y1=c(1/4, 2/4, 3/4), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/10, 9/10, length.out=5), labels= colnames(RelDirFishmat), las=3, cex.axis=1.5)
axis(4, at=seq(1/8, 7/8, length.out=4), labels= rev(rownames(RelDirFishmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relation per fishery type", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("270", x=0.97, y=0, font=3)
text("135", x=0.97, y=0.15, font=3)
dev.off()

#-----------------------------------------------
## Heatmaps of relation direction for ecosystem components and pressure types.
#-----------------------------------------------
RelDirEcoPres                        <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Ecosystem.component_level1", "Pressure.type")]
RelDirEcoPres$Pressure.type          <- ifelse(RelDirEcoPres$Pressure.type == "Catch_and_bycatch", "Catch & bycatch",
                                               ifelse(RelDirEcoPres$Pressure.type == "Physical disturbance of the seabed", "Physical disturbance", RelDirEcoPres$Pressure.type))
rCols                                <- data.table(colcode = viridis(max(RelDirEcoPres$NrPaps)),
                                                   value = c(1:max(RelDirEcoPres$NrPaps)))

tiff(paste0(outPath, "RelDirEcoPres_heatmap.tiff"), width= 2000, height = 500, res = 100)
par(mfrow=c(1,7))
par(mar=c(10, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=11)), axes=F, box=F)
axis(2, tick=F, at=seq(1/22, 21/22, length.out=11), labels=rev(sort(unique(data$Ecosystem.component_level1))), las=1, pos=1.2, cex.axis=1.5)


for(iType in unique(RelDirEcoPres$Pressure.type)[1:5]){
  subdat                             <- RelDirEcoPres[Pressure.type == iType,,]
  
  RDEPmat                            <- matrix(nrow = length(unique(data$Ecosystem.component_level1)),
                                               ncol = length(unique(data$Direction.of.relationship)))
  colnames(RDEPmat)                  <- sort(unique(data$Direction.of.relationship))  
  rownames(RDEPmat)                  <- sort(unique(data$Ecosystem.component_level1))
  
  for(iRow in c(1:nrow(subdat))){
    subdat2                          <- subdat[iRow,]
    RDEPmat[subdat2$Ecosystem.component_level1, subdat2$Direction.of.relationship] <- subdat2$NrPaps
  }
  r                                  <- raster(RDEPmat)
  
  par(mar=c(10, 1, 10, 1))
  plot(r, axes=F,
       col=subset(rCols, value %in% values(r))$colcode, legend=F, box=F)
  title(iType, font.main=2, cex.main=2, line=1.75)
  title(paste0("(n = ", sum(subdat$NrPaps), ")"), font.main=3, cex.main=1.25, line=0.5)
  segments(x0=seq(0, 1, length.out=ncol(RDEPmat)+1), x1=seq(0, 1, length.out=ncol(RDEPmat)+1), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
  segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
           y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
  segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
  segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
  axis(1, at=seq((1/ncol(RDEPmat))/2, 1-((1/ncol(RDEPmat))/2), length.out=ncol(RDEPmat)), labels= colnames(RDEPmat), las=3, cex.axis=1.5, pos=0)
  #axis(2, at=seq(1/22, 21/22, length.out=11), labels=rep("",11), pos=0)
}

par(mar=c(10, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=11)), axes=F, box=F)
axis(4, tick=F, at=seq(1/22, 21/22, length.out=11), labels=rev(rownames(RDEPmat)), las=1, pos=-0.15, cex.axis=1.5)

par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,0))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relationship per pressure type and ecosystem components", cex.main=2, font.main=2, line=0)
gradient.rect(xleft=0.9, xright=1.01, ybottom=0.05, ytop=0.15, col=rCols$colcode, gradient="x")
text("Number of \n papers retained", x=0.955, y=0.2, font=4, cex=1.3)
text("1", x=0.895, y=0.1, font=3)
text("75", x=1.015, y=0.1, font=3)
text(x=0, y=0.25, font=2, "Noise:", pos=4, cex=1.5)
text(x=0.005, y=0.21, "- 1x negative relation for seabirds", pos=4)
text(x=0.005, y=0.17, "- 1x multiple relations for marine mammals", pos=4)
text(x=0, y=0.12, font=2, "Visual disturbance:", pos=4, cex=1.5)
text(x=0.005, y=0.08, "- 1x positive relation for teleost fish", pos=4)
dev.off()


#-----------------------------------------------
## Heatmaps of relation direction for response variable categories.
#-----------------------------------------------
RelDirRVC                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Direction.of.relationship", "Response.variable_category")]

RelDirRVCmat                         <- matrix(nrow = length(unique(data$Response.variable_category)),
                                               ncol = length(unique(data$Direction.of.relationship)))
colnames(RelDirRVCmat)               <- sort(unique(data$Direction.of.relationship))  
rownames(RelDirRVCmat)               <- sort(unique(data$Response.variable_category))

for(iRow in c(1:nrow(RelDirRVC))){
  subdat                             <- RelDirRVC[iRow,]
  RelDirRVCmat[subdat$Response.variable_category, subdat$Direction.of.relationship] <- subdat$NrPaps
}
r                                    <- raster(RelDirRVCmat)
rCols                                <- viridis(n=max(RelDirRVC$NrPaps))

tiff(paste0(outPath, "RelDirRVC_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(15, 3, 7, 20))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=seq(0, 1, length.out=ncol(RelDirRVCmat)+1), x1=seq(0, 1, length.out=ncol(RelDirRVCmat)+1), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=seq(0,1,length.out=nrow(RelDirRVCmat)+1), y1=seq(0,1, length.out=nrow(RelDirRVCmat)+1), col="lightgrey", lty=2, lwd=0.8)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
axis(1, at=seq((1/ncol(RelDirRVCmat))/2, 1-((1/ncol(RelDirRVCmat))/2), length.out=ncol(RelDirRVCmat)), labels= colnames(RelDirRVCmat), las=3, cex.axis=1.5, pos=0)
axis(4, at=seq((1/nrow(RelDirRVCmat))/2, 1-((1/nrow(RelDirRVCmat))/2), length.out=nrow(RelDirRVCmat)), labels = rev(rownames(RelDirRVCmat)), las=1, cex.axis=1.5, pos=1)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,0))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Direction of relationship per reponse variable (Category)", cex.main=2, font.main=2, line=0)
gradient.rect(xleft=0.75, xright=0.9, ybottom=0.05, ytop=0.15, col=rCols, gradient="x")
text("Number of \n papers retained", x=0.825, y=0.2, font=4, cex=1.3)
text("1", x=0.745, y=0.1, font=3)
text("135", x=0.915, y=0.1, font=3)
dev.off()

#-----------------------------------------------
## Overview of species studied: class
#-----------------------------------------------
SpecsStudied                         <- dat1[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("class", "Ecosystem.component_level1")]
dat1$class                           <- ifelse(is.na(dat1$class)==T, "not specified", dat1$class)

SpecsStudmat                         <- matrix(ncol = length(unique(dat1$Ecosystem.component_level1)),
                                               nrow = length(unique(dat1$class)))
rownames(SpecsStudmat)               <- sort(unique(dat1$class))  
colnames(SpecsStudmat)               <- sort(unique(dat1$Ecosystem.component_level1))

for(iRow in c(1:nrow(SpecsStudied))){
  subdat                             <- SpecsStudied[iRow,]
  SpecsStudmat[subdat$class, subdat$Ecosystem.component_level1] <- subdat$NrPaps
}
r                                    <- raster(SpecsStudmat)

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(SpecsStudmat, na.rm=T))   

tiff(paste0(outPath, "SpecsStudmat_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(15, 1, 5, 15))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11, 10/11), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1:32)/33, y1=c(1:32)/33, lty=2, col="lightgrey", lwd=0.8)
axis(4, at=seq(1/66, 65/66, length.out=33), labels= rev(rownames(SpecsStudmat)), las=1, cex.axis=1, pos=1)
axis(1, at=seq(1/22, 21/22, length.out=11), labels= colnames(SpecsStudmat), las=3, pos=0, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Studied classes per ecosystem component", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.9, xright=0.95, ybottom=0, ytop=0.2, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.925, y=0.25, font=4, cex=1.3)
text("1", x=0.97, y=0.2, font=3)
text("160", x=0.97, y=0, font=3)
text("80", x=0.97, y=0.1, font=3)
dev.off()


#-----------------------------------------------
## Overview of species studied: valid_name
#-----------------------------------------------
SpecsStudied                         <- dat1[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("valid_name")]
SpecsStudied                         <- SpecsStudied[order(NrPaps, decreasing=T),,]


#-----------------------------------------------



#-----------------------------------------------
## Creating tables reporting on inclusion/exclusion 
#-----------------------------------------------
# Check whether papers got assigned multiple exclusion criteria
data_excl          <- subset(data_allScreened, !is.na(Exclusion.Criteria))

length(unique(data_excl$SW.ID)) #182 papers excluded
excl_ID            <- unique(data_excl$SW.ID)
nrow(data_excl) #with 183 rows, so some have multiple
no_excl_cri        <- aggregate(Exclusion.Criteria ~ SW.ID, data_excl, function(x) length(unique(x))) 
head(no_excl_cri[order(-no_excl_cri$Exclusion.Criteria),])#SW4_1550 has two

## Check duplicates
excl_dupl          <- data_excl[duplicated(data_excl$SW.ID),] #again SW4_1550, so no real duplicates to remove

## Table with number and percentages of papers excluded during data extraction
tab              <- aggregate(SW.ID ~ Exclusion.Criteria, data_excl, function(x) length(unique(x)))
names(tab)       <- c("Crit","Times")
tab$perc         <- tab$Times / sum(tab$Times) * 100
tab              <- rbind(tab, data.frame(Crit = "EXCLUDE multiple",
                                          Times = nrow(data_excl)-length(excl_ID2),
                                          perc = (nrow(data_excl)-length(excl_ID2))/sum(tab$Times) * 100))
tab              <- rbind(tab, data.frame(Crit = "EXCLUDE unique",
                                          Times = length(unique(data_excl$SW.ID)),
                                          perc = length(unique(data_excl$SW.ID)) / length(unique(data_allScreened$SW.ID)) *100))

## To do the same for included papers, split the tasks first
Tasks             <- tstrsplit(data$WP4.task, split=" _ ")
Tasks2            <- data.table(SW.ID = rep(data$SW.ID, 3), Task  = c(Tasks[[1]], Tasks[[2]], Tasks[[3]])) 
Tasks2            <- na.omit(Tasks2)

## Calculate and number and percentages for inclusion
RetTask           <- Tasks2[, .(NrPaps = length(unique(SW.ID))), by = c("Task")]
RetTask           <- RetTask[order(Task, decreasing=F),,]
RetTask$perc      <- RetTask$NrPaps / sum(RetTask$NrPaps) * 100
names(RetTask)    <- names(tab)
RetTask           <- rbind(RetTask, data.frame(Crit = "INCLUDE multiple",
                                          Times = sum(RetTask$Times) - length(unique(data$SW.ID)),
                                          perc = (sum(RetTask$Times) - length(unique(data$SW.ID)))/sum(RetTask$Times) * 100))
RetTask           <- rbind(RetTask, data.frame(Crit = "INCLUDE unique",
                                          Times = length(unique(data$SW.ID)),
                                          perc = length(unique(data$SW.ID)) / length(unique(data_allScreened$SW.ID)) *100))

## Combine the two tables and save
tab               <- rbind(tab, RetTask)
tab$perc          <- round(tab$perc,1)

write.csv(tab, file = paste0(outPath,"summary table data extraction exclusion and inclusion.csv"), row.names = FALSE)



#-----------------------------------------------
## Table of quality scores  
#-----------------------------------------------

dataUnique        <- data[!duplicated(data$SW.ID),]

tab               <- aggregate(SW.ID ~ Quality...Spatial..relative.1.3., dataUnique, function(x) length(unique(x)))
names(tab)        <- c("Quality.spatial","NrPaps")
tab$PercPaps      <- tab$NrPaps / sum(tab$NrPaps) * 100

write.csv(tab, file=paste0(outPath, "QualitySpatialPercentage.csv"), row.names = FALSE)


tab               <- aggregate(SW.ID ~ Quality...Temporal, dataUnique, function(x) length(unique(x)))
names(tab)        <- c("Quality.temporal","NrPaps")
tab$PercPaps      <- tab$NrPaps / sum(tab$NrPaps) * 100

write.csv(tab, file=paste0(outPath, "QualityTemporalPercentage.csv"), row.names = FALSE)


tab               <- aggregate(SW.ID ~ Quality...Methods, dataUnique, function(x) length(unique(x)))
names(tab)        <- c("Quality.methods","NrPaps")
tab$PercPaps      <- tab$NrPaps / sum(tab$NrPaps) * 100

write.csv(tab, file=paste0(outPath, "QualityMethodsPercentage.csv"), row.names = FALSE)


#-----------------------------------------------
## Barplot of quality scores by region
#-----------------------------------------------

dataUnique        <- data[!duplicated(data$SW.ID),]

## Spatial
Qual              <- aggregate(SW.ID ~ Region + Quality...Spatial..relative.1.3., dataUnique, function(x) length(unique(x)))
names(Qual)       <- c("Region","Quality","NrPaps")
Qual$Region       <- factor(Qual$Region, levels=c("CS - Mediterranean","CS - Western Waters","CS - North Sea", "CS - Baltic Sea",
                                                  "Mediterranean - non CS","Western Waters - non CS","North Sea - non CS","Baltic Sea - non CS",
                                                  "Barents Sea","Black Sea","NE-Atlantic","Global"))

p <- ggplot(Qual, aes(Region,NrPaps, fill=as.factor(Quality))) +
  geom_bar(position = "fill",stat="identity") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values=viridis(3), name= "Quality spatial") +
  labs(x="Region", y="Proportion of papers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
print(p)
ggsave("QualitySpatial.tiff", p, path=outPath, width = 8, height = 5)

## Temporal
Qual              <- aggregate(SW.ID ~ Region + Quality...Temporal, dataUnique, function(x) length(unique(x)))
names(Qual)       <- c("Region","Quality","NrPaps")
Qual$Region       <- factor(Qual$Region, levels=c("CS - Mediterranean","CS - Western Waters","CS - North Sea", "CS - Baltic Sea",
                                                  "Mediterranean - non CS","Western Waters - non CS","North Sea - non CS","Baltic Sea - non CS",
                                                  "Barents Sea","Black Sea","NE-Atlantic","Global"))

p <- ggplot(Qual, aes(Region,NrPaps, fill=as.factor(Quality))) +
  geom_bar(position = "fill",stat="identity") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values=viridis(3), name= "Quality temporal") +
  labs(x="Region", y="Proportion of papers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
print(p)
print(p)
ggsave("QualityTemporal.tiff", p, path=outPath, width = 8, height = 5)

## Methods
Qual              <- aggregate(SW.ID ~ Region + Quality...Methods, dataUnique, function(x) length(unique(x)))
names(Qual)       <- c("Region","Quality","NrPaps")
Qual$Region       <- factor(Qual$Region, levels=c("CS - Mediterranean","CS - Western Waters","CS - North Sea", "CS - Baltic Sea",
                                                  "Mediterranean - non CS","Western Waters - non CS","North Sea - non CS","Baltic Sea - non CS",
                                                  "Barents Sea","Black Sea","NE-Atlantic","Global"))

p <- ggplot(Qual, aes(Region,NrPaps, fill=as.factor(Quality))) +
  geom_bar(position = "fill",stat="identity") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values=viridis(3), name= "Quality methods") +
  labs(x="Quality methods", y="Proportion of papers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
print(p)
print(p)
ggsave("QualityMethods.tiff", p, path=outPath, width = 8, height = 5)

