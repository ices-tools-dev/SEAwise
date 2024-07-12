#####################################################################################################################-
#####################################################################################################################-
#
#     Script to explore and produce output for the general results section of the manuscript.
#
#     Note that this includes both output to be put in both the manuscript and the Supplement,
#     with output for the latter linked to the general results section in the manuscript.
#
#     By Esther Beukhof, including code written by Karin van der Reijden and Elliot J. Brown
#     June 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths ----
#-----------------------------------------------#

library(data.table)
library(ggplot2)
library(viridis)
library(ggsankey)
library(ggthemes)
library(dplyr)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(plotrix)
library(colorspace)
library(openxlsx)
library(worms)
library(tidyr)
library(stringr)
library(ggforce)
library(patchwork)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS/"


#-----------------------------------------------#
# Read in data ----
#
#  info:
#  This section depends on the processed data file produced in step 1.
# 
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))


# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                              ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                     ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                            ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

data_allScreened$RegionSEAwise        <- data_allScreened$Region
data_allScreened$Region               <- with(data_allScreened, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                                       ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                              ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                                     ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

# Load fate of papers
load(paste0(datPath, "FatePapers.Rdata"))


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# PART I - General plots across all papers ----
#-----------------------------------------------#

#-----------------------------------------------#
## Table excluded papers during screening ----
#-----------------------------------------------#

datExclScr                           <- subset(FatePapers, Screening.Fate %in% "Excluded")
datExclScr                           <- cSplit(datExclScr, "Screening.Exclusion.Code", " _ ", "long")
exclScre                             <- aggregate(SW.ID ~ Screening.Exclusion.Code, datExclScr, function(x) length(unique(x)))
names(exclScre)[2]                   <- "NrPaps"
exclScre$totalNrPaps                 <- sum(exclScre$NrPaps)
exclScre$propPaps                    <- round(exclScre$NrPaps / exclScre$totalNrPaps, 2)

write.csv(exclScre, paste0(outPath,"nr and prop papers excluded screening.csv"), row.names = FALSE)



#-----------------------------------------------#
## Table excluded papers during data extraction ----
#-----------------------------------------------#

datExclDat                           <- subset(FatePapers, Extraction.Fate %in% "Excluded")
datExclDat                           <- cSplit(datExclDat, "Extraction.Exclusion.Code", " _ ", "long")
exclDat                              <- aggregate(SW.ID ~ Extraction.Exclusion.Code, datExclDat, function(x) length(unique(x)))
names(exclDat)[2]                    <- "NrPaps"
exclDat$totalNrPaps                  <- sum(exclDat$NrPaps)
exclDat$propPaps                     <- round(exclDat$NrPaps / exclDat$totalNrPaps, 2)

write.csv(exclDat, paste0(outPath,"nr and prop papers excluded data extraction.csv"), row.names = FALSE)



#-----------------------------------------------#
## Table number of papers per year ----
#-----------------------------------------------#

nrPaps                               <- aggregate(SW.ID ~ Year, data, function(x) length(unique(x)))

write.csv(nrPaps, paste0(outPath,"nr papers per year.csv"), row.names = FALSE)



#-----------------------------------------------#
## Barplot by year of number of studies ----
#-----------------------------------------------#

YearRet                          <- data[,.(NrPaps = length(unique(SW.ID))),
                                         by = c("Year")]
YearRet2                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)))
YearRet2                         <- merge(YearRet2, YearRet, by="Year", all.x=TRUE)
YearRet2$col                     <- with(YearRet2, ifelse(Year == 2022, "incomplete","complete"))

p <- ggplot(YearRet2, aes(Year, NrPaps, fill=col)) +
  geom_bar(stat="identity") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 43), n.breaks = 10) +
  scale_fill_manual(values = rev(viridis(3)[-3])) +
  labs(x="Publication year", y="Number of papers") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 3, unit = "mm")),
        panel.grid.minor = element_blank())
print(p)

ggsave("YearBar.png", p, path=outPath, width = 3.2, height = 3.2)


## Try colouring by study type

# Calculate number of study types
Study                              <- data[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Study.type")]
Study
sum(Study$NrPaps) #546, so a bit more than the 527 unique papers, meaning that a several studies have multiple study types
## In that case, we cannot colour by study type, as we only want each paper the be represented once
## Try it anyway, just to get a feeling of how things have changed (if they did)

YearStu                          <- data[,.(NrPaps = length(unique(SW.ID))),
                                         by = c("Year","Study.type")]
YearStu2                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)))
YearStu2                         <- merge(YearStu2, YearStu, by="Year", all.x=TRUE)

p <- ggplot(YearStu2, aes(Year, NrPaps, fill=Study.type)) +
  geom_bar(stat="identity") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 43), n.breaks = 10) +
  scale_fill_manual(values = brewer.pal(7,"Paired"), name="Study type") +
  labs(x="Publication year", y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 3, unit = "mm")),
        panel.grid.minor = element_blank(),
        legend.position = c(0.25,0.7),
        legend.text = element_text(size=8),
        legend.title = element_text(size = 10),
        legend.key.height = unit(0.75,"line"))
print(p)

ggsave("YearBarStudy.png", p, path=outPath, width = 5, height = 3.2)


## Try adding a second axis with cumulative number of papers

# Calculate cumulative number of papers
YearRet                          <- data[,.(NrPaps = length(unique(SW.ID))),
                                         by = c("Year")]
YearRet                          <- YearRet[order(YearRet$Year),]
YearRet$cumNrPaps                <- cumsum(YearRet$NrPaps)
YearRet2                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)))
YearRet2                         <- merge(YearRet2, YearRet, by="Year", all.x=TRUE)
YearRet2$col                     <- with(YearRet2, ifelse(Year == 2022, "incomplete","complete"))
coeff                            <- 10

p <- ggplot(YearRet2, aes(x=Year)) +
  geom_bar(aes(y=NrPaps, fill=col), stat="identity") +
  geom_point(aes(y=cumNrPaps / coeff), size=0.5) +
  geom_line(aes(y=cumNrPaps / coeff), linewidth=0.1) +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 55), n.breaks = 10,
                     sec.axis = sec_axis(~.*coeff, name="Cumulative number of papers")) +
  scale_fill_manual(values = rev(viridis(3)[-3])) +
  labs(x="Year", y="Number of papers") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y.right = element_text(margin = margin(l = 3, unit = "mm")),
        panel.grid.minor = element_blank())
print(p)

ggsave("YearBarCum.png", p, path=outPath, width = 3.5, height = 3.2)


#-----------------------------------------------#
## Sankey diagram of fate of all records ----
#-----------------------------------------------#

#===#
# Data cleaning
#====#

## Create columns for fate for screening and extraction phase by code
FatePapers$Screening.Result        <- with(FatePapers, ifelse(Screening.Fate %in% "Included","INCLUDE",Screening.Exclusion.Code))
FatePapers$Extraction.Result       <- with(FatePapers, ifelse(Extraction.Fate %in% "Included","INCLUDE",Extraction.Exclusion.Code))

## Create long versions of both screening and full-text fates for each record
screenFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Screening.Result")],
                         splitCols = "Screening.Result",
                         sep = " _ ",
                         direction = "long")

ExtractFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Extraction.Result")],
                          splitCols = "Extraction.Result",
                          sep = " _ ",
                          direction = "long")
## Merge screening and extraction fates
fatelong <- merge(screenFatelong, ExtractFatelong, by = "SW.ID", all = TRUE)

fatelong$Extraction.Result[is.na(fatelong$Extraction.Result)] <- "EXCLUDE"
# fatelong$Extraction.Result <- with(fatelong, ifelse(!Extraction.Result %in% "INCLUDE","Excluded from Review",Extraction.Result))

fatelong$SearchResults <- rep("Search Results", nrow(fatelong))
fatelong$UltimateFate <- with(fatelong, ifelse(Extraction.Result %in% "INCLUDE","INCLUDE","EXCLUDE"))


sfate <- make_long(fatelong, SearchResults, Screening.Result, Extraction.Result, UltimateFate)

#===#
# Create sankey diagram
#====#
## Build sankey
sankey <- ggplot(sfate,
                 mapping = aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(node),
                               label = node)) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +  
  geom_sankey_label(size=7, colour="white") +
  scale_fill_manual(values=viridis(11)) +
  theme_few()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=20),
        legend.position = "none")

## View Sankey
sankey

## Save sankey
ggsave("SankeyFateRecords.tiff", sankey, path=outPath,
       width = 400,
       height = 200,
       units = "mm")

ggsave("SankeyFateRecords.png", sankey, path=outPath,
       width = 400,
       height = 200,
       units = "mm")


#===#
# Create alternative sankey diagram using ggforce
#====#





#-----------------------------------------------#
## Barplot for ecosystem component ----
#-----------------------------------------------#

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Ecosystem.component_level1]
EcoComp                               <- EcoComp[order(NrPaps),,]


png(paste0(outPath, "EcoComp.png"), width=1000, height=750, res=100)
par(mar=c(5, 15, 2, 2))
b                                     <- barplot(EcoComp$NrPaps, horiz=TRUE, axes=F, xlim=c(0,230), col=viridis(3)[2])
box()
axis(2, at=b, labels=EcoComp$Ecosystem.component_level1 , las=1, cex.axis=2)
axis(1, at= seq(0,220, 25), labels=seq(0, 220, 25), cex.axis=1.5)
text(x=180, y=0, paste0("Total number of unique papers: ", length(unique(data$SW.ID))))
text(x=EcoComp$NrPaps+7, y=b, EcoComp$NrPaps, cex = 1.3)
axis(1, at=110, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


# ggplot version
p <- ggplot(EcoComp, aes(NrPaps,reorder(Ecosystem.component_level1,NrPaps))) +
  geom_bar(stat="identity", fill=viridis(3)[2]) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 0), limits = c(0, 220)) +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 3, unit = "mm")))
print(p)

ggsave("EcoComp2.png", p, path=outPath, width = 4.5, height = 3)





#-----------------------------------------------#
## Barplot for Response variable categories ----
#-----------------------------------------------#

# First manually combine Survival and Mortality studies (all as Mortality)
ResVarCats                            <- data
# ResVarCats$Response.variable_category <- with(ResVarCats, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))                            

ResVarCats                            <- ResVarCats[, .(NrPaps = length(unique(SW.ID))),
                                              by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

png(paste0(outPath, "ResVarCats.png"), width=1250, height=750, res=100)
par(mar=c(5, 25, 2, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,265), col=viridis(3)[2])
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=2)
axis(1, at= seq(0,250, 25), labels=seq(0, 250, 25), cex.axis=1.5)
text(x=162, y=0, paste0("Total number of unique papers: ", length(unique(data$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+7.5, y=b, ResVarCats$NrPaps, cex = 1.3)
axis(1, at=125, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


#-----------------------------------------------#
## Barplot for pressure types ----
#-----------------------------------------------#

Pressure                              <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Pressure.type]
Pressure                              <- Pressure[order(NrPaps, decreasing = TRUE),,]
Pressure                              <- Pressure[order(NrPaps),,]

png(paste0(outPath, "Pressures.png"), width=1200, height=750, res=100)
par(mar=c(5, 28, 2, 2))
b                                     <- barplot(Pressure$NrPaps, horiz=TRUE, axes=F, xlim=c(0,350), col=viridis(3)[2])
box()
axis(2, at=b, labels=Pressure$Pressure.type, las=1, cex.axis=2)
axis(1, at= seq(0,350, 50), labels=seq(0, 350, 50), cex.axis=1.5)
text(x=192, y=0, paste0("Total number of unique papers: ", length(unique(data$SW.ID))), pos=4)
text(x=Pressure$NrPaps+12, y=b, Pressure$NrPaps, cex = 1.3)
axis(1, at=190, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


#-----------------------------------------------#
## Barplot of fishing gears studied - Comm only ----
#-----------------------------------------------#

Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears                                <- subset(Gears, Fishery.type %in% "Commercial")
Gears$Gear_level1                    <- with(Gears, ifelse(is.na(Gear_level1),"Unspecified",Gear_level1))
Gears                                <- Gears[order(NrPaps),,]

tiff(paste0(outPath, "GearsComm.tiff"), width=1050, height=750, res=100)
par(mar=c(5, 15, 2, 2))
b                                     <- barplot(Gears$NrPaps, horiz=TRUE, axes=F, xlim=c(0,350), col=viridis(3)[2])
box()
axis(2, at=b, labels=Gears$Gear_level1, las=1, cex.axis=2)
axis(1, at= seq(0,350, 50), labels=seq(0, 350, 50), cex.axis=1.5)
# text(x=192, y=0, paste0("Total number of unique papers: ", length(unique(data$SW.ID))), pos=4)
text(x=Gears$NrPaps+10, y=b, Gears$NrPaps, cex = 1.3)
axis(1, at=190, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


#-----------------------------------------------#
## Barplots of fishing gears studied - Comm + Recr only ----
#-----------------------------------------------#
Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears                                <- subset(Gears, Fishery.type %in% c("Commercial","Recreational"))
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
GearLevels                           <- unique(Gears$Fishery.type)
uniquePaps                           <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = "Fishery.type"]

png(paste0(outPath, "GearsStudiedCommRecr.png"), width= 1500, height = 1000, res = 100)
par(mfrow=c(1,2))
par(mar=c(18,8,7,1))
for(iType in seq_along(GearLevels)){
  subdat                             <- Gears[Gears$Fishery.type == GearLevels[iType],]
  subdat                             <- subdat[order(NrPaps, decreasing=T)]
  b                                  <- barplot(subdat$NrPaps, axes=F, ylim=c(0, max(subdat$NrPaps*1.05)), col=viridis(3)[c(2,3)][iType])
  title(GearLevels[iType], cex.main=3, font=2, line=3)
  title(paste0("(n = ", uniquePaps$NrPaps[iType], ")"), font.main=3, cex.main=2, line=1.5)
  axis(1, at=b, labels=subdat$Gear_level1, las=3, cex.axis=2.5)
  axis(2, las=1, cex.axis=2)
  text(x=b, y=max(subdat$NrPaps)*0.035, labels=subdat$NrPaps, cex=1.5, font=3)
  box()
  if(iType==1){
    axis(2, tick=F, at=(max(subdat$NrPaps)*1.05)/2, labels="Number of papers",
         line=4.5, cex.axis=3)}
}
par(fig=c(0,1,0,1), new=TRUE)
par(mar=c(2,2,6,2))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title("The different fishing gears studied", line=0, cex.main=3, font.main=2)
dev.off()



#-----------------------------------------------#
## Table Sampling method by Ecosystem component ----
#-----------------------------------------------#

tab <- table(data_allScreened$Sampling.Method.used.for.data.collection,data_allScreened$Response.variable_category)

write.table(tab, file=paste0(outPath, "sampl method ecosystem comp.csv"), sep=";")



#-----------------------------------------------#
## Heatmap of ecosystem component level 1 vs pressure type level 1 ----
#-----------------------------------------------#

EcoPress                             <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Pressure.type")]
EcoPressExp                          <- expand.grid(Ecosystem.component_level1=EcoPress$Ecosystem.component_level1, 
                                                    Pressure.type=EcoPress$Pressure.type)
EcoPressExp                          <- EcoPressExp[!duplicated(EcoPressExp),]
EcoPressExp                          <- merge(EcoPress, EcoPressExp, by=c("Ecosystem.component_level1","Pressure.type"), all=TRUE)
# EcoPressExp$NrPaps[is.na(EcoPressExp$NrPaps)] <- 0

p <- ggplot(EcoPressExp, aes(x=Pressure.type, y=Ecosystem.component_level1, fill=NrPaps)) +
  geom_tile() +
  scale_fill_viridis_c(name="No. papers", na.value = "transparent") +
  labs(x="Pressure type", y="Ecosystem component") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size=12))
print(p)

ggsave("EcoPress_heatmap.png", p, path=outPath, width = 9, height = 7)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Regional differences ----
#-----------------------------------------------#

#-----------------------------------------------#
## Table with number and proportion of papers by region -----
#-----------------------------------------------#

Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]   
Regions$Prop                          <- Regions$NrPaps / sum(Regions$NrPaps)

sum(Regions$NrPaps) #higher than number of unique papers in data, so some papers cover multiple regions

write.csv(Regions, paste0(outPath,"nr and prop papers region.csv"), row.names = FALSE)


#-----------------------------------------------#
## Barplot with number of papers by region -----
#-----------------------------------------------#

Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
Regions                               <- Regions[, .(NrPaps = sum(NrPaps)), by="Region"]
Regions                               <- Regions[order(NrPaps, decreasing = F),,]

sum(Regions$NrPaps) #542, suggesting that, out of 527 papers, several of them studied multiple regions


tiff(paste0(outPath, "Regions.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(Regions$NrPaps, horiz=TRUE, axes=F, xlim=c(0,max(Regions$NrPaps+20)), col=viridis(3)[2])
box()
axis(2, at=b, labels=Regions$Region, las=1, cex.axis=2)
axis(1, at= seq(0,max(Regions$NrPaps), 50), labels=seq(0, max(Regions$NrPaps), 50), cex.axis=1.5)
# title(main="Number of papers per region", cex.main=1.5, font.main=2)
text(x=175, y=0, paste0("Total number of unique papers ", length(unique(data$SW.ID))), pos=4)
text(x=Regions$NrPaps + 9, y=b, Regions$NrPaps, cex=1.3)
axis(1, at=max(Regions$NrPaps)/2, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


#-----------------------------------------------#
## Barplot with EcoComp and number of papers by region -----
#-----------------------------------------------#

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Region","Ecosystem.component_level1")]

EcoOrder                              <- data[, .(NrPaps = length(unique(SW.ID))), by = "Ecosystem.component_level1"]
EcoOrder                              <- EcoOrder[order(NrPaps, decreasing = T),,]

EcoComp$Ecosystem.component_level1    <- factor(EcoComp$Ecosystem.component_level1, levels = EcoOrder$Ecosystem.component_level1)

RegionOrder                           <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
RegionOrder                           <- RegionOrder[order(NrPaps, decreasing = T),,]

EcoComp$Region                        <- factor(EcoComp$Region, levels = RegionOrder$Region)

## Option1
p <- ggplot(EcoComp, aes(Ecosystem.component_level1, NrPaps, fill=Region)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0,1), n.breaks = 10) +
  scale_fill_brewer(palette = "Set1") +
  labs(x="Ecosystem component", y="Number of papers") +
  theme_bw() +
  theme(legend.position = c(0.9,0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
print(p)

ggsave("EcoCompRegion1.tiff", p, path=outPath, width = 10, height = 5)


## Option2
p <- ggplot(EcoComp, aes(NrPaps, Ecosystem.component_level1, fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_discrete(limits=rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of  papers", y="Ecosystem component") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  facet_wrap(~Region)
print(p)
ggsave("EcoCompRegion2.tiff", p, path=outPath, width=12, height = 8)


## Option3
p <- ggplot(EcoComp, aes(NrPaps, Region, fill=Region)) +
  geom_bar(stat="identity") +
  scale_x_continuous(n.breaks = 6) +
  scale_y_discrete(limits=rev) +
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of papers", y="Region") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(~Ecosystem.component_level1)
print(p)
ggsave("EcoCompRegion3.tiff", p, path=outPath, width=9, height = 6)


#-----------------------------------------------#
## Time series by region -----
#-----------------------------------------------#

Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Region","Year")]
RegionsExpand                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)), 
                                                     Region=unique(Regions$Region))
Regions                               <- merge(RegionsExpand, Regions, by=c("Region","Year"), all.x = TRUE)

RegionOrder                           <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
RegionOrder                           <- RegionOrder[order(NrPaps, decreasing = T),,]

Regions$Region                        <- factor(Regions$Region, levels = RegionOrder$Region)

p <- ggplot(Regions, aes(Year,NrPaps)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = 8) +
  # scale_colour_brewer(palette = "Paired") +
  labs(y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  facet_wrap(~Region)
print(p)
ggsave("RegionYear.tiff", p, path=outPath, width=10, height = 6)



#-----------------------------------------------#
## Barplot with Pressure and number of papers by region -----
#-----------------------------------------------#

Pressure                              <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Region","Pressure.type")]

PressOrder                            <- data[, .(NrPaps = length(unique(SW.ID))), by = "Pressure.type"]
PressOrder                            <- PressOrder[order(NrPaps, decreasing = T),,]

Pressure$Pressure.type                <- factor(Pressure$Pressure.type, levels = PressOrder$Pressure.type)

RegionOrder                           <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
RegionOrder                           <- RegionOrder[order(NrPaps, decreasing = T),,]

Pressure$Region                       <- factor(Pressure$Region, levels = RegionOrder$Region)

## Option1
p <- ggplot(Pressure, aes(Pressure.type, NrPaps, fill=Region)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0,1), n.breaks = 10) +
  scale_fill_brewer(palette = "Set1") +
  labs(x="Pressure type", y="Number of papers") +
  theme_bw() +
  theme(legend.position = c(0.87,0.65),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
print(p)

ggsave("PressureRegion.tiff", p, path=outPath, width = 8, height = 6)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Ecosystem components ----
#-----------------------------------------------#

#-----------------------------------------------#
## Time series by ecosystem component -----
#-----------------------------------------------#

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Ecosystem.component_level1","Year")]
EcoCompExpand                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)),
                                                     Ecosystem.component_level1=unique(EcoComp$Ecosystem.component_level1))
EcoComp                               <- merge(EcoCompExpand, EcoComp, by=c("Ecosystem.component_level1","Year"), all.x = TRUE)

EcoCompOrder                          <- data[, .(NrPaps = length(unique(SW.ID))), by = "Ecosystem.component_level1"]
EcoCompOrder                          <- EcoCompOrder[order(NrPaps, decreasing = T),,]

EcoComp$Ecosystem.component_level1    <- factor(EcoComp$Ecosystem.component_level1, levels = EcoCompOrder$Ecosystem.component_level1)

p <- ggplot(EcoComp, aes(Year,NrPaps)) +
  geom_point() +
  geom_line() +
  scale_x_continuous() +
  # scale_colour_brewer(palette = "Paired") +
  labs(y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 14)) +
  facet_wrap(~Ecosystem.component_level1)
print(p)
ggsave("EcoCompYear.tiff", p, path=outPath, width=10, height = 6)


## Include also panel with all ecosystem components combined
EcoComp2                              <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Year")]
EcoComp2$Ecosystem.component_level1   <- "All"
EcoComp2                              <- EcoComp2[,c(3,1,2)]
EcoComp2Expand                        <- expand.grid(Year=seq(min(data$Year),max(data$Year)),
                                                     Ecosystem.component_level1=unique(EcoComp2$Ecosystem.component_level1))
EcoComp2                              <- merge(EcoComp2Expand, EcoComp2, by=c("Ecosystem.component_level1","Year"), all.x = TRUE)
EcoComp2                              <- rbind(EcoComp2,EcoComp)

p <- ggplot(EcoComp2, aes(Year,NrPaps)) +
  geom_point() +
  geom_line() +
  scale_x_continuous() +
  # scale_colour_brewer(palette = "Paired") +
  labs(y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 14)) +
  facet_wrap(~Ecosystem.component_level1)
print(p)
ggsave("EcoCompAllYear.tiff", p, path=outPath, width=10, height = 6)

#-----------------------------------------------#
## Barplot with EcoComp and Aspect -----
#-----------------------------------------------#

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Ecosystem.component_level1","Response.variable_category")]

EcoOrder                              <- data[, .(NrPaps = length(unique(SW.ID))), by = "Ecosystem.component_level1"]
EcoOrder                              <- EcoOrder[order(NrPaps, decreasing = T),,]
names(EcoOrder)[2]                    <- "NrPapsTot"
EcoComp                               <- merge(EcoComp, EcoOrder, by="Ecosystem.component_level1")

EcoComp$Ecosystem.component_level1    <- factor(EcoComp$Ecosystem.component_level1, levels = EcoOrder$Ecosystem.component_level1)

AspectOrder                           <- data[, .(NrPaps = length(unique(SW.ID))), by = "Response.variable_category"]
AspectOrder                           <- AspectOrder[order(NrPaps, decreasing = T),,]

EcoComp$Response.variable_category    <- factor(EcoComp$Response.variable_category, levels = AspectOrder$Response.variable_category)

noCol                                 <- length(AspectOrder$Response.variable_category)
mycolors                              <- sample(colorRampPalette(brewer.pal(12,"Paired"))(noCol))

p <- ggplot(EcoComp, aes(Ecosystem.component_level1, NrPaps, fill=Response.variable_category)) +
  geom_bar(stat="identity") +
  geom_text(aes(x=Ecosystem.component_level1, y=NrPapsTot, label=NrPapsTot), size=3) +
  scale_y_continuous(expand = c(0,1), n.breaks = 10) +
  scale_fill_manual(values = mycolors) +
  labs(x="Ecosystem component", y="Number of times aspect is studied across all papers") +
  theme_bw() +
  theme(legend.position = c(0.87,0.58),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
print(p)

ggsave("EcoCompAspect.tiff", p, path=outPath, width = 10, height = 6)


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Pressures ----
#-----------------------------------------------#

#-----------------------------------------------#
## Time series by pressure -----
#-----------------------------------------------#

Pressures                               <- data[, .(NrPaps = length(unique(SW.ID))), by = c("Pressure.type","Year")]
PressuresExpand                         <- expand.grid(Year=seq(min(data$Year),max(data$Year)), 
                                                     Pressure.type=unique(Pressures$Pressure.type))
Pressures                               <- merge(PressuresExpand, Pressures, by=c("Pressure.type","Year"), all.x = TRUE)

PressureOrder                           <- data[, .(NrPaps = length(unique(SW.ID))), by = "Pressure.type"]
PressureOrder                           <- PressureOrder[order(NrPaps, decreasing = T),,]

Pressures$Pressure.type                 <- factor(Pressures$Pressure.type, levels = PressureOrder$Pressure.type)

p <- ggplot(Pressures, aes(Year,NrPaps)) +
  geom_point() +
  geom_line() +
  scale_x_continuous() +
  # scale_colour_brewer(palette = "Paired") +
  labs(y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 14)) +
  facet_wrap(~Pressure.type, nrow = 2)
print(p)
ggsave("PressureYear.tiff", p, path=outPath, width=10, height = 5)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Gears ----
#-----------------------------------------------#

#-----------------------------------------------#
## Barplot time series commercial and recreational fishing -----
#-----------------------------------------------#

FishTypeYear                      <- data[,.(NrPaps = length(unique(SW.ID))),
                                         by = c("Year","Fishery.type")]

p <- ggplot(FishTypeYear, aes(Year, NrPaps, fill=Fishery.type)) +
  geom_bar(stat="identity", position = "stack") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 45), n.breaks = 10) +
  scale_fill_viridis_d(begin = 0, end = 0.9, name = "Fishery") +
  labs(x="Publication year", y="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 3, unit = "mm")),
        panel.grid.minor = element_blank()) #+
  # facet_wrap(~Fishery.type)
print(p)

ggsave("FishTypeYear.png", p, path=outPath, width = 5, height = 3.2)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Sankey diagram ----
#-----------------------------------------------#

#-----------------------------------------------#
## Fisheries-Pressure-Ecosystem component-Aspect -----
#-----------------------------------------------#

#===-
# Data cleaning
#====-
## Create long version
sankeyDat                            <- data[,c("SW.ID","Fishery.type","Pressure.type","Ecosystem.component_level1","Response.variable_category")]
sankeyDat                            <- sankeyDat[!duplicated(sankeyDat),]

sankeyDat$Fishery.type               <- factor(sankeyDat$Fishery.type, levels = c("Unknown","Scientific","Recreational","Commercial"))

sankeyDat$Response.variable_category <- with(sankeyDat, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))  

sankeyInput                          <- make_long(sankeyDat, Fishery.type,Pressure.type,Ecosystem.component_level1,Response.variable_category)


#===-
# Create sankey diagram
#====-
## Set colors
colorpal                             <- brewer.pal(8,"Paired")[-c(2,4,6,8)]
colorpal                             <- viridis(8)[-c(1,2,3,6)]
# mycolors                             <- c(rep(colorpal[1],4), rep(colorpal[3],7), rep(colorpal[5],11), rep(colorpal[7],13))

## Build sankey
sankey <- ggplot(sankeyInput,
                 mapping = aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(x),
                               label = node)) +
  scale_x_discrete(labels=c("Fishery","Pressure","Ecosystem component","Impact")) +
  scale_fill_manual(values=colorpal) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +
  geom_sankey_label(size=7) +
  theme_few()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=20, colour = "black"),
        legend.position = "none")

## View Sankey
sankey

## Save sankey
ggsave("Sankey.tiff", sankey, path=outPath,
       width = 400,
       height = 200,
       units = "mm")

## View interactive sankey (needs work)
# ggplotly(sankey)



#-----------------------------------------------#
## Fisheries-Gear-Pressure-Ecosystem component-Aspect -----
#-----------------------------------------------#

#===-
# Data cleaning
#====-
## Create long version
sankeyDat                            <- data[,c("SW.ID","Fishery.type","Pressure.type","Gear_level1","Ecosystem.component_level1","Response.variable_category")]
sankeyDat$Gear_level1                <- with(sankeyDat, ifelse(is.na(Gear_level1),"Unknown ",Gear_level1))
sankeyDat                            <- sankeyDat[!duplicated(sankeyDat),]

sankeyDat$Fishery.type               <- factor(sankeyDat$Fishery.type, levels = c("Unknown","Scientific","Recreational","Commercial"))
sankeyDat$Pressure.type              <- factor(sankeyDat$Pressure.type, levels = c("Catch_and_bycatch","Input of litter","Physical disturbance of the seabed","Electromagnetic input",
                                                                                   "Noise","Discarding","Visual disturbance"))
sankeyDat$Gear_level1                <- factor(sankeyDat$Gear_level1, levels = c("Demersal trawls","Seines","Pots","Hooks_and_lines","Nets","Dredges","Pelagic trawls","Other","Spearfishing","Unknown "))
sankeyDat$Ecosystem.component_level1 <- factor(sankeyDat$Ecosystem.component_level1, levels = c("Foodweb","Cephalopods","Benthos","Seabirds","Physical_habitats","Fish_cartilaginous",
                                                                                                "Fish_teleost","Marine_mammals","Reptiles","Plants","Plankton"))
sankeyDat$Response.variable_category <- factor(sankeyDat$Response.variable_category, levels = c("Abundance/biomass/density","Behaviour","Community composition","Mortality",
                                                                                                "Sediment & physical properties","Damage & entanglement","Growth","Trophic structure",
                                                                                                "Biodiversity","Size/age structure","Survival","Other","Reproduction",
                                                                                                "Production/productivity","Physiology"))

# sankeyDat$Response.variable_category <- with(sankeyDat, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))  

sankeyInput                          <- make_long(sankeyDat, Fishery.type,Gear_level1,Pressure.type,Ecosystem.component_level1,Response.variable_category)

sankeyInput$node                     <- factor(sankeyInput$node, levels = c("Unknown","Scientific","Recreational","Commercial",
                                                                            rev(c("Seines","Pots","Demersal trawls","Pelagic trawls","Hooks_and_lines","Nets","Dredges","Spearfishing","Other","Unknown ")),
                                                                            rev(c("Catch_and_bycatch","Physical disturbance of the seabed","Input of litter","Electromagnetic input","Noise","Discarding","Visual disturbance")),
                                                                            "Plants","Plankton","Foodweb","Benthos","Physical_habitats","Cephalopods","Fish_cartilaginous",
                                                                            "Fish_teleost","Seabirds","Marine_mammals","Reptiles",
                                                                            rev(c("Behaviour","Mortality","Survival","Damage & entanglement","Abundance/biomass/density",
                                                                                  "Sediment & physical properties","Trophic structure",
                                                                                  "Community composition","Biodiversity","Size/age structure","Reproduction","Growth",
                                                                                  "Production/productivity","Physiology"))))


#===-
# Create sankey diagram
#====-
## Set colors
colorpal                             <- viridis(10)[-c(1,2,3,4,5)]

## Build sankey
sankey <- ggplot(sankeyInput,
                 mapping = aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(x),
                               label = node)) +
  scale_x_discrete(labels=c("Fishery","Gear","Pressure","Ecosystem component","Impact")) +
  scale_fill_manual(values=colorpal) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +
  geom_sankey_label(size=8) +
  theme_few()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=24, colour = "black"),
        legend.position = "none")

## View Sankey
sankey

## Save sankey
ggsave("Sankey5.png", sankey, path=outPath,
       width = 500,
       height = 250,
       units = "mm")


#===-
# Create alternative sankey diagram using ggforce
#====-




#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Map with regions ----
#-----------------------------------------------#

# -----------------------------------------------#
## ## Creating shapefile with Regions ----
# -----------------------------------------------#
# ## Load ICES regions as downloaded from https://gis.ices.dk/sf/ and mediterranean GSA's from https://www.fao.org/gfcm/data/maps/gsas/en/
# ICESareas                            <- st_read(paste0(GISpath, "ICES_Areas_20160601_cut_dense_3857.shp"))
# ICESareas                            <- st_transform(ICESareas, crs=3035)
# ICESEcors                            <- st_read(paste0(GISpath, "ICES_ecoregions_20171207_erase_ESRI.shp"))
# ICESEcors                            <- st_transform(ICESEcors, crs=3035)
# 
# ICESEcors$Region                     <- ifelse(ICESEcors$Ecoregion %in% c("Western Mediterranean Sea", "Adriatic Sea",
#                                                                    "Ionian Sea and the Central Mediterranean Sea",
#                                                                    "Aegean-Levantine Sea"), "Mediterranean Sea",
#                                                ifelse(ICESEcors$Ecoregion == "Greater North Sea", "North Sea",
#                                                       ifelse(ICESEcors$Ecoregion %in% c("Black Sea", "Norwegian Sea","Barents Sea","Baltic Sea"), ICESEcors$Ecoregion,
#                                                              ifelse(ICESEcors$Ecoregion %in% c("Azores", "Oceanic Northeast Atlantic", "Greenland Sea",
#                                                                                                       "Icelandic Waters", "Faroes", "Celtic Seas"), "NE-Atlantic", NA))))
# 
# ICESareas$Region                     <- ifelse(ICESareas$Area_Full %in% c("27.7.a", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.8.a","27.8.b","27.8.c", "27.8.d.2"), "Western Waters",
#                                                ifelse(ICESareas$Area_Full %in% c("27.9.a", "27.9.b.1", "27.9.b.2", "27.8.e.1", "27.8.e.2", "27.6.a", "27.7.c.2",
#                                                                                  "27.6.b.2", "27.7.b", "27.7.k.2", "27.7.j.2", "27.7.b", "27.7.j.1"), "NE-Atlantic", NA))
# 
# Regions                              <- rbind(subset(ICESEcors[,c("Region", "geometry")], is.na(Region)==F), subset(ICESareas[,c("Region", "geometry")], is.na(Region)==F))
# 
# # Select regions that already consist of one area
# Regs                                 <- subset(Regions, Region %in% c("Baltic Sea","Barents Sea", "Black Sea", "North Sea", "Norwegian Sea"))
# 
# # Combine subregions into on region
# for (iReg in c("Mediterranean Sea", "Western Waters", "NE-Atlantic")){
#   subdat                             <- subset(Regions, Region == iReg)
#   a                                  <- st_sf(st_union(subdat))
#   b                                  <- data.frame(Region = iReg)
#   b                                  <- st_set_geometry(b, st_geometry(a))
#   Regs                               <- rbind(Regs, b)
# } # end iReg loop
# 
# ## Fix some overlaps that should not be there
# ## NE-Atlantic
# WW                                   <- st_difference(subset(Regs, Region == "NE-Atlantic"), subset(Regs, Region == "Western Waters"))
# WW$Region.1                          <- NULL
# 
# ## Update the fixes
# Regs2                                <- subset(Regs, !Region %in% "NE-Atlantic")
# Regs2                                <- rbind(Regs2, WW)
# 
# RegionalSeas                         <- Regs2
# st_write(RegionalSeas, paste0(GISpath, "RegionalSeas.shp"))
# save(RegionalSeas, file=paste0(GISpath, "RegionalSeas.Rdata"))


#-----------------------------------------------#
## Creating map with Region specific info ----
#-----------------------------------------------#
Regions                              <- data[, .(NrPaps = length(unique(SW.ID))), 
                                              by = Region]
Regions                              <- Regions[order(NrPaps),,]
RegCol                               <- data.frame(colcode = viridis(max(Regions$NrPaps)+2),
                                                   value = c(1:c(max(Regions$NrPaps)+2)))
Regions$Colcode                      <- RegCol$colcode [match(Regions$NrPaps, RegCol$value)]
load(paste0(GISpath, "RegionalSeas.Rdata"))
RegionalSeas$colcode                 <- Regions$Colcode [match(RegionalSeas$Region, Regions$Region)]
Centerpoints                         <- st_coordinates(st_centroid(RegionalSeas))
RegionalSeas$Xloc              <- Centerpoints[,1]
RegionalSeas$Yloc              <- Centerpoints[,2]
RegionalSeas$NrPaps            <- Regions$NrPaps [match(RegionalSeas$Region, Regions$Region)]
RegionalSeas$textcol           <- ifelse(RegionalSeas$NrPaps >50, "black", "white")

## Change some points to a better location
RegionalSeas$Xloc              <- ifelse(RegionalSeas$Region == "Baltic Sea", 4900000, RegionalSeas$Xloc)
RegionalSeas$Xloc              <- ifelse(RegionalSeas$Region == "Western Waters", 3100000, RegionalSeas$Xloc)


## Create plot
tiff(paste0(outPath, "RegMap.tiff"), width = 1000, height = 900, res = 100)
par(mar=c(1,1,1,1))
plot(st_geometry(RegionalSeas), col=RegionalSeas$colcode, border=F)
text(RegionalSeas$NrPaps, x= RegionalSeas$Xloc, y=RegionalSeas$Yloc, col=RegionalSeas$textcol, font=2, cex=1.4)
text(x= 1E6, y=6.01E6, "Global studies:", font=3, cex=1.4)
text(x=1e6, y=5.85e6, paste0(subset(Regions, Region == "Global")$NrPaps), font=2, cex=1.4)
gradient.rect(xleft=0, xright=1E6, ytop=1.5e6, ybottom=1.25E6, col=RegCol$colcode, gradient="x")
text(x=0, y=1.18e6, "1")
text(x=1E6, y=1.18e6, max(Regions$NrPaps)+2)
text(x=0.5e6, y=1.72e6, "Number of papers", cex=1.2)
dev.off()


## Smaller version plot
RegionalSeas$Xloc              <- ifelse(RegionalSeas$Region == "Baltic Sea", 4950000, RegionalSeas$Xloc) #change points to a better location
RegionalSeas$Yloc              <- ifelse(RegionalSeas$Region == "Baltic Sea", 4010000, RegionalSeas$Yloc) #change points to a better location

png(paste0(outPath, "RegMapSmall.png"), width = 700, height = 600, res = 100)
par(mar=c(1,1,1,1))
plot(st_geometry(RegionalSeas), col=RegionalSeas$colcode, border=F)
text(RegionalSeas$NrPaps, x= RegionalSeas$Xloc, y=RegionalSeas$Yloc, col=RegionalSeas$textcol, font=2, cex=1.4)
text(x= 1E6, y=6.1E6, "Global studies:", font=3, cex=1.4)
text(x=1e6, y=5.85e6, paste0(subset(Regions, Region == "Global")$NrPaps), font=2, cex=1.4)
gradient.rect(xleft=0, xright=1E6, ytop=1.5e6, ybottom=1.25E6, col=RegCol$colcode, gradient="x")
text(x=0, y=1.15e6, "1")
text(x=1E6, y=1.15e6, max(Regions$NrPaps)+2)
text(x=0.5e6, y=1.72e6, "Number of papers", cex=1.2)
dev.off()



# ## Create a smaller version
# load(paste0(GISpath, "RegionalSeas.Rdata"))
# 
# library(dplyr)
# RegionalSeas <- RegionalSeas %>%
#   left_join(Regions, by="Region")
# 
# ### Settings for plotting
# library(rnaturalearth)
# world   <- ne_countries(scale = "medium", returnclass = "sf")
# xlim    <- c(290000,6912085)
# ylim    <- c(861290.7,6962509)
# colours <- brewer.pal(9,"YlOrRd") 
# 
# ggplot(data = RegionalSeas) +
#   geom_sf(data = RegionalSeas, aes(fill=NrPaps)) +
#   # geom_sf(data = world, fill="white", col="white") +
#   coord_sf(xlim = xlim, ylim = ylim) +
#   scale_fill_viridis_c() +
#   theme_bw()



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# PART II - Case Study Litter: Plots and numbers ----
#-----------------------------------------------#
# Subset to litter only
litter <- droplevels(data[Pressure.type == "Input of litter", ])

## Extra Data Cleaning - May need to be considered at the database level.
litter[litter$SW.ID == "SW4_0725" & litter$Ecosystem.component_level1 == "Fish_teleost", "Direction.of.relationship"] <- "Negative"
litter[litter$SW.ID == "SW4_0725" & litter$Ecosystem.component_level1 == "Cephalopods", "Direction.of.relationship"] <- "Negative"
litter[litter$SW.ID == "SW4_0144", "Direction.of.relationship"] <- "Positive"
litter[litter$SW.ID == "SW4_0619" & litter$Response.variable_category == "Damage & entanglement", "Direction.of.relationship"] <- "Positive"
litter[litter$SW.ID == "SW4_0851" & litter$Response.variable_category == "Damage & entanglement", "Direction.of.relationship"] <- "Positive"
litter[litter$Response.variable_category == "Biodiversity" & litter$Direction.of.relationship == "Positive",]
litter[litter$Direction.of.relationship == "Mortality",]
litter[litter$Response.variable_category == "Biodiversity" & litter$Direction.of.relationship == "Positive",]

# Litter specific data cleaning
litter[is.na(litter$Gear_level1) & !duplicated(litter$SW.ID), ]
litter[is.na(litter$Gear_level1), "Gear_level1"] <- "Other"

litter[litter$Pressure.variable_category == "Fishing effort",]

#-----------------------------------------------#
## Basic Numbers ----
#-----------------------------------------------#
# Number of unique papers
nrow(litter[!duplicated(litter$SW.ID),])

# Number of records
nrow(litter)

# Earliest paper
litter[litter$Year == min(litter$Year), ]
litter[litter$Year == min(litter$Year), "Year"]

#-----------------------------------------------#
## Gears ----
#-----------------------------------------------#
unique(litter$Gear_level1)
table(litter$Gear_level1)
unique(litter$Gear_level2)
table(litter$Gear_level2)

#-----------------------------------------------#
## Methods ----
#-----------------------------------------------#
unique(litter$Sampling.Method.used.for.data.collection)
table(litter$Sampling.Method.used.for.data.collection)
table(litter$Analytical.method.used.for.inference)
table(litter$Study.type)

#===-
### Data cleaning ----
#====-
## Unique papers only:
# tempLit <- litter[!duplicated(litter$SW.ID), c("SW.ID","Ecosystem.component_level1", "Sampling.Method.used.for.data.collection", "Response.variable_category", "Analytical.method.used.for.inference", "Direction.of.relationship")]
# Unique combinations of relevant variables (some duplication of papers)
tempLit <- litter[, c("SW.ID","Ecosystem.component_level1", "Sampling.Method.used.for.data.collection", "Response.variable_category", "Analytical.method.used.for.inference")]
tempLit <- tempLit[!duplicated(tempLit), ]

tempLit$Response.variable_category <- ifelse(tempLit$Response.variable_category %in% "Survival","Mortality",
                                             ifelse(tempLit$Response.variable_category %in% c("Community composition"), "Biodiversity",
                                             tempLit$Response.variable_category))  
tempLit$Sampling.Method.used.for.data.collection <- ifelse(tempLit$Sampling.Method.used.for.data.collection %in% c("In situ structural growth", "Visual Analyses of Quadrats/Transects"), "Visual or Photographic\nAnalyses",
                                                           ifelse(tempLit$Sampling.Method.used.for.data.collection %in% c("Irregular Fisheries Independent Survey"), "Irregular Fisheries\nIndependent Survey",
                                                                  ifelse(tempLit$Sampling.Method.used.for.data.collection %in% c("Active Acoustic Sampling Survey"), "Active Acoustic\nSampling Survey",
                                                                         tempLit$Sampling.Method.used.for.data.collection)))

tempLit$AnalyticalMethod <- ifelse(tempLit$Analytical.method.used.for.inference %in% c("Summarizing statistics",
                                                                                       "Deacriptive statistics (Percentage abundance; Frequeny´cy of occurrence)",
                                                                                       "questionnaire (Local Ecological Knowledge), chemical digestion analysis of guts, polymer identification",
                                                                                       "occurrence _ abundance",
                                                                                       NA,
                                                                                       "Digital Elevation Models (DEMs) _ ROV video images _ SIS",
                                                                                       "Video data", "postmortem examination of species, sex, body mass, external measures and pathoanatomical dissection using protocol",
                                                                                       "MATLAB _ Rainbow Click software package "),
                                   "Descriptive\nstatistics",
                                   ifelse(tempLit$Analytical.method.used.for.inference %in% c("ANOSIM _ Mann-Whitney test _ Kruskal-Wallis",
                                                                                              "Kruskal–Wallis test _ Mann–Whitney U test _ chi-square test",
                                                                                              "CPUE _ Kruskal-Wallis _ 1-way ANOVA"),
                                          "Non-parametric\nGroupwise",
                                          ifelse(tempLit$Analytical.method.used.for.inference %in% c("Quantitative image analysis _ Pearson and Kendall correlation _ species richness and abundance _ frequency of occurrence _ mean size",
                                                                                                     "least-square regresssion analysis",
                                                                                                     "CPUE"),
                                                 "Linear Models",
                                                 ifelse(tempLit$Analytical.method.used.for.inference %in% c("PERMANOVA _ Pairwise _ Canonical Analysis of Principal coordinates _ 3D-nMDS _ Ordinary LS",
                                                                                                            "PCA, Significance tests",
                                                                                                            "PERMANOVA"),
                                                        "Multivariate &\nOrdination",
                                                        ifelse(tempLit$Analytical.method.used.for.inference %in% c("Benthic Terrain Modelling (BTM)"),
                                                               "GLMs GAMs &\nMachine Learning",
                                                               tempLit$Analytical.method.used.for.inference)))))

## Order factors for plotting nicely
tempLit$Ecosystem.component_level1 <- reorder(x = as.factor(tempLit$Ecosystem.component_level1),
                                              X = as.factor(tempLit$Ecosystem.component_level1),
                                              FUN = length)
tempLit$Sampling.Method.used.for.data.collection <- reorder(x = as.factor(tempLit$Sampling.Method.used.for.data.collection),
                                                            X = as.factor(tempLit$Sampling.Method.used.for.data.collection),
                                                            FUN = length)
tempLit$Response.variable_category <- reorder(x = as.factor(tempLit$Response.variable_category),
                                              X = as.factor(tempLit$Response.variable_category),
                                              FUN = length)
tempLit$AnalyticalMethod <- reorder(x = as.factor(tempLit$AnalyticalMethod),
                                                        X = as.factor(tempLit$Analytical.method.used.for.inference),
                                                        FUN = length)
tempLit <- droplevels(tempLit)


#=== - 
### Basic proportions for manuscript ----
#==== - 
# Proportion of studies using only descriptive statistics
nrow(tempLit[tempLit$AnalyticalMethod == "Descriptive\nstatistics", ])/nrow(tempLit)
# Proportions of studies using correlative approaches
nrow(tempLit[tempLit$AnalyticalMethod == "Non-parametric\nGroupwise", ])/nrow(tempLit)
nrow(tempLit[tempLit$AnalyticalMethod == "Linear Models", ])/nrow(tempLit)
nrow(tempLit[tempLit$AnalyticalMethod == "GLMs GAMs &\nMachine Learning", ])/nrow(tempLit)
nrow(tempLit[tempLit$AnalyticalMethod == "Multivariate &\nOrdination", ])/nrow(tempLit)

# Proportions of studies using various responses
nrow(tempLit[tempLit$Response.variable_category == "Growth", ])/nrow(tempLit)
nrow(tempLit[tempLit$Response.variable_category == "Mortality", ])/nrow(tempLit)


#===-
### Create sankey diagram ----
#====-
## Create long versions for sankeys
litterMethodsInput <- make_long(tempLit, Ecosystem.component_level1, Sampling.Method.used.for.data.collection, Response.variable_category, AnalyticalMethod)

## Set colors
# colorpal                             <- brewer.pal(8,"Paired")[-c(2,4,6,8)]
colorpal                             <- viridis(8)[-c(1,2,3,6)]
# mycolors                             <- c(rep(colorpal[1],4), rep(colorpal[3],7), rep(colorpal[5],11), rep(colorpal[7],13))

## Build sankey
litterMethods <- ggplot(litterMethodsInput,
                        mapping = aes(x = x,
                                      next_x = next_x,
                                      node = node,
                                      next_node = next_node,
                                      fill = factor(x),
                                      label = node)) +
  scale_x_discrete(labels=c("Ecosystem\nComponent","Sampling\nMethodology","Response\nMeasured","Analytic\nMethod")) +
  # scale_fill_manual(values=colorpal) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +
  geom_sankey_label(size=2) +
  theme_few()+
  theme(text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "none")

ggsave(plot = litterMethods,
       filename = paste0(outPath, "litterMethods.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")


#===-
### Create alternative sankey diagram with ggforce ----
#====-

# Prepare data
tempLitNoDupl       <- tempLit[!duplicated(tempLit), ]
tempLitNoDupl$value <- 1
tempLitNoDupl$Analytical.method.used.for.inference <- NULL

# Transform data so that it can be handled by ggplot2
tempLitTr           <- gather_set_data(tempLitNoDupl, 
                                       x = c("Ecosystem.component_level1", "Sampling.Method.used.for.data.collection", "Response.variable_category", "AnalyticalMethod"), 
                                       id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(tempLitTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Ecosystem.component_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size = 2.4) +
  scale_x_continuous(breaks = c(2:5),
                     labels = c("Ecosystem\ncomponent","Sampling\nMethodology","Response\nmeasured","Analytic\nmethod")) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(plot = p,
       filename = paste0(outPath, "litterMethods_altn.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 100,
       units = "mm")



#-----------------------------------------------#
## Geographic spread ----
#-----------------------------------------------#
tempLitGeo <- litter[!duplicated(litter$SW.ID), ]

tempLitGeo <- as.data.frame(table(tempLitGeo$Region))
colnames(tempLitGeo) <- c("Region", "Number of Articles")
# tempLitGeo$Region <- factor(tempLitGeo$Region, levels = c("Barents Sea", "NE-Atlantic", "Baltic Sea", "Mediterranean Sea"))

regSea <- data.frame(Region = as.factor(unique(data$Region)),
                     `Number of Articles` = rep(0, times = length(unique(data$Region))))

tempLitGeo <- merge(x = regSea,
                    y = tempLitGeo,
                    by = "Region",
                    all.x = TRUE)
tempLitGeo$Number.of.Articles <- NULL
tempLitGeo[is.na(tempLitGeo$`Number of Articles`), "Number of Articles"] <- 0

tempLitGeo$Region <- factor(tempLitGeo$Region,
                            levels = levels(tempLitGeo$Region)[order(tempLitGeo$`Number of Articles`,
                                                                     tempLitGeo$Region,
                                                                     decreasing = c(TRUE, FALSE))])


ggsave(filename = paste0(outPath, "litterGeo.png"),
       device = "png",
       dpi = 300,
       width = 80,
       height = 90,
       units = "mm",
       plot = ggplot()+
         geom_col(data = tempLitGeo,
                  mapping = aes(x = Region,
                                y = `Number of Articles`,
                                fill = Region)) +
         theme_few()+
         theme(text = element_text(size = 10),
               # axis.title = element_blank(),
               axis.text = element_text(colour = "black"),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
               legend.position = "none")
         )

#-----------------------------------------------#
## Spatial Scales ----
#-----------------------------------------------#

## Make spatial extent and scale categories
litter$ScaleSpatial <- factor(x = litter$Scale...Spatial..m.,
                              levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                              ordered = TRUE)
litter$ResSpatial <- factor(x = litter$Resolution...Spatial..m.,
                              levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                              ordered = TRUE)

spatResEx_cat <- expand.grid(levels(litter$ScaleSpatial),
                             levels(litter$ScaleSpatial))

colnames(spatResEx_cat) <- c("SpatialExtent_m", "SpatialRes_m")

## Make counts of articles in different combinations of SPATIAL EXTENTS & RESOLUTIONS
spatResEx_count <- aggregate(SW.ID~ScaleSpatial+ResSpatial,
                             data = litter[!duplicated(litter$SW.ID), ],
                             FUN = length)

names(spatResEx_count)[names(spatResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatResEx_count <- merge(x = spatResEx_cat,
                         y = spatResEx_count,
                         by.y = c("ScaleSpatial", "ResSpatial"),
                         by.x = c("SpatialExtent_m", "SpatialRes_m"),
                         all.x = TRUE)

spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "litter_spatialResVExt.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatResEx_count,
                   mapping = aes(x = SpatialExtent_m,
                                 y = SpatialRes_m,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0) +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Sampling Resolution (m)") +
         xlab("Sampling Extent (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
               )
)

#-----------------------------------------------#
## Temporal Scales ----
#-----------------------------------------------#

## Make temporal extent and scale categories
litter$ScaleTemporal <- factor(x = litter$Scale...Temporal,
                               levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                               ordered = TRUE)
litter$ResTemporal <- factor(x = litter$Resolution...Temporal,
                             levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                             ordered = TRUE)

tempResEx_cat <- expand.grid(levels(litter$ScaleTemporal),
                             levels(litter$ScaleTemporal))

colnames(tempResEx_cat) <- c("TemporalExtent", "TemporalRes")

## Make counts of articles in different combinations of TEMPORAL EXTENTS & RESOLUTIONS
tempResEx_count <- aggregate(SW.ID~ScaleTemporal+ResTemporal,
                             data = litter[!duplicated(litter$SW.ID), ],
                             FUN = length)

names(tempResEx_count)[names(tempResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


tempResEx_count <- merge(x = tempResEx_cat,
                         y = tempResEx_count,
                         by.y = c("ScaleTemporal", "ResTemporal"),
                         by.x = c("TemporalExtent", "TemporalRes"),
                         all.x = TRUE)

tempResEx_count[is.na(tempResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "litter_temporalResVExt.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = tempResEx_count,
                   mapping = aes(x = TemporalExtent,
                                 y = TemporalRes,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0) +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Sampling Resolution") +
         xlab("Sampling Extent") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)


#-----------------------------------------------#
## SpatioTemporal Extent ----
#-----------------------------------------------#
## Dependent on code above


# Make spatio-temperal extent and resolution category matrix in long form
spatempResEx_cat <- expand.grid(levels(litter$ScaleSpatial),
                                levels(litter$ScaleTemporal))

colnames(spatempResEx_cat) <- c("SpatialScale_m", "TemporalScale")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL EXTENTS
spatempEx_count <- aggregate(SW.ID~ScaleSpatial+ScaleTemporal,
                             data = litter[!duplicated(litter$SW.ID), ],
                             FUN = length)
names(spatempEx_count)[names(spatempEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempEx_count <- merge(x = spatempResEx_cat,
                         y = spatempEx_count,
                         by.y = c("ScaleSpatial", "ScaleTemporal"),
                         by.x = c("SpatialScale_m", "TemporalScale"),
                         all.x = TRUE)

spatempEx_count[is.na(spatempEx_count$NumberOfArticles), "NumberOfArticles"] <- 0


## Plot
ggsave(filename = paste0(outPath, "litter_spatiotemporalExt.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatempEx_count,
                   mapping = aes(x = SpatialScale_m,
                                 y = TemporalScale,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0) +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Temporal Extent") +
         xlab("Spatial Extent (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)

#-----------------------------------------------#
## SpatioTemporal Resolution ----
#-----------------------------------------------#
## Dependent on code above

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = litter[!duplicated(litter$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempResEx_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialScale_m", "TemporalScale"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "litter_spatiotemporalRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatempRes_count,
                   mapping = aes(x = SpatialScale_m,
                                 y = TemporalScale,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0) +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Temporal Resolution") +
         xlab("Spatial Resolution (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)

#-----------------------------------------------#
## Pressures & Responses ----
#-----------------------------------------------#
# Pressures exerted
unique(litter$Response.variable_paper)
unique(litter$Response.variable_category)
table(litter$Response.variable_category)
table(litter$Pressure.variable_category)

#-----------------------------------------------#
## Responses & Ecosystem Components ----
#-----------------------------------------------#
unique(litter$Ecosystem.component_level1)
table(litter$Ecosystem.component_level1)
unique(litter$Ecosystem.component_level2)
table(litter$Ecosystem.component_level2)

#=== -
### Data cleaning
#====-
tempLitDR <- litter[, c("SW.ID","Ecosystem.component_level1", "Response.variable_category", "Direction.of.relationship")]
tempLitDR <- tempLitDR[!duplicated(tempLitDR), ]

tempLitDR$Response.variable_category <- ifelse(tempLitDR$Response.variable_category %in% "Survival","Mortality",
                                               ifelse(tempLitDR$Response.variable_category %in% c("Community composition"), "Biodiversity",
                                                      tempLitDR$Response.variable_category))  

## Order factors for plotting nicely
tempLitDR$Ecosystem.component_level1 <- reorder(x = as.factor(tempLitDR$Ecosystem.component_level1),
                                                X = as.factor(tempLitDR$Ecosystem.component_level1),
                                                FUN = length)
tempLitDR$Response.variable_category <- reorder(x = as.factor(tempLitDR$Response.variable_category),
                                                X = as.factor(tempLitDR$Response.variable_category),
                                                FUN = length)
tempLitDR$Direction.of.relationship <- reorder(x = as.factor(tempLitDR$Direction.of.relationship),
                                               X = as.factor(tempLitDR$Direction.of.relationship),
                                               FUN = length)
tempLitDR <- droplevels(tempLitDR)

#===-
### Create sankey diagram ----
#====-
## Make Sankey compatible data
litterLinkageInput <- make_long(tempLitDR, Ecosystem.component_level1, Response.variable_category, Direction.of.relationship)


## Build sankey
litterLinkage <- ggplot(litterLinkageInput,
                        mapping = aes(x = x,
                                      next_x = next_x,
                                      node = node,
                                      next_node = next_node,
                                      fill = factor(x),
                                      label = node)) +
  scale_x_discrete(labels=c("Ecosystem\nComponent","Response\nMeasured","Direction of\nRelationship")) +
  # scale_fill_manual(values=colorpal) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +
  geom_sankey_label(size=2) +
  theme_few()+
  theme(text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "none")

ggsave(plot = litterLinkage,
       filename = paste0(outPath, "litterLinkage.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# PART III - Case Study: PET species bycatch ----
#-----------------------------------------------#

# Set path
outPathPET                           <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/bycatch case study/"


#-----------------------------------------------#
## Load and process PET species list ----
#-----------------------------------------------#

### Load list of PET species - same used for screening and data extraction
petList                              <- read.xlsx("Systematic Reviews/Analysis Task 4.1/PET species/PET_list_adjusted.xlsx", sheet = 1)

### Check taxonomy against WoRMS

# Get taxonomy from WoRMS
speciesList                         <- data.frame(Taxon = sort(unique(petList$Scientific.name.adjusted)))
speciesList$group                   <- c(rep(1:10, each=32) , rep(10, nrow(speciesList)-320))
speciesList <- speciesList[!speciesList$Taxon %in% c("Hydrobates castro","Hydrobates monteiroi"),] #exclude for now as they are not in WoRMS

fileName                            <- "taxonomy_PET.csv"

if(!file.exists(paste0("Systematic Reviews/Analysis Task 4.1/PET species/",fileName))){
  tax                                 <- wormsbymatchnames(subset(speciesList, group==1)$Taxon, marine_only = FALSE)
  for(iGr in 2:max(speciesList$group)){
    print(iGr)
    tax2                           <- wormsbymatchnames(subset(speciesList, group==iGr)$Taxon, marine_only = FALSE)
    tax                            <- rbind(tax, tax2)
  }
  write.csv(tax, paste0("Systematic Reviews/Analysis Task 4.1/PET species/",fileName))
} else {
  tax                                 <- read.csv(paste0("Systematic Reviews/Analysis Task 4.1/PET species/",fileName))
}

# Add valid names to the PET list
idx                                 <- match(petList$Scientific.name.adjusted, tax$scientificname)
petList$validName                   <- tax$valid_name[idx]
petList$validName                   <- with(petList, ifelse(is.na(validName),Scientific.name.adjusted,validName))


### Check regions in PET list

sort(unique(petList$`Region/RFMO`))

# OSPAR Region I = Arctic waters -> Norwegian Sea, Barents Sea or NE-Atlantic
# OSPAR Region II = Greater North Sea -> North Sea
# OSPAR Region III = Celtic Seas -> Western Waters
# OSPAR Region IV = Bay of Biscay and Iberian Coast -> Western Waters
# OSPAR Region V = Wider Atlantic -> NE-Atlantic

petRegions                          <- data.frame(RegionPetList = sort(unique(petList$`Region/RFMO`)), Region = NA)
petRegionList                       <- sort(unique(petList$`Region/RFMO`))
petRegionList

tmp                                 <- grep("Baltic", petRegionList, value = TRUE)
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% tmp,"Baltic Sea",Region))
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% c("Black Sea","Black sea"),"Black Sea",Region))
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% "Mediterranean Sea","Mediterranean Sea",Region))
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% "OSPAR  II","North Sea",Region))
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% "OSPAR IV","Western Waters",Region))
petRegions$Region                   <- with(petRegions, ifelse(RegionPetList %in% "OSPAR V","NE-Atlantic",Region))
petRegions                          <- rbind(petRegions,
                                             data.frame(RegionPetList = "Baltic Sea and Black",
                                                        Region        = "Black Sea"),
                                             data.frame(RegionPetList = "GSA 1.1, 1.2, 1.3 and Black Sea GSA 29",
                                                        Region        = c("Mediterranean Sea","Black Sea")),
                                             data.frame(RegionPetList = "Mediterranean Sea and Black Sea; Baltic Sea; OSPAR II, IV",
                                                        Region        = c("Mediterranean Sea","Black Sea","North Sea","Western Waters")),
                                             data.frame(RegionPetList = "Mediterranean Sea; OSPAR II, III",
                                                        Region        = c("Mediterranean Sea","North Sea","Western Waters")),
                                             data.frame(RegionPetList = "All OSPAR",
                                                        Region        = c("North Sea","Western Waters","NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR I",
                                                        Region        = c("NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR  I",
                                                        Region        = c("NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR I, II,",
                                                        Region        = c("NE-Atlantic","Norwegian Sea","Barents Sea","North Sea")),
                                             data.frame(RegionPetList = "OSPAR I, II, III, IV",
                                                        Region        = c("North Sea","Western Waters","NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR I, II, III, IV, Baltic sea",
                                                        Region        = c("North Sea","Western Waters","NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR I, II, III, IV, Baltic Sea",
                                                        Region        = c("North Sea","Western Waters","NE-Atlantic","Norwegian Sea","Barents Sea")),
                                             data.frame(RegionPetList = "OSPAR II, III, IV",
                                                        Region        = c("North Sea","Western Waters")),
                                             data.frame(RegionPetList = "OSPAR II, III, IV, V",
                                                        Region        = c("North Sea","Western Waters","NE-Atlantic")),
                                             data.frame(RegionPetList = "All areas",
                                                        Region        = sort(unique(data_allScreened$Region))),
                                             data.frame(RegionPetList = "All oceans",
                                                        Region        = sort(unique(data_allScreened$Region))),
                                             data.frame(RegionPetList = "All oceans and seas",
                                                        Region        = sort(unique(data_allScreened$Region))),
                                             data.frame(RegionPetList = "All regions",
                                                        Region        = sort(unique(data_allScreened$Region))),
                                             data.frame(RegionPetList = "All oceans + Mediterranean and Black Sea",
                                                        Region        = sort(unique(data_allScreened$Region))))
petRegions                          <- petRegions[complete.cases(petRegions),]
petRegions                          <- petRegions[!duplicated(petRegions),]

# Merge with actual PET species list
colnames(petRegions)[1]             <- "Region/RFMO"
petList                             <- merge(petList, petRegions, by = "Region/RFMO")



#-----------------------------------------------#
## Subset to bycatch PET papers ----
#-----------------------------------------------#

# First subset to bycatch papers
subset_byc                          <- data_allScreened[(data_allScreened$Pressure.type%in%c("Catch_and_bycatch")& data_allScreened$Pressure_level%in%c("Bycatch", "Non-target")),]
length(unique((subset_byc$SW.ID))) #172

# Kick out all ecosystem components that are not PET species in the first place
sort(unique(subset_byc$Ecosystem.component_level1))
subset_byc                          <- subset(subset_byc, !Ecosystem.component_level1 %in% c("Benthos","Cephalopods","Plankton","Plants")) #but keep food web as these may still include PET species
length(unique((subset_byc$SW.ID))) #140

# Create unique combinations of SW ID and and taxonomic group
subset_byc$ID.tax                   <- paste(subset_byc$SW.ID, subset_byc$Species.taxonomic.group.s.)
sort(unique(subset_byc$ID.tax))
length(unique(subset_byc$ID.tax))

# Split rows in case of multiple species/taxa
subset_byc_split                    <- cSplit(subset_byc, "Species.taxonomic.group.s.", " _ ", "long")
length(unique((subset_byc_split$SW.ID))) #114
length(unique((subset_byc_split$ID.tax))) #153
subset_byc_not_split                <- subset_byc[!subset_byc$ID.tax %in% subset_byc_split$ID.tax,]
length(unique((subset_byc_not_split$SW.ID))) #35
length(unique((subset_byc_not_split$ID.tax))) #35
subset_byc                          <- rbind(subset_byc_split, subset_byc_not_split)
subset_byc                          <- subset_byc[order(subset_byc$SW.ID),]
subset_byc$Species.taxonomic.group.s.[subset_byc$Species.taxonomic.group.s. %in% ""] <- NA
length(unique(subset_byc$SW.ID)) #140
length(unique(subset_byc$ID.tax)) #188

# Create combination of species and region column
subset_byc$SpeciesRegion            <- paste(subset_byc$Species.taxonomic.group.s., subset_byc$Region)
petList$SpeciesRegion               <- paste(petList$Scientific.name.adjusted, petList$Region)

subset_byc$SpeciesRegion[grepl("NA",subset_byc$SpeciesRegion)] <- NA


# Keep rows with PET species based on whether they are on the list and the region

### Marine mammals ----

# All cetaceans of all regions are included on the PET list, so these can be kept
subset_PET                          <- subset(subset_byc, Ecosystem.component_level2 %in% "Cetaceans")

# Check out seal species in the database - are they on the PET list and by region?
seals                               <- subset(subset_byc, Ecosystem.component_level2 %in% "Seals")
specReg                             <- sort(unique(seals$SpeciesRegion))
specReg
specReg %in% petList$SpeciesRegion # all are on it, so add all seals

subset_PET                          <- rbind(subset_PET, seals)

# Marine mammals not further specified?
subset_byc[subset_byc$Ecosystem.component_level1 %in% "Marine_mammals" & is.na(subset_byc$Ecosystem.component_level2),] #yes, one paper, as it is like dealing with PET species (also marked as T4.2).

# Check rows without species/taxon
mammals                             <- subset(subset_byc, Ecosystem.component_level1 %in% "Marine_mammals")
nrow(mammals[is.na(mammals$Species.taxonomic.group.s.),]) #not many
length(unique(mammals$SW.ID[is.na(mammals$Species.taxonomic.group.s.)])) #from 3 papers
## let's keep them in, as they likely contain PET species

# So basically all marine mammals in the database are PET
subset_PET                         <- subset(subset_byc, Ecosystem.component_level1 %in% "Marine_mammals")


### Seabirds ----
seabirds                           <- subset(subset_byc, Ecosystem.component_level1 %in% "Seabirds")
specReg                             <- sort(unique(seabirds$SpeciesRegion))
specReg
specReg %in% petList$SpeciesRegion # not all are on it but this due to mismatch of taxonomic rank.
specReg[!specReg %in% petList$SpeciesRegion]
# Alcidae: species from this family are on PET list
# Anatidae: species from this family are on PET list
# Calonectris diomedea diomedea: is on PET list without the subspecies name
# Fulica atra: Eurasian/common coot, NOT ON PET LIST
# Mergus: species from this genus are on PET list
# Podiceps: species from this genus are on PET list

# Check rows without species/taxon
nrow(seabirds[is.na(seabirds$Species.taxonomic.group.s.),]) #not many
length(unique(seabirds$SW.ID[is.na(seabirds$Species.taxonomic.group.s.)])) #from 3 papers
## let's keep them in, as they likely contain PET species

# So all seabirds can be kept, except Fulica atra
subset_PET                          <- rbind(subset_PET, seabirds[!seabirds$Species.taxonomic.group.s. %in% "Fulica atra",])


### Reptiles ----
reptiles                            <- subset(subset_byc, Ecosystem.component_level1 %in% "Reptiles")
specReg                             <- sort(unique(reptiles$SpeciesRegion))
specReg
specReg %in% petList$SpeciesRegion # not all are on it but this due to mismatch of taxonomic rank
specReg[!specReg %in% petList$SpeciesRegion]
# "Chelonioidea Mediterranean Sea" - superfamily that has members that are PET, so include

# Check rows without species/taxon
nrow(reptiles[is.na(reptiles$Species.taxonomic.group.s.),]) #not many
length(unique(reptiles$SW.ID[is.na(reptiles$Species.taxonomic.group.s.)])) #from 3 papers
## let's keep them in, as they likely contain PET species

# So include all rows on reptiles in the PET dataset
subset_PET                          <- rbind(subset_PET, reptiles)


### Cartilaginous fish ----
cart                                <- subset(subset_byc, Ecosystem.component_level1 %in% "Fish_cartilaginous")
specReg                             <- sort(unique(cart$SpeciesRegion))
specReg
specReg %in% petList$SpeciesRegion # not all are on PET list
specRegNotListed                    <- specReg[!specReg %in% petList$SpeciesRegion] #check out those that are not
specRegNotListed
## Not on list
# [1] "Aetomylaeus bovinus Mediterranean Sea" - not on  list
# [2] "Dasyatis pastinaca Mediterranean Sea" - only in Black Sea   
# [3] "Dipturus oxyrinchus Mediterranean Sea" - not on list  
# [6] "Leucoraja naevus Western Waters" - not on list
# [8] "Myliobatis aquila Mediterranean Sea" - not on list
# [9] "Oxynotus centrina Mediterranean Sea" - not on list (but other species of same genus is)
# [10] "Raja asterias Mediterranean Sea" - not on list   
# [12] "Raja radula Mediterranean Sea" - not on list
# [15] "Scyliorhinus canicula Mediterranean Sea" - only in Baltic Sea
# [16] "Scyliorhinus canicula Western Waters" - only in Baltic Sea     
# [17] "Scyliorhinus stellaris Mediterranean Sea" - not on list
# [18] "Scyliorhinus stellaris Western Waters" - not on list
# [19] "Squalus blainville Mediterranean Sea" 
# [20] "Squalus Mediterranean Sea" - not on list
# [21] "Tetronarce nobiliana Mediterranean Sea" - not on list
# [22] "Torpedo marmorata Mediterranean Sea" - only in Baltic Sea    
# [23] "Torpedo torpedo Mediterranean Sea" - not on list    

## On list so should be included
# [4] "Elasmobranchii North Sea" - no specific species from this area on the list, but likely entails species that are listed under 'All oceans'                 
# [5] "Elasmobranchii Western Waters" - no specific species from this area on the list, but likely entails species that are listed under 'All oceans' 
# [7] "Mustelus Mediterranean Sea" - only genus, but species are on the list
# [11] "Raja North Sea" - only genus, but species are on the list
# [13] "Raja Western Waters" - only genus, but species are on the list
# [14] "Rajidae North Sea" - only family, but species are on the list
# [20] "Squalus Mediterranean Sea" - only genus, but species are on the list

# Double-check species list with IUCN
## Listed as at least near threatened?
# [1] "Aetomylaeus bovinus Mediterranean Sea" - CE
# [9] "Oxynotus centrina Mediterranean Sea" - EN
# [2] "Dasyatis pastinaca Mediterranean Sea" - VU
# [3] "Dipturus oxyrinchus Mediterranean Sea" - NT
# [8] "Myliobatis aquila Mediterranean Sea" - VU in Europe
# [10] "Raja asterias Mediterranean Sea" - NT  
# [12] "Raja radula Mediterranean Sea" - EN
# [17] "Scyliorhinus stellaris Mediterranean Sea" - NT in Med
# [18] "Scyliorhinus stellaris Western Waters" - NT in Europe
# [19] "Squalus blainville Mediterranean Sea" - data deficient, but assume status is not good

## Least concern, so they can be excluded
# [6] "Leucoraja naevus Western Waters" - LC (but endangered in Med)
# [15] "Scyliorhinus canicula Mediterranean Sea" - LC in Europe
# [16] "Scyliorhinus canicula Western Waters" - LC in Europe
# [21] "Tetronarce nobiliana Mediterranean Sea" - LC
# [22] "Torpedo marmorata Mediterranean Sea" - LC in Med   
# [23] "Torpedo torpedo Mediterranean Sea" - LC in Europe                       

specRegNotListedDrop                 <- specRegNotListed[c(6,15,16,21,22,23)]
idx                                  <- match(specRegNotListedDrop, specReg)
specRegKeep                          <- specReg[-idx]
cart_keep                            <- subset(cart, SpeciesRegion %in% specRegKeep)

# Check rows without species/taxon
nrow(cart[is.na(cart$Species.taxonomic.group.s.),]) #18
length(unique(cart$SW.ID[is.na(cart$Species.taxonomic.group.s.)])) #from 9 papers
## let's keep them in, as they likely contain PET species
cart_keep                            <- rbind(cart_keep, cart[is.na(cart$Species.taxonomic.group.s.),])

# Add to dataframe
subset_PET                           <- rbind(subset_PET, cart_keep)


### Bony fish ----
fish                                <- subset(subset_byc, Ecosystem.component_level1 %in% "Fish_teleost")
specReg                             <- sort(unique(fish$SpeciesRegion))
specReg
specReg %in% petList$SpeciesRegion # not all are on PET list
specRegListed                       <- specReg[specReg %in% petList$SpeciesRegion] #check those that are listed
specRegListed
specRegNotListed                    <- specReg[!specReg %in% petList$SpeciesRegion] #check out those that are not
specRegNotListed
# Check those that have a higher taxonomic rank with members that are on the PET list - include those
# [9] "Epinephelinae Mediterranean Sea"
# [10] "Epinephelus marginatus Mediterranean Sea"
# [41] "Sebastes Barents Sea" 
# Add these to the listed species as well
specRegListed                       <- c(specRegListed,
                                         "Epinephelinae Mediterranean Sea",
                                         "Epinephelus marginatus Mediterranean Sea",
                                         "Sebastes Barents Sea")

# Check rows without species/taxon
nrow(fish[is.na(fish$Species.taxonomic.group.s.),]) #quite some - 40
length(unique(fish$SW.ID[is.na(fish$Species.taxonomic.group.s.)])) #from 21 papers
unique(fish$SW.ID[is.na(fish$Species.taxonomic.group.s.)])
#"SW4_0109" - includes Hippocampus hippocampus as non-target which is not on PET list but considered as NT by IUCN, so keep
#"SW4_0351" - Diplodus on PET list, so keep
#"SW4_0402" - Diplodus on PET list, so keep
#"SW4_0476" - looks at selectivity of target of fish and bycatch of invertebrates which are not on PET list, so drop
#"SW4_0534" - several non-target species on PET list, e.g. Diplodus annularis, Acipenser stellatus, so keep
#"SW4_0562" - looks at discard rates of cod (and elasmobranchs), so keep
#"SW4_0633" - several species on PET list, so include this paper
#"SW4_0904" - no PET species studies, so drop
#"SW4_0962" - Antimora rostrata on PET list
#"SW4_0973" - several species on PET list, e.g. Pagellus bogaraveo, Helicolenus dactylopterus, so keep
#"SW4_1065" - no species names are explicitly given, neither are the results split by target or non-target, so drop
#"SW4_1100" - several species on PET list, e.g. Pagellus acarne, Pagellus bogaraveo, so keep
#"SW4_1213" - cod on PET list but in study taken as target species, so drop
#"SW4_1223" - several species on PET list, e.g. Pagellus acarne, Pagrus pagrus, Epinephelus, so keep
#"SW4_1229" - several species on PET list, e.g. Coryphaenoides rupestris, Antimora rostrata, so keep
#"SW4_1332" - several species on PET list, e.g. Diplodus, Epinephelus, Pagellus, so keep
#"SW4_1536" - not reported which species are included and no specific analysis on non-target species, so drop
#"SW4_1597" - several species on PET list, e.g. Centrolophus niger, Chimaera monstrosa, so keep
#"SW4_1621" - several species on PET list, e.g. Gadus morhua, Sebastes viviparus, so keep
#"SW4_1793" - several species on PET list, e.g. Cyclopterus lumpus, Anguilla anguilla, so keep
#"SW4_1845" - no species names reported and neither a specific analysis on non-target or bycatch species, so drop

# Select papers and species to keep
fish_keep                            <- subset(fish, SpeciesRegion %in% specRegListed)
fish_keep                            <- rbind(fish_keep, fish[fish$SW.ID %in% c("SW4_0109","SW4_0351","SW4_0402","SW4_0534",
                                                                                "SW4_0562","SW4_0633","SW4_0962","SW4_0973",
                                                                                "SW4_1100","SW4_1213","SW4_1223","SW4_1229",
                                                                                "SW4_1332","SW4_1597","SW4_1621","SW4_1793"),])

# Add to PET dataset
subset_PET                           <- rbind(subset_PET, fish_keep)



### Have a closer look at Pressure Catch ----

# Select those papers that have 'Catch' as Pressure variable category - are these really on bycatch?
catch                 <- subset(subset_PET, Pressure.variable_category %in% "Catch")

# Number of papers
length(unique(catch$SW.ID)) #1
unique(catch$SW.ID)

# SW4_0081
## Not on Bycatch, as it studies how catches of small-pelagics affect food availability of dolphins.
## Exclude from case study.
subset_PET            <- subset(subset_PET, !(SW.ID %in% "SW4_0081" & Pressure.variable_category %in% "Catch" & 
                                                Ecosystem.component_level1 %in% "Marine_mammals"))

# SW4_0633
## Studies trait composition of catches, including target and non-target species.
## Non-target species are specifically addressed as bycatch, so the pressure category for non-target
## species should be changed to Bycatch. This was done in Step 4 data processing script.

# Sw4_1033
## The entered fish and elasmobranch species are classified as non-target and are also on the PET species list.
## Pressure variable category should therefore be Bycatch instead of Catch.
## This was done in Step 4 data processing script.

# SW4_1094
## Paper in distributional shifts of plaice and sole in the North Sea. The data come mostly from otter 
## trawler landings where sole and plaice are bycatch. This is reported in the database, BUT: the actual
## fishing effects are studied by taking fishing mortality from stock assesssments and modelling that
## as a variable of fishing pressure against distribution. So this records should be changed to report
## this fishing impact instead. Was done in the data extraction file directly.
## After that, this paper will not be part of the bycatch case study.

# SW4_1229
## Several species on PET species list, but some also target. Rows for target and non-target have the same
## impact, so change pressure variable category for non-target from Catch to Bycatch.
## This was done in Step 4 data processing script.


### Double-check whether papers are on bycatch and drop in case not ----

# Check out reported pressures, as they give an indication
sort(unique(subset_PET$Pressure_variable)) #select those which we're not sure about whether they're really about bycatch

# SW4_0081 - "% consumption of Small Pelagic Fish production"
# Bycatch of dolphins was included in the model, though main focus was on food web interactions with pelagic
# fisheries. Model showed neglible impact. Include study.

# SW4_0633 - "Biological trait removal by fishing"   
# Include paper, as it is about fishing impacts on fish communities, including PET fish species.

# SW4_1069 - "direct and indirect impact of fishing treating them as predators"
# Paper is about how catching of small pelagics indirectly influences dolphins through food web interactions.
# Bycatch is included in the Ecopath model, yet bycatch of dolphins very low in the study area, as mentioned and
# discussed in the Discussion of the paper.
# Therefore, keep only the row where pressure level is 'Bycatch' and drop the rows where it is 'Non-target'.
subset_PET                 <- subset(subset_PET, !(SW.ID %in% "SW4_1069" & Pressure_level %in% "Non-target"))



### Final PET dataset ----

# Check whether papers look more at mortality or at survival
subset_PET[, .(NrPaps = length(unique(SW.ID))), by = c("Response.variable_category")]
## Mostly at mortality

# Revert survival to mortality
table(subset_PET$Response.variable_category, subset_PET$Direction.of.relationship)
subset_PET$Direction.of.relationship    <- with(subset_PET, ifelse(Response.variable_category %in% "Survival" & Direction.of.relationship %in% "Negative","Positive",
                                                                   ifelse(Response.variable_category %in% "Surival" & Direction.of.relationship %in% "Positive","Negative",Direction.of.relationship)))
table(subset_PET$Response.variable_category, subset_PET$Direction.of.relationship)
subset_PET$Response.variable_category   <- with(subset_PET, ifelse(Response.variable_category %in% "Survival","Mortality", Response.variable_category))
table(subset_PET$Response.variable_category, subset_PET$Direction.of.relationship)

# Check final number of papers
length(unique(subset_PET$SW.ID)) #122 papers
table(subset_PET$Ecosystem.component_level1)


### Check number of citations per paper ----

# Load file where the number of citations was extracted from the search results from WoS and Scopus and match
# them with the bycatch PET papers
load("C:/Users/estb/OneDrive - Danmarks Tekniske Universitet/Projects/SEAwise/T4.1/Search results/search results_citedBy.RData")

# Subset to case study papers
paperIDs                <- sort(unique(subset_PET$SW.ID))
citations               <- subset(search_results, SW_ID %in% paperIDs)

write.xlsx(citations, file = paste0(outPathPET, "no. of citations bycatch PET case study.xlsx"))

# Add number of citations to main dataset
idx                     <- match(subset_PET$SW.ID, citations$SW_ID)
subset_PET$Cited.by     <- citations$Cit[idx]


### Save ----
saveRDS(subset_PET, file = paste0(outPathPET,"bycatch case study dataset.RDS"))
write.xlsx(subset_PET, file = paste0(outPathPET,"bycatch case study dataset.xlsx"))



#-----------------------------------------------#
## Basic Numbers ----
#-----------------------------------------------#
# Number of unique papers
nrow(subset_PET[!duplicated(subset_PET$SW.ID),])

# Number of records
nrow(subset_PET)

# Earliest paper
subset_PET[subset_PET$Year == min(subset_PET$Year), ]
subset_PET[subset_PET$Year == min(subset_PET$Year), "Year"] #1992

# Number of papers per year
Years                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Year")]
## Compare with trend in ALL papers - quick plot
YearsAll                             <- data[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Year")]
ggplot(YearsAll, aes(Year, NrPaps)) +
  geom_line() +
  geom_line(data = Years, aes(Year, NrPaps), col="red") +
  theme_bw()


#-----------------------------------------------#
### Barplot number of papers per year bycatch ETP ----
#-----------------------------------------------#

Years                                <- subset_PET[,.(NrPapsByc = length(unique(SW.ID))),
                                                   by = c("Year")]
# Years$Category                       <- "Bycatch ETP papers"
YearsAll                             <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year")]
# YearsAll$Category                    <- "All papers"
YearsComb                            <- merge(YearsAll, Years, by = "Year", all.x = TRUE)
YearsComb[is.na(YearsComb)]          <- 0
YearsComb$NrPapsAllNoByc             <- YearsComb$NrPaps - YearsComb$NrPapsByc
YearsPlot                            <- data.frame(Year = YearsComb$Year,
                                                   Category = c(rep("All papers",nrow(YearsComb)), rep("Bycatch ETP papers",nrow(YearsComb))),
                                                   NrPaps = c(YearsComb$NrPapsAllNoByc, YearsComb$NrPapsByc))

p <- ggplot(YearsPlot, aes(Year, NrPaps, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 50), n.breaks = 10) +
  scale_fill_manual(values = c("darkgrey", viridis(1))) +
  labs(x="Publication year", y="Number of papers") +
  theme_bw() +
  theme(legend.position = c(0.35,0.86),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 3, unit = "mm")),
        panel.grid.minor = element_blank())
print(p)

ggsave("YearBarBycatch.png", p, path=outPathPET, width = 3.2, height = 3.2)


#-----------------------------------------------#
### Gears ----
#-----------------------------------------------#
unique(subset_PET$Gear_level1)
table(subset_PET$Gear_level1, useNA = "always")
unique(subset_PET$Gear_level2)
table(subset_PET$Gear_level2, useNA = "always")

Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Gear_level1")]
Gears

# Check gears by fishery type
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Fishery.type", "Gear_level1")]
Gears

# Check fisheries type only
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Fishery.type")]
Gears

# Check gears by region
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Gear_level1","Region")]
Gears

# Check gears by ecosystem component
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Gear_level1","Ecosystem.component_level1")]
Gears

#-----------------------------------------------#
### Methods ----
#-----------------------------------------------#
unique(subset_PET$Sampling.Method.used.for.data.collection)
table(subset_PET$Sampling.Method.used.for.data.collection)
table(subset_PET$Analytical.method.used.for.inference)
table(subset_PET$Study.type)

Methods                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Sampling.Method.used.for.data.collection")]
Methods
sum(Methods$NrPaps) #137, so more than the 123 unique papers, meaning that several studies used multiple sampling methods

# Check exactly how many papers have more than one study type
MethNrPap                         <- aggregate(Sampling.Method.used.for.data.collection ~ SW.ID, subset_PET, function(x) length(unique(x)))
nrow(MethNrPap[MethNrPap$Sampling.Method.used.for.data.collection > 1,]) #13 papers with more than one method


#-----------------------------------------------#
### Ecosystem components ----
#-----------------------------------------------#

EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                  by = c("Ecosystem.component_level1")]
EcoComp

# Also by region
EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Region","Ecosystem.component_level1")]
EcoComp


#-----------------------------------------------#
### Region ----
#-----------------------------------------------#

Region                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Region")]
Region


#-----------------------------------------------#
### Study type ----
#-----------------------------------------------#

Study                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                  by = c("Study.type")]
Study
sum(Study$NrPaps) #128, so a bit more than the 123 unique papers, meaning that a few studies have multiple study types

# Check exactly how many papers have more than one study type
StudyNrPap                         <- aggregate(Study.type ~ SW.ID, subset_PET, function(x) length(unique(x)))
StudyNrPap[order(StudyNrPap$Study.type),] #five papers with two study types


#-----------------------------------------------#
### Pressure ----
#-----------------------------------------------#

Pressure                           <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Pressure.variable_category")]
Pressure
sum(Pressure$NrPaps) #132, so a few more than the 123 unique papers, meaning that a few studies have multiple study types

# Check exactly how many papers 
PressNrPap                         <- aggregate(Pressure.variable_category ~ SW.ID, subset_PET, function(x) length(unique(x)))
PressNrPap[order(PressNrPap$Pressure.variable_category),] 
nrow(PressNrPap[PressNrPap$Pressure.variable_category > 1,]) #8 papers with more than 1 pressure variable


#-----------------------------------------------#
### Response ----
#-----------------------------------------------#

Response                           <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Response.variable_category")]
Response
sum(Response$NrPaps) #144, so more than the 123 unique papers, meaning that a few studies have multiple response variables

# Check exactly how many papers
RespNrPap                         <- aggregate(Response.variable_category ~ SW.ID, subset_PET, function(x) length(unique(x)))
RespNrPap[order(RespNrPap$Response.variable_category),] 
nrow(RespNrPap[RespNrPap$Response.variable_category > 1,]) #17 papers with more than 1 response variable


#-----------------------------------------------#
### Direction of relationship ----
#-----------------------------------------------#

Direction                            <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Direction.of.relationship")]
Direction
sum(Direction$NrPaps) #150, so more than the 123 unique papers, meaning that a few studies have multiple study types

# Check exactly how many papers
DirNrPap                             <- aggregate(Direction.of.relationship ~ SW.ID, subset_PET, function(x) length(unique(x)))
DirNrPap[order(DirNrPap$Direction.of.relationship),] 
nrow(DirNrPap[DirNrPap$Direction.of.relationship > 1,]) #23 papers with more than 1 response variable

# Check direction by pressure variable
DirPress                             <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Pressure.variable_category","Direction.of.relationship")]
DirPress                             <- DirPress[order(DirPress$NrPaps, decreasing = TRUE),]
DirPress                             <- DirPress[order(DirPress$Pressure.variable_category),]
DirPress



#-----------------------------------------------#
## Ecosystem components ----
#-----------------------------------------------#

#-----------------------------------------------#
### By Pressure level/catch category ----
#-----------------------------------------------#

EcoPress                             <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Pressure_level")]
EcoTot                               <- EcoPress[, .(TotNrPaps = sum(NrPaps)),
                                                 by = "Ecosystem.component_level1"]
EcoPress                             <- merge(EcoPress, EcoTot, by="Ecosystem.component_level1")


p <- ggplot(EcoPress, aes(NrPaps, reorder(Ecosystem.component_level1, TotNrPaps), fill=Pressure_level)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 10, limits = c(0,max(EcoPress$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(3), name= "Catch category") +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("EcoCompPressLevel.png", p, path=outPathPET, width = 8, height = 5)


#-----------------------------------------------#
### By region ----
#-----------------------------------------------#

EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Region")]
EcoTot                               <- EcoComp[, .(TotNrPaps = sum(NrPaps)),
                                                 by = "Ecosystem.component_level1"]
EcoComp                              <- merge(EcoComp, EcoTot, by="Ecosystem.component_level1")
EcoComp$Region[EcoComp$Region %in% "NE-Atlantic"] <- "Northeast Atlantic"

p <- ggplot(EcoComp, aes(NrPaps, reorder(Ecosystem.component_level1, TotNrPaps), fill=Region)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 10, limits = c(0,max(EcoComp$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(8), name= "Region") +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("EcoCompRegion1.png", p, path=outPathPET, width = 8, height = 3)


# Plot by region individually
EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Region")]
EcoTot                               <- EcoComp[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Ecosystem.component_level1"]
EcoComp                              <- merge(EcoComp, EcoTot, by="Ecosystem.component_level1")
EcoComp$Region[EcoComp$Region %in% "NE-Atlantic"] <- "Northeast Atlantic"

p <- ggplot(EcoComp, aes(NrPaps, reorder(Ecosystem.component_level1, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,25)) +
  scale_fill_manual(values=viridis(6), name= "Region") +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~Region, nrow = 2)
print(p)

ggsave("EcoCompRegion2.png", p, path=outPathPET, width = 10, height = 5)


#-----------------------------------------------#
## Gears ----
#-----------------------------------------------#

#-----------------------------------------------#
### Fishery type and gear ----
#-----------------------------------------------#

Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Fishery.type", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
GearTot                              <- Gears[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Gear_level1"]
Gears                                <- merge(Gears, GearTot, by="Gear_level1")

p <- ggplot(Gears, aes(NrPaps, reorder(Gear_level1, TotNrPaps), fill=Fishery.type)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Gears$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(4), name= "Fishery type") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("FisheryTypeGears.png", p, path=outPathPET, width = 6, height = 4)


#-----------------------------------------------#
### COMBINED PLOT: Ecosystem component by region and Fishery type and gear ----
#-----------------------------------------------#

# First plot
EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Region")]
EcoTot                               <- EcoComp[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Ecosystem.component_level1"]
EcoComp                              <- merge(EcoComp, EcoTot, by="Ecosystem.component_level1")
EcoComp$Region[EcoComp$Region %in% "NE-Atlantic"] <- "Northeast Atlantic"

p1 <- ggplot(EcoComp, aes(NrPaps, reorder(Ecosystem.component_level1, TotNrPaps), fill=Region)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 10, limits = c(0,max(EcoComp$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(8), name= "Region") +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank(),
        # legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.86,0.30),
        legend.key.size = unit(0.75,"line"))
print(p1)

# Second plot
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Fishery.type", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
GearTot                              <- Gears[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Gear_level1"]
Gears                                <- merge(Gears, GearTot, by="Gear_level1")

p2 <- ggplot(Gears, aes(NrPaps, reorder(Gear_level1, TotNrPaps), fill=Fishery.type)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Gears$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(4), name= "Fishery type") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank(),
        # legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.89,0.19),
        legend.key.size = unit(0.75,"line"))
print(p2)

# Combine plots and save
p <- p1 / p2 + plot_annotation(tag_levels = 'A')

print(p)

ggsave("EcoCompRegion_FisheryTypeGears.png", p, path=outPathPET, width = 8, height = 7)



#-----------------------------------------------#
### Gear and gear level 2 ----
#-----------------------------------------------#

Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Gear_level1","Gear_level2")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
Gears$Gear_level2                    <- ifelse(is.na(Gears$Gear_level2)==T, "Not specified", Gears$Gear_level2)
GearTot                              <- Gears[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Gear_level1"]
Gears                                <- merge(Gears, GearTot, by="Gear_level1")

noCol                                <- length(unique(Gears$Gear_level2))
mycolors                             <- sample(colorRampPalette(brewer.pal(12,"Paired"))(noCol))


p <- ggplot(Gears, aes(NrPaps, reorder(Gear_level1, TotNrPaps), fill=Gear_level2)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Gears$TotNrPaps+2))) +
  scale_fill_manual(values = mycolors) +
  # scale_fill_manual(values=viridis(13), name= "Specific gear") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("GearTypeGears.png", p, path=outPathPET, width = 8, height = 4)


#-----------------------------------------------#
### Gear and ecosystem component ----
#-----------------------------------------------#

Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
GearTot                              <- Gears[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Gear_level1"]
Gears                                <- merge(Gears, GearTot, by="Gear_level1")

p <- ggplot(Gears, aes(NrPaps, reorder(Gear_level1, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Gears$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Ecosystem component") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("GearsEcoComp.png", p, path=outPathPET, width = 7, height = 4)


#-----------------------------------------------#
### COMBINED PLOT: Ecosystem component by region and Gear and ecosystem component ----
#-----------------------------------------------#

# First plot
EcoComp                              <- subset_PET[, .(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Region")]
EcoTot                               <- EcoComp[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Ecosystem.component_level1"]
EcoComp                              <- merge(EcoComp, EcoTot, by="Ecosystem.component_level1")
EcoComp$Region[EcoComp$Region %in% "NE-Atlantic"] <- "Northeast Atlantic"

p1 <- ggplot(EcoComp, aes(NrPaps, reorder(Ecosystem.component_level1, TotNrPaps), fill=Region)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 10, limits = c(0,max(EcoComp$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(8), name= "Region") +
  labs(x="Number of papers", y="Ecosystem component") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank(),
        # legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.86,0.30),
        legend.key.size = unit(0.75,"line"))
print(p1)

# Second plot
Gears                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
GearTot                              <- Gears[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Gear_level1"]
Gears                                <- merge(Gears, GearTot, by="Gear_level1")

p2 <- ggplot(Gears, aes(NrPaps, reorder(Gear_level1, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Gears$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Ecosystem component") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.86,0.21),
        legend.key.size = unit(0.75,"line"))
print(p2)

# Combine plots and save
p <- p1 / p2 + plot_annotation(tag_levels = 'A')

print(p)

ggsave("EcoCompRegion_GearsEcoComp.png", p, path=outPathPET, width = 8, height = 7)



#-----------------------------------------------#
## Pressure ----
#-----------------------------------------------#

Press                                <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Ecosystem.component_level1", "Pressure.variable_category")]
PressTot                             <- Press[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Pressure.variable_category"]
Press                                <- merge(Press, PressTot, by="Pressure.variable_category")

p <- ggplot(Press, aes(NrPaps, reorder(Pressure.variable_category, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Press$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Ecosystem component") +
  labs(x="Number of papers", y="Pressure variable") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("PressEcoComp.png", p, path=outPathPET, width = 9, height = 4)


#-----------------------------------------------#
## Methods ----
#-----------------------------------------------#

#-----------------------------------------------#
### By ecosystem component ----
#-----------------------------------------------#

Methods                              <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Sampling.Method.used.for.data.collection","Ecosystem.component_level1")]
MethTot                              <- Methods[, .(TotNrPaps = sum(NrPaps)),
                                              by = "Sampling.Method.used.for.data.collection"]
Methods                              <- merge(Methods, MethTot, by="Sampling.Method.used.for.data.collection")

p <- ggplot(Methods, aes(NrPaps, reorder(Sampling.Method.used.for.data.collection, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Methods$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Ecosystem component") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("MethodsEcoComp.png", p, path=outPathPET, width = 10, height = 4)


#-----------------------------------------------#
### By response variable ----
#-----------------------------------------------#

Methods                              <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Sampling.Method.used.for.data.collection","Response.variable_category")]
MethTot                              <- Methods[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Sampling.Method.used.for.data.collection"]
Methods                              <- merge(Methods, MethTot, by="Sampling.Method.used.for.data.collection")

p <- ggplot(Methods, aes(NrPaps, reorder(Sampling.Method.used.for.data.collection, TotNrPaps), fill=Response.variable_category)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Methods$TotNrPaps+2))) +
  # scale_fill_manual(values=viridis(11), name= "Response") +
  scale_fill_manual(values = brewer.pal(12,"Paired")[-11], name = "Response measured") +
  labs(x="Number of papers", y="Sampling method") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("MethodsRes.png", p, path=outPathPET, width = 10, height = 4)


#-----------------------------------------------#
## Response variable ----
#-----------------------------------------------#

#-----------------------------------------------#
### By ecosystem component ----
#-----------------------------------------------#

Resp                                 <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Response.variable_category","Ecosystem.component_level1")]
RespTot                              <- Resp[, .(TotNrPaps = sum(NrPaps)),
                                                by = "Response.variable_category"]
Resp                              <- merge(Resp, RespTot, by="Response.variable_category")

p <- ggplot(Resp, aes(NrPaps, reorder(Response.variable_category, TotNrPaps), fill=Ecosystem.component_level1)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Resp$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Ecosystem component") +
  labs(x="Number of papers", y="Response measured") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank())
print(p)

ggsave("RespVarEcoComp.png", p, path=outPathPET, width = 10, height = 4)


#-----------------------------------------------#
### By sampling method ----
#-----------------------------------------------#

Resp                              <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Response.variable_category","Sampling.Method.used.for.data.collection")]
RespTot                           <- Resp[, .(TotNrPaps = sum(NrPaps)),
                                             by = "Response.variable_category"]
Resp                              <- merge(Resp, RespTot, by="Response.variable_category")

p <- ggplot(Resp, aes(NrPaps, reorder(Response.variable_category, TotNrPaps), fill=Sampling.Method.used.for.data.collection)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Resp$TotNrPaps+3))) +
  # scale_fill_manual(values=viridis(11), name= "Sampling method") +
  scale_fill_manual(values = brewer.pal(12,"Paired")[-11], name = "Sampling method") +
  labs(x="Number of papers", y="Response measured") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.73,0.35),
        legend.key.size = unit(0.85,"line"))
print(p)

ggsave("RespVarMethods.png", p, path=outPathPET, width = 8, height = 4)


## Alternative plot
Resp                              <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                by = c("Response.variable_category","Pressure.variable_category","Sampling.Method.used.for.data.collection")]
RespTot                           <- Resp[, .(TotNrPaps = sum(NrPaps)),
                                          by = "Response.variable_category"]
Resp                              <- merge(Resp, RespTot, by="Response.variable_category")

p <- ggplot(Resp, aes(reorder(Pressure.variable_category, TotNrPaps), NrPaps, fill=Sampling.Method.used.for.data.collection)) +
  geom_bar(stat="identity") +
  # scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Resp$TotNrPaps+2))) +
  # scale_fill_manual(values=viridis(11), name= "Sampling method") +
  scale_fill_manual(values = brewer.pal(12,"Paired")[-11], name = "Sampling method") +
  labs(x="Number of papers", y="Response measured") +
  theme_bw() +
  theme(axis.text = element_text(size = 12, angle=90, hjust=1, vjust=1),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~Response.variable_category)
print(p)

ggsave("RespVarMethodsPress.png", p, path=outPathPET, width = 10, height = 6)


#-----------------------------------------------#
### By Pressure and Direction of relationship ----
#-----------------------------------------------#

Resp                                 <- subset_PET[,.(NrPaps = length(unique(SW.ID))),
                                                   by = c("Response.variable_category","Pressure.variable_category","Direction.of.relationship")]
RespTot                              <- Resp[, .(TotNrPaps = sum(NrPaps)),
                                             by = "Response.variable_category"]
Resp                              <- merge(Resp, RespTot, by="Response.variable_category")

p <- ggplot(Resp, aes(NrPaps, reorder(Response.variable_category, TotNrPaps), fill=Direction.of.relationship)) +
  geom_bar(stat="identity") +
  scale_x_continuous(expand = c(0,0), n.breaks = 6, limits = c(0,max(Resp$TotNrPaps+2))) +
  scale_fill_manual(values=viridis(5), name= "Direction of\nrelationship") +
  labs(x="Number of papers", y="Gear") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~Pressure.variable_category, scales = "free_x")
print(p)

ggsave("RespPressDir.png", p, path=outPathPET, width = 10, height = 6)



#-----------------------------------------------#
## Explore direction of relationship ----
#-----------------------------------------------#

# For several pressures, we do not expect a positive direction of relationship with most of the response variables: 
# - Bycatch
# - Mortality
# - Presence of fishing activity
# - Fishing effort (no positive direction)
# - Fleet capacity (no positive direction)
# Therefore, check out their relationships in more detail.

# Select cases
datPos                  <- subset(subset_PET, Pressure.variable_category %in% c("Bycatch","Mortality","Presence of fishing activity") &
                                    Direction.of.relationship %in% "Positive")

# Check number of papers
length(unique(datPos$SW.ID)) #8 papers


# Explore pressure Bycatch
datPosByc               <- subset(datPos, Pressure.variable_category %in% "Bycatch")

length(unique(datPosByc$SW.ID)) # 4 papers
unique(datPosByc$SW.ID)

## SW4_0703
# Response is number of stranded dolphins, which has a positive relationship with the pressure. The Response category is Abundance,
# so but this is misleading, as this should be interpreted as the Abundance of the population. So change direction of relationship
# to negative.
# Manually changed in data processing script Step 5.

## SW4_0715
# The pressure is not bycatch, but bycatch excluding device, so the pressure variable category should be changed accordingly.
# This will show that using the device has a positive relationship with Abundance, as using the device leads to a higher population
# abundance compared not using the device (which lowers the population abundance). Furthermore, the row that now reports the impact
# of using NO device can be dropped.
# Manually changed in the data processing script Step 4.

## SW4_1355
# Response variable is entanglement in nets, so makes sense there's a positive relationship between Bycatch as pressure and
# Damange & entanglement as response variable.

## SW4_0314
# Pressure reported as catch (target species) and bycatch (bycatch species), but paper is actually a gear comparison: lure vs bait
# recreational angling. So pressure variable category has to be changed to Bycatch reduction & selectivity.
# Manually changed in the data processing script Step 4.


# Explore pressure Presence of fishing activity
datPosAct               <- subset(datPos, Pressure.variable_category %in% "Presence of fishing activity")

length(unique(datPosAct$SW.ID)) # 3 papers
unique(datPosAct$SW.ID)

## SW4_0285
# Changed response variable from Abundance to Behaviour as it was about the size of aggregations of dolphins rather than actual
# population size. The link with bycatch is only made implicitly.
# Manually changed in the data extraction file.

## SW4_1457
# Positive relationship between the presence of gillnets and the time spent by dolphins in the study area, so makes sense.

## SW4_1223
# Instead of looking at the presence of fishing activity, it looks at the effect of an MPA on the fish community.
# So pressure variable should be changed to Closure. One elasmobranch species decreased in mean size since the MPA was
# established, while other species increased. Hence, the negative relationship between Closure and size/age structure.
# But, the decline could be caused by other factors not accounted for, so this is marked in quality of the methods.
# Manually changed in the data extraction file.


# Explore pressure Mortality
datPosMor               <- subset(datPos, Pressure.variable_category %in% "Mortality")

length(unique(datPosMor$SW.ID)) # 1 paper
unique(datPosMor$SW.ID)

## SW4_0209
# Although total mortality is estimated, this is done based on several analyses, including modelling the spatial overlap
# between at-sea origin of stranded dolphins and fishing effort from VMS data. So the pressure variable should be changed to
# fishing effort rather than mortality. 
# The response variable is density of bycaught dead dolphins. This can be reflected either as Mortality or Abundance (in terms 
# of population) size as response variable category. As the paper focuses on estimating mortality rather than any 
# subsequent effects of that mortality on population size, the response variable category will be changed to Mortality.
# Manually changed in the data extraction file.



# For other pressures, we do not necessarily expect a negative direction of relationship with most of the response variables: 
# - Closure
# - Bycatch reduction & selectivity
# Therefore, check out their relationships in more detail.

# Select cases
datNeg                  <- subset(subset_PET, Pressure.variable_category %in% c("Closure","Bycatch reduction & selectivity") &
                                    Direction.of.relationship %in% "Negative")

# Check number of papers
length(unique(datNeg$SW.ID)) #5 papers


# Explore pressure Closure
datNegClo               <- subset(datNeg, Pressure.variable_category %in% "Closure")

length(unique(datNegClo$SW.ID)) # 1 paper
unique(datNegClo$SW.ID)

## SW4_1531
# A mistake was made in reporting the directions of relationship, as biomass is predicted to increase for elasmobranchs.
# Manually changed in data extraction file.


# Explore pressure Bycatch reduction & selectivity
datNegByc               <- subset(datNeg, Pressure.variable_category %in% "Bycatch reduction & selectivity")

length(unique(datNegByc$SW.ID)) # 4 papers
unique(datNegByc$SW.ID)

## SW4_0333
# Paper reports on negative relationship between use of excluding device and presence of turtles in the catch, meaning
# that when the device was used, no turtles were caught. So makes sense to have negative relationship with Behaviour
# as response variable.

## SW4_1107
# In the database, a negative relationship between the use of an excluding device and catch rate of turtles is reported.
# This is categorized as Abundance as response variable, but this implies a negative relationship between the device
# and population abundance of turtles, which is not the case. Furthermore, the paper could not assess the effect of the
# device in detail, as only one turtle entered the device but successfully escaped. So let us change the reported
# response variable from 'catch rate' to 'bycatch' (as there is no rate reported), the categorized response variable from
# Abundance to Behaviour.
# Changes made in data processing script Step 5.

## SW4_0355
# Paper compares the commonly used grid as well as a sieve, plus the combination of the two in its effects on catch and 
# bycatch. The grid is already used in the fishery, the sieve not yet. Use of the sieve leads to fewer smaller individual
# redfish being caught, hence the reported negative relationship between the use of the device and the size/age structure.
# Yet this may be interpreted as the device reducing the size/age structure of the population, while it is the catch.
# Therefore, change the response variable category to Behaviour.
# Changes made in data processing script Step 5.

## SW4_1991
# Paper studies and compares the selectivity of gill nets with different mesh sizes for non-target fish species. It was
# reports a negative relationship between the Pressure and length -> Size/age structure as the response variable. The 
# paper found that a larger mesh size resulted in a greater catch length, and that is therefore important to use the  
# largest mesh size to ensure stock sustainability of non-target fish.
# Let's change the current reported pressure variable 'change in selectivity' to 'increase in mesh size'. This will then
# result in a positive relationship (rather then the current negative relationship). Also add a row for gear comparison.
# Manually changed in data processing scripts 4 and 5.



#-----------------------------------------------#
## Sankey diagrams ----
#-----------------------------------------------#

#-----------------------------------------------#
### Ecosystem component - Response - Direction ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Ecosystem.component_level1", "Response.variable_category", "Direction.of.relationship")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Ecosystem.component_level1", "Response.variable_category", "Direction.of.relationship"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Ecosystem.component_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Ecosystem\ncomponent","Response\nvariable","Direction\nof relationship")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_EcoCompResDir.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 100,
       units = "mm")


#-----------------------------------------------#
### Ecosystem component - Pressure - Response variable - Direction ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Ecosystem.component_level1", "Pressure.variable_category","Response.variable_category", "Direction.of.relationship")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Ecosystem.component_level1", "Pressure.variable_category",
                                                    "Response.variable_category", "Direction.of.relationship"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Direction.of.relationship), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:5),
                     labels = c("Ecosystem\ncomponent","Pressure","Response\nmeasured","Direction\nof relationship")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_EcoCompPressResDir.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Ecosystem component - Pressure - Direction ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Ecosystem.component_level1", "Pressure.variable_category", "Direction.of.relationship")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Ecosystem.component_level1", "Pressure.variable_category", "Direction.of.relationship"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Ecosystem.component_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Ecosystem\ncomponent","Pressure","Direction\nof relationship")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_EcoCompPressDir.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")


#-----------------------------------------------#
### Pressure - Direction - Ecosystem component ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Pressure.variable_category","Direction.of.relationship","Ecosystem.component_level1")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Pressure.variable_category","Direction.of.relationship","Ecosystem.component_level1"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Direction.of.relationship), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Pressure","Direction\nof relationship","Ecosystem\ncomponent")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_PressDirEcoComp.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")


#-----------------------------------------------#
### Pressure - Direction - Response ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Pressure.variable_category","Direction.of.relationship","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR),]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Pressure.variable_category","Direction.of.relationship","Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Pressure.variable_category), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray90") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=3.1) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Pressure","Direction\nof relationship","Response\nmeasured"),
                     expand = c(0.12, 0.05)) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_PressDirResp.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Gear - Direction - Response ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Gear_level1","Direction.of.relationship","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1
tempPETDR$Gear_level1[is.na(tempPETDR$Gear_level1)]   <- "Not specified"

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Gear_level1","Direction.of.relationship","Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Direction.of.relationship), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray70") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Gear","Direction\nof relationship","Response\nmeasured"),
                     expand = c(0.12, 0.05)) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_GearDirResp.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Gear - Ecosystem component - Response ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Gear_level1","Ecosystem.component_level1","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1
tempPETDR$Gear_level1[is.na(tempPETDR$Gear_level1)]   <- "Not specified"

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Gear_level1","Ecosystem.component_level1","Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Gear_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray70") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Gear","Direction\nof relationship","Response\nmeasured"),
                     expand = c(0.12, 0.05)) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_GearEcoResp.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Ecosystem component - Gear - Pressure - Response variable ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Ecosystem.component_level1", "Gear_level1", "Pressure.variable_category","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Mark NAs as not specified
tempPETDR$Gear_level1     <- with(tempPETDR, ifelse(is.na(Gear_level1), "Not specified", Gear_level1))

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Ecosystem.component_level1", "Gear_level1", "Pressure.variable_category",
                                                    "Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Ecosystem.component_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:5),
                     labels = c("Ecosystem\ncomponent","Gear","Pressure","Response\nmeasured")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_EcoCompGearPressRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Study Type - Pressure - Response variable ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Study.type", "Pressure.variable_category","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Study.type", "Pressure.variable_category",
                                                    "Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Study.type), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Study type","Pressure","Response\nmeasured")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_StudyPressRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")


#-----------------------------------------------#
### Sampling method - Response variable ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Sampling.Method.used.for.data.collection", "Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Re-label long names
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"] <- "Fisheries\nDependent Data"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Visual Analyses of Quadrats/Transects"] <- "Visual Analyses of\nQuadrats/Transects"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "Regular Fisheries\nIndependent Survey"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "Irregular Fisheries\nIndependent Survey"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Data Storage, GPS, Acoustic Taggin"] <- "Data Storage, GPS,\nAcoustic Tagging"

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Sampling.Method.used.for.data.collection", "Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sampling.Method.used.for.data.collection), alpha = 0.7, axis.width = 0.1, show.legend = FALSE, sep = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray70", sep = 0.2) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4, sep = 0.2
                            # aes(label = after_stat(paste("", label, sep = "\n")))
  ) +
  scale_x_continuous(breaks = c(2:3),
                     labels = c("Sampling\nmethod","Response\nmeasured"),
                     expand = c(0.12, 0.05)) +
  # scale_fill_viridis_d() +
  # scale_fill_brewer(type = "qual", palette = "Paired") +
  # scale_fill_manual(values = brewer.pal(12, "Set3")[-2]) +
  scale_fill_manual(values = brewer.pal(12, "Paired")[-11]) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_MethodRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
### Sampling method - Pressure - Response variable ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Sampling.Method.used.for.data.collection", "Pressure.variable_category","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1

# Re-label long names
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"] <- "Fisheries\nDependent Data"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Visual Analyses of Quadrats/Transects"] <- "Visual Analyses of\nQuadrats/Transects"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "Regular Fisheries\nIndependent Survey"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "Irregular Fisheries\nIndependent Survey"
tempPETDR$Sampling.Method.used.for.data.collection[tempPETDR$Sampling.Method.used.for.data.collection %in% "Data Storage, GPS, Acoustic Taggin"] <- "Data Storage, GPS,\nAcoustic Tagging"

# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Sampling.Method.used.for.data.collection", "Pressure.variable_category",
                                                    "Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sampling.Method.used.for.data.collection), alpha = 0.7, axis.width = 0.1, show.legend = FALSE, sep = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray70", sep = 0.2) +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4, sep = 0.2
                            # aes(label = after_stat(paste("", label, sep = "\n")))
                            ) +
  scale_x_continuous(breaks = c(2:4),
                     labels = c("Sampling\nmethod","Pressure","Response\nmeasured"),
                     expand = c(0.12, 0.05)) +
  # scale_fill_viridis_d() +
  # scale_fill_brewer(type = "qual", palette = "Paired") +
  # scale_fill_manual(values = brewer.pal(12, "Set3")[-2]) +
  scale_fill_manual(values = brewer.pal(12, "Paired")[-11]) +
    theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_MethodPressRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")


#-----------------------------------------------#
### Fishery - Gear - Pressure - Ecosystem component - Response ----
#-----------------------------------------------#

# Data prepping
tempPETDR       <- subset_PET[, c("SW.ID","Fishery.type", "Gear_level1","Pressure.variable_category","Ecosystem.component_level1","Response.variable_category")]
tempPETDR       <- tempPETDR[!duplicated(tempPETDR), ]
tempPETDR$value <- 1
tempPETDR$Gear_level1                <- with(tempPETDR, ifelse(is.na(Gear_level1),"Unknown ",Gear_level1))


# Transform data so that it can be handled by ggplot2
datTr           <- gather_set_data(tempPETDR, x = c("Fishery.type", "Gear_level1", "Pressure.variable_category",
                                                    "Ecosystem.component_level1", "Response.variable_category"), id_name = "SW.ID")

# Plot Sankey diagram
p <- ggplot(datTr, aes(x, id = SW.ID, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Ecosystem.component_level1), alpha = 0.3, axis.width = 0.1, show.legend = FALSE) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "gray60") +
  geom_parallel_sets_labels(colour = 'black', angle = 0, size=2.4) +
  scale_x_continuous(breaks = c(2:6),
                     labels = c("Fishery","Gear","Pressure","Ecosystem\ncomponent","Response\nmeasured")) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(p)

ggsave(plot = p,
       filename = paste0(outPathPET, "Sankey_FishGearPresEcoRes.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



#-----------------------------------------------#
## Spatial Scales ----
#-----------------------------------------------#

## Make spatial extent and scale categories
subset_PET$ScaleSpatial <- factor(x = subset_PET$Scale...Spatial..m.,
                              levels = c("Not specified","0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                              ordered = TRUE)
subset_PET$ResSpatial <- factor(x = subset_PET$Resolution...Spatial..m.,
                            levels = c("Not specified","0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                            ordered = TRUE)

spatResEx_cat <- expand.grid(levels(subset_PET$ScaleSpatial),
                             levels(subset_PET$ScaleSpatial))

colnames(spatResEx_cat) <- c("SpatialExtent_m", "SpatialRes_m")

## Make counts of articles in different combinations of SPATIAL EXTENTS & RESOLUTIONS
spatResEx_count <- aggregate(SW.ID~ScaleSpatial+ResSpatial,
                             data = subset_PET[!duplicated(subset_PET$SW.ID), ],
                             FUN = length)

names(spatResEx_count)[names(spatResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatResEx_count <- merge(x = spatResEx_cat,
                         y = spatResEx_count,
                         by.y = c("ScaleSpatial", "ResSpatial"),
                         by.x = c("SpatialExtent_m", "SpatialRes_m"),
                         all.x = TRUE)

spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPathPET, "spatialResVExt.png"),
       device = "png",
       dpi = 300,
       width = 160,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatResEx_count,
                   mapping = aes(x = SpatialExtent_m,
                                 y = SpatialRes_m,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0,
                                          name="No. of\narticles") +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Spatial Sampling Resolution (m)") +
         xlab("Spatial Sampling Extent (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)

#-----------------------------------------------#
## Temporal Scales ----
#-----------------------------------------------#

## Make temporal extent and scale categories
subset_PET$ScaleTemporal <- subset_PET$Scale...Temporal
subset_PET$ScaleTemporal[subset_PET$ScaleTemporal %in% "snapshot/no repeated sampling"] <- "snapshot/no\nrepeated sampling"
subset_PET$ScaleTemporal <- factor(x = subset_PET$ScaleTemporal,
                               levels = c("Not specified","snapshot/no\nrepeated sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                               ordered = TRUE)
subset_PET$ResTemporal <- subset_PET$Resolution...Temporal
subset_PET$ResTemporal[subset_PET$ResTemporal %in% "snapshot/no repeat sampling"] <- "snapshot/no\nrepeated sampling"
subset_PET$ResTemporal <- factor(x = subset_PET$ResTemporal,
                             levels = c("Not specified","snapshot/no\nrepeated sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                             ordered = TRUE)

tempResEx_cat <- expand.grid(levels(subset_PET$ScaleTemporal),
                             levels(subset_PET$ScaleTemporal))

colnames(tempResEx_cat) <- c("TemporalExtent", "TemporalRes")

## Make counts of articles in different combinations of TEMPORAL EXTENTS & RESOLUTIONS
tempResEx_count <- aggregate(SW.ID~ScaleTemporal+ResTemporal,
                             data = subset_PET[!duplicated(subset_PET$SW.ID), ],
                             FUN = length)

names(tempResEx_count)[names(tempResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


tempResEx_count <- merge(x = tempResEx_cat,
                         y = tempResEx_count,
                         by.y = c("ScaleTemporal", "ResTemporal"),
                         by.x = c("TemporalExtent", "TemporalRes"),
                         all.x = TRUE)

tempResEx_count[is.na(tempResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPathPET, "temporalResVExt.png"),
       device = "png",
       dpi = 300,
       width = 160,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = tempResEx_count,
                   mapping = aes(x = TemporalExtent,
                                 y = TemporalRes,
                                 fill = NumberOfArticles)) +
         # scale_fill_continuous_sequential(palette = "blues3",
         #                                  rev = TRUE,
         #                                  na.value = 0,
         #                                  name="No. of\narticles") +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Temporal Sampling Resolution") +
         xlab("Temporal Sampling Extent") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)


#-----------------------------------------------#
## SpatioTemporal Extent ----
#-----------------------------------------------#
## Dependent on code above

# Make spatio-temperal extent and resolution category matrix in long form
spatempResEx_cat <- expand.grid(levels(subset_PET$ScaleSpatial),
                                levels(subset_PET$ScaleTemporal))

colnames(spatempResEx_cat) <- c("SpatialScale_m", "TemporalScale")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL EXTENTS
spatempEx_count <- aggregate(SW.ID~ScaleSpatial+ScaleTemporal,
                             data = subset_PET[!duplicated(subset_PET$SW.ID), ],
                             FUN = length)
names(spatempEx_count)[names(spatempEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempEx_count <- merge(x = spatempResEx_cat,
                         y = spatempEx_count,
                         by.y = c("ScaleSpatial", "ScaleTemporal"),
                         by.x = c("SpatialScale_m", "TemporalScale"),
                         all.x = TRUE)

spatempEx_count[is.na(spatempEx_count$NumberOfArticles), "NumberOfArticles"] <- 0


## Plot
ggsave(filename = paste0(outPathPET, "spatiotemporalExt.png"),
       device = "png",
       dpi = 300,
       width = 140,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatempEx_count,
                   mapping = aes(x = SpatialScale_m,
                                 y = TemporalScale,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0,
                                          name="No. of\narticles") +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Temporal Sampling Extent") +
         xlab("Spatial Sampling Extent (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)

#-----------------------------------------------#
## SpatioTemporal Resolution ----
#-----------------------------------------------#
## Dependent on code above

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = subset_PET[!duplicated(subset_PET$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempResEx_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialScale_m", "TemporalScale"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPathPET, "spatiotemporalRes.png"),
       device = "png",
       dpi = 300,
       width = 140,
       height = 90,
       units = "mm",
       plot = 
         ggplot() +
         geom_tile(data = spatempRes_count,
                   mapping = aes(x = SpatialScale_m,
                                 y = TemporalScale,
                                 fill = NumberOfArticles)) +
         scale_fill_continuous_sequential(palette = "blues3",
                                          rev = TRUE,
                                          na.value = 0,
                                          name="No. of\narticles") +
         scale_x_discrete(drop = FALSE) +
         scale_y_discrete(drop = FALSE) +
         ylab("Temporal Sampling Resolution") +
         xlab("Spatial Sampling Resolution (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)


### COMBINE figures into one plot ----

pSpatial <- ggplot() +
  geom_tile(data = spatResEx_count,
            mapping = aes(x = SpatialExtent_m,
                          y = SpatialRes_m,
                          fill = NumberOfArticles)) +
  scale_fill_continuous_sequential(palette = "blues3",
                                   rev = TRUE,
                                   na.value = 0,
                                   name="No. of\npapers",
                                   n.breaks = 8) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  ylab("Spatial Sampling Resolution (m)") +
  xlab("Spatial Sampling Extent (m)") +
  theme_few() +
  theme(text = element_text(size = 9), 
        # axis.text.x = element_text(hjust = 0.5, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  )

pTemporal <- ggplot() +
  geom_tile(data = tempResEx_count,
            mapping = aes(x = TemporalExtent,
                          y = TemporalRes,
                          fill = NumberOfArticles)) +
  scale_fill_continuous_sequential(palette = "blues3",
                                   rev = TRUE,
                                   na.value = 0,
                                   name="No. of\npapers",
                                   n.breaks=6) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  ylab("Temporal Sampling Resolution") +
  xlab("Temporal Sampling Extent") +
  theme_few() +
  theme(text = element_text(size = 9), 
        # axis.text.x = element_text(hjust = 0.5, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  )

p <- pSpatial / pTemporal + plot_annotation(tag_levels = 'A')

ggsave("spatialTemporalScales2.png", p, path=outPathPET, width = 7, height = 7)
