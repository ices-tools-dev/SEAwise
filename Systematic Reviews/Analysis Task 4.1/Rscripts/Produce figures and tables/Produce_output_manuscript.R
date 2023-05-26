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
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_AllScreened_correctTaxa_PressVar_RespVar_Methods.rds"))


# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                              ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                     ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                            ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

# Load fate of papers
load(paste0(datPath, "FatePapers.Rdata"))


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# General plots across all papers ----
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



#-----------------------------------------------#
## Barplot for ecosystem component ----
#-----------------------------------------------#

EcoComp                               <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Ecosystem.component_level1]
EcoComp                               <- EcoComp[order(NrPaps),,]


tiff(paste0(outPath, "EcoComp.tiff"), width=1000, height=750, res=100)
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
ResVarCats$Response.variable_category <- with(ResVarCats, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))                            

ResVarCats                            <- ResVarCats[, .(NrPaps = length(unique(SW.ID))),
                                              by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats.tiff"), width=1250, height=750, res=100)
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

## Vertical bars
tiff(paste0(outPath, "Pressures.tiff"), width=850, height=1000, res=100)
par(mar=c(25,7, 2, 2))
b                                     <- barplot(Pressure$NrPaps, axes=F, ylim=c(0,350), col=viridis(3)[2])
box()
axis(1, at=b, labels=Pressure$Pressure.type , las=2, cex.axis=1.7)
axis(2, at= seq(0,350, 50), labels=seq(0, 350, 50), cex.axis=1.5, las=1)
text(x=6.3, y=340, paste0("Total number of unique papers: ", length(unique(data$SW.ID))), cex = 1.3)
text(x=b, y=Pressure$NrPaps+10, Pressure$NrPaps, cex = 1.3)
# text(x=b, y=max(subdat$NrPaps)*0.035, labels=subdat$NrPaps, cex=1.5, font=3)
axis(2, at=190, tick=F, line=3, label="Number of papers", cex.axis=2)
dev.off()

## Horizontal bars
Pressure                              <- Pressure[order(NrPaps),,]

tiff(paste0(outPath, "Pressures_hor.tiff"), width=1200, height=750, res=100)
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

tiff(paste0(outPath, "GearsStudiedCommRecr.tiff"), width= 1500, height = 1000, res = 100)
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

ggsave("EcoPress_heatmap.tiff", p, path=outPath, width = 9, height = 7)



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

sum(Regions$NrPaps) #542, suggesting that, out of 528 papers, several of them studied multiple regions


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
ggplotly(sankey)



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
# Case Study Litter: Plots and numbers ----
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
tempLit <- atempLit[!duplicated(atempLit), ]

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
                                                                                       "NA",
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

#-----------------------------------------------#
## Geographic spread ----
#-----------------------------------------------#
tempLitGeo <- litter[!duplicated(litter$SW.ID), ]

tempLitGeo <- as.data.frame(table(tempLitGeo$Region))
colnames(tempLitGeo) <- c("Region", "Number of Articles")
tempLitGeo$Region <- factor(tempLitGeo$Region, levels = c("Barents Sea", "NE-Atlantic", "Baltic Sea", "Mediterranean Sea"))


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
ggsave(filename = paste(outPath, "litter_spatialResVExt.png"),
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
ggsave(filename = paste(outPath, "litter_temporalResVExt.png"),
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
ggsave(filename = paste(outPath, "litter_spatiotemporalExt.png"),
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
ggsave(filename = paste(outPath, "litter_spatiotemporalRes.png"),
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
tempLitDR <- atempLitDR[!duplicated(atempLitDR), ]

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

