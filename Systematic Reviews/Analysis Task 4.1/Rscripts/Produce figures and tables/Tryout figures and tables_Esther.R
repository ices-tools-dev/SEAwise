#####################################################################################################################-
#####################################################################################################################-
#
#     Script with some trials and temporary figures and tabels by Esther
#
#     By Esther Beukhof, including code written by Karin van der Reijden
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
# Worked-out examples ----
#-----------------------------------------------#

#-----------------------------------------------#
## Physical disturbance benthos -----
#-----------------------------------------------#

# Subset data
datasub                              <- subset(data, Pressure.type %in% "Physical disturbance of the seabed")
length(unique(datasub$SW.ID)) #182 papers


# Convert all survival studies to mortality studies
table(datasub$Response.variable_category)
datasub$Direction.of.relationship    <- with(datasub, ifelse(Response.variable_category %in% "Survival" & 
                                                               Direction.of.relationship %in% "Positive","Negative",
                                                             ifelse(Response.variable_category %in% "Survival" &
                                                                      Direction.of.relationship %in% "Negative","Positive",Direction.of.relationship)))
datasub$Response.variable_category   <- with(datasub, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))
table(datasub$Response.variable_category)


# Barplot with number of papers by ecosystem component
EcoComp                               <- datasub[, .(NrPaps = length(unique(SW.ID))), by = "Ecosystem.component_level1"]
EcoComp                               <- EcoComp[, .(NrPaps = sum(NrPaps)), by="Region"]
EcoComp                               <- EcoComp[order(NrPaps, decreasing = F),,]

tiff(paste0(outPath, "EcoComp_PhysDist.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 16, 4, 2))
b                                     <- barplot(EcoComp$NrPaps, horiz=TRUE, axes=F, xlim=c(0,max(EcoComp$NrPaps)+10), col=viridis(3)[2])
box()
axis(2, at=b, labels=EcoComp$Ecosystem.component_level1, las=1, cex.axis=2)
axis(1, at= seq(0,max(EcoComp$NrPaps), 20), labels=seq(0, max(EcoComp$NrPaps), 20), cex.axis=1.5)
# title(main="Number of papers per region", cex.main=1.5, font.main=2)
text(x=80, y=0, paste0("Total number of unique papers ", length(unique(datasub$SW.ID))), pos=4)
text(x=EcoComp$NrPaps + 4, y=b, EcoComp$NrPaps, cex=1.3)
axis(1, at=max(EcoComp$NrPaps)/2, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()



# Subset data further to benthos
datasub                              <- subset(data, Pressure.type %in% "Physical disturbance of the seabed" &
                                                 Ecosystem.component_level1 %in% "Benthos")
length(unique(datasub$SW.ID)) #127 papers

# Convert all survival studies to mortality studies
table(datasub$Response.variable_category)
datasub$Direction.of.relationship    <- with(datasub, ifelse(Response.variable_category %in% "Survival" & 
                                                               Direction.of.relationship %in% "Positive","Negative",
                                                             ifelse(Response.variable_category %in% "Survival" &
                                                                      Direction.of.relationship %in% "Negative","Positive",Direction.of.relationship)))
datasub$Response.variable_category   <- with(datasub, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))
table(datasub$Response.variable_category)


# Barplot with number of papers by region
Regions                               <- datasub[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
Regions                               <- Regions[, .(NrPaps = sum(NrPaps)), by="Region"]
Regions                               <- Regions[order(NrPaps, decreasing = F),,]

tiff(paste0(outPath, "Regions_PhysBenthos.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 16, 4, 2))
b                                     <- barplot(Regions$NrPaps, horiz=TRUE, axes=F, xlim=c(0,max(Regions$NrPaps)+5), col=viridis(3)[2])
box()
axis(2, at=b, labels=Regions$Region, las=1, cex.axis=2)
axis(1, at= seq(0,max(Regions$NrPaps), 10), labels=seq(0, max(Regions$NrPaps), 10), cex.axis=1.5)
# title(main="Number of papers per region", cex.main=1.5, font.main=2)
text(x=80, y=0, paste0("Total number of unique papers ", length(unique(datasub$SW.ID))), pos=4)
text(x=Regions$NrPaps + 1.5, y=b, Regions$NrPaps, cex=1.3)
axis(1, at=max(Regions$NrPaps)/2, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


# Barplot for Response variable categories
ResVarCats                            <- datasub[, .(NrPaps = length(unique(SW.ID))),
                                                 by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats_PhysBenthos.tiff"), width=1200, height=750, res=100)
par(mar=c(5, 23, 2, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,70), col=viridis(3)[2])
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=2)
axis(1, at= seq(0,70, 10), labels=seq(0, 70, 10), cex.axis=1.5)
text(x=43, y=0, paste0("Total number of unique papers: ", length(unique(datasub$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+1.5, y=b, ResVarCats$NrPaps, cex = 1.3)
axis(1, at=35, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


# Barplot for Response variable categories including direction of relationship
dataplot                              <- datasub[, .(NrPaps = length(unique(SW.ID))),
                                                 by = c("Response.variable_category", "Direction.of.relationship")]
names(dataplot)                       <- c("ResVar","DirRel","NrPaps")

ResOrder                              <- data.frame(ResVar = unique(dataplot$ResVar),
                                                    Sord    = c(1,2,3,4,7,5,8,6,9,10,12,11))
ResOrder                              <- ResOrder[order(ResOrder$Sord, decreasing = TRUE),]

DirOrder                              <- data.frame(DirRel = unique(dataplot$DirRel),
                                                    Sord    = c(2,1,4,3,5))
DirOrder                              <- DirOrder[order(DirOrder$Sord),]

dataplot2                             <- matrix(nrow=5, ncol=12, dimnames=list(DirOrder$DirRel,ResOrder$ResVar))

for(iRow in c(1:nrow(dataplot))){
  subset                              <- dataplot[iRow,]
  dataplot2[subset$DirRel,subset$ResVar] <- subset$NrPaps
}
dataplot2[is.na(dataplot2)]             <- 0

tiff(paste0(outPath, "ResVarDir_PhysBenthos.tiff"), width=1200, height=750, res=100)
par(mar=c(5,20,2,2))
b <- barplot(dataplot2, xlim=c(0,80), axes=T, col=viridis(5), main=NULL, horiz = TRUE, 
             las=2, axis.lty = "solid", cex.axis = 1.5, cex.names = 1.5)
box()
# axis(2, at=b, labels=colnames(dataplot2), las=1, cex.axis=2)
# axis(1, at= seq(0,100, 10), labels=seq(0, 100, 10), cex.axis=1.5)
axis(1, at=50, "Number of papers", cex.axis=1.5, tick=F, line=1.5)
legend(x=55, y=4.5, cex=1.5, fill=viridis(5), legend=DirOrder$DirRel)
dev.off()

tiff(paste0(outPath, "ResVarCats_PhysBenthos.tiff"), width=1200, height=750, res=100)
par(mar=c(5, 23, 2, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,70), col=viridis(3)[2])
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=2)
axis(1, at= seq(0,70, 10), labels=seq(0, 70, 10), cex.axis=1.5)
text(x=43, y=0, paste0("Total number of unique papers: ", length(unique(datasub$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+1.5, y=b, ResVarCats$NrPaps, cex = 1.3)
axis(1, at=35, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()


# Table direction of relationship
Direction                            <- datasub[, .(NrPaps = length(unique(SW.ID))), 
                                                by = Direction.of.relationship]
Direction$Prop                       <- Direction$NrPaps / sum(Direction$NrPaps)



dataplot                            <- datasub[, .(NrPaps = length(unique(SW.ID))), 
                                               by = c("Response.variable_category","Direction.of.relationship")]
dataplot                            <- dataplot[order(NrPaps),,]


ggplot(dataplot, aes(NrPaps, Response.variable_category, fill=Direction.of.relationship)) +
  geom_bar(stat = "identity")



#-----------------------------------------------#
## Discarding -----
#-----------------------------------------------#

# Subset data
datasub                              <- subset(data, Pressure.type %in% "Discarding")
length(unique(datasub$SW.ID))


# Barplot for Ecosys comp and Region
dataplot                             <- datasub[, .(NrPaps = length(unique(SW.ID))), 
                                                by = c("Ecosystem.component_level1","Region")]
names(dataplot)                       <- c("EcosysComp","Region","NrPaps")

EcoOrder                              <- data.frame(EcoComp = unique(dataplot$EcosysComp),
                                                    Sord    = c(3,1,4,5,2))
EcoOrder                              <- EcoOrder[order(EcoOrder$Sord),]

RegionOrder                           <- data.frame(Region = unique(dataplot$Region),
                                                    Sord    = c(1,4,3,2,5))
RegionOrder                           <- RegionOrder[order(RegionOrder$Sord),]

dataplot2                             <- matrix(nrow=5, ncol=5, dimnames=list(RegionOrder$Region,EcoOrder$EcoComp))

for(iRow in c(1:nrow(dataplot))){
  subset                              <- dataplot[iRow,]
  dataplot2[subset$Region,subset$EcosysComp] <- subset$NrPaps
}
dataplot2[is.na(dataplot2)]             <- 0

tiff(paste0(outPath, "EcoRegion_DiscSeabirds.tiff"), width = 800, height = 800, res=100)
par(mar=c(10,5,2,0))
b <- barplot(dataplot2, ylim=c(0,25), axes=F, names.arg=rep("", 5), width=1, xlim=c(0,6), col=viridis(5), main=NULL)
axis(2, at=seq(0,25,5), cex.axis=1.5, las=1, pos=0)
axis(1, at=b, colnames(dataplot2), las=3, cex.axis=1.5)
axis(2, at=15, "Number of papers", cex.axis=2, tick=F, line=1.5)
legend(x=3.5, y=25, cex=1.5, fill=viridis(5), legend=RegionOrder$Region)
dev.off()


# Barplot for Response variable and Direction of relationship Seabirds
dataplot                             <- subset(datasub, Ecosystem.component_level1 %in% "Seabirds")
dataplot                             <- dataplot[, .(NrPaps = length(unique(SW.ID))), 
                                                 by = c("Response.variable_category","Direction.of.relationship")]
names(dataplot)                       <- c("ResVar","DirRel","NrPaps")

ResOrder                              <- data.frame(ResVar = unique(dataplot$ResVar),
                                                    Sord    = c(1,2,3,6,5,7,4))
ResOrder                              <- ResOrder[order(ResOrder$Sord),]

DirOrder                              <- data.frame(DirRel = unique(dataplot$DirRel),
                                                    Sord    = c(1,2,3,4))
DirOrder                              <- DirOrder[order(DirOrder$Sord),]

dataplot2                             <- matrix(nrow=4, ncol=7, dimnames=list(DirOrder$DirRel,ResOrder$ResVar))

for(iRow in c(1:nrow(dataplot))){
  subset                              <- dataplot[iRow,]
  dataplot2[subset$DirRel,subset$ResVar] <- subset$NrPaps
}
dataplot2[is.na(dataplot2)]             <- 0

tiff(paste0(outPath, "ResVarDir_DiscSeabirds.tiff"), width = 800, height = 800, res=100)
par(mar=c(10,5,2,0))
b <- barplot(dataplot2, ylim=c(0,10), axes=F, names.arg=rep("", 7), width=1, col=viridis(4), main=NULL)
axis(2, at=seq(0,10,1), cex.axis=1.5, las=1, pos=0)
axis(1, at=b, colnames(dataplot2), las=3, cex.axis=1.7)
axis(2, at=4, "Number of papers", cex.axis=2, tick=F, line=1.5)
legend(x=6, y=9.5, cex=1.5, fill=viridis(4), legend=DirOrder$DirRel)
dev.off()


## Ggplot versionpar(mar=c(10,5,2,0))
dataplot                             <- subset(datasub, Ecosystem.component_level1 %in% "Seabirds")
ResVarOrder                          <- dataplot[, .(NrPaps = length(unique(SW.ID))), by = "Response.variable_category"]
dataplot                             <- dataplot[, .(NrPaps = length(unique(SW.ID))), 
                                                 by = c("Response.variable_category","Direction.of.relationship")]
dataplot$Response.variable_category  <- factor(dataplot$Response.variable_category, 
                                               levels = ResVarOrder$Response.variable_category[sort(ResVarOrder$NrPaps, decreasing = TRUE)])

p <- ggplot(dataplot, aes(NrPaps, Response.variable_category, fill=Direction.of.relationship)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(values = rev(viridis(5)[c(1:4)])) +
  labs(x="Number of papers") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title.y = element_blank(),
        panel.grid = element_blank())
print(p)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Get citations by paper ----
#-----------------------------------------------#

load("Systematic Reviews/Analysis Task 4.1/Search/search results_citedBy.RData")
