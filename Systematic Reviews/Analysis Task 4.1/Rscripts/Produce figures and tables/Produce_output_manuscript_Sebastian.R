#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths ----
#-----------------------------------------------#

library(data.table)
library(ggplot2)
library(viridis)
library(devtools)
library(ggthemes)
library(dplyr)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(plotrix)
library(grDevices)


devtools::install_github("davidsjoberg/ggsankey")


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
#outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/bycatch case study/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS/"


#-----------------------------------------------#
# Read in data ----
#
#  info:
#  This section depends on the processed data file produced in step 1.
# 
#-----------------------------------------------#
#data                                  <- readRDS(file=paste0(datPath, "data.rds"))
#data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened.rds"))

data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_AllScreened_correctTaxa_PressVar_RespVar_Methods.rds"))


# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                           ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                  ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                         ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# General plots ----
#-----------------------------------------------#

#-----------------------------------------------#
## Time series across all papers ----
#-----------------------------------------------#


## There appear to be Meta analyses included in the data extraction that should have been excluded:
nrow(data[!duplicated(data$SW.ID) & data$Study.type == "Meta-analysis", ])


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


#-----------------------------------------------#
## Barplot for Response variable categories ----
#-----------------------------------------------#

# First manually combine Survival and Mortality studies (all as Mortality)
ResVarCats                            <- data
ResVarCats$Response.variable_category <- with(ResVarCats, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))                            

ResVarCats                            <- ResVarCats[, .(NrPaps = length(unique(SW.ID))),
                                                    by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats.tiff"), width=1200, height=750, res=100)
par(mar=c(5, 23, 2, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,265), col=viridis(3)[2])
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=2)
axis(1, at= seq(0,250, 25), labels=seq(0, 250, 25), cex.axis=1.5)
text(x=162, y=0, paste0("Total number of unique papers: ", length(unique(data$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+7.5, y=b, ResVarCats$NrPaps, cex = 1.3)
axis(1, at=125, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()




#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Subset to bycatch papers ----
#-----------------------------------------------#

data_allScreened[data_allScreened == ""] <- NA 

#Pressure.type = 'discarding' or Catch_and_bycatch
#subset_byc<-data_allScreened[(data_allScreened$Pressure.type%in%c("Catch_and_bycatch","Discarding") & data_allScreened$Pressure_level%in%c("Bycatch", "Non-target", "Not specified", "NA")),]

subset_byc<-data_allScreened[(data_allScreened$Pressure.type%in%c("Catch_and_bycatch")& data_allScreened$Pressure_level%in%c("Bycatch", "Non-target", "Not specified")),]

#subset_dis<-data_allScreened[(data_allScreened$Pressure.type%in%c("Discarding")),]

#intersect(names(subset_byc),names(subset_dis))

#combined_subset<-rbind(subset_byc, subset_dis)


#-----------------------------------------------
## Barplots of fishing gears studied
#-----------------------------------------------
Gears                                <- subset_byc[,.(NrPaps = length(unique(SW.ID))),
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
## Barplot for ecosystem component (level 1)
#-----------------------------------------------
EcoComp                               <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
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

EcoComp                               <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
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



#GGPLOT version


#-----------------------------------------------#
## Heatmap of ecosystem component level 1 vs pressure type level 1 ----
#-----------------------------------------------#

EcoPress                             <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Pressure_level")]
EcoPressExp                          <- expand.grid(Ecosystem.component_level1=EcoPress$Ecosystem.component_level1, 
                                                    Pressure_level=EcoPress$Pressure_level)
EcoPressExp                          <- EcoPressExp[!duplicated(EcoPressExp),]
EcoPressExp                          <- merge(EcoPress, EcoPressExp, by=c("Ecosystem.component_level1","Pressure_level"), all=TRUE)
# EcoPressExp$NrPaps[is.na(EcoPressExp$NrPaps)] <- 0

p <- ggplot(EcoPressExp, aes(x=Pressure_level, y=Ecosystem.component_level1, fill=NrPaps)) +
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


# #-----------------------------------------------
# ## Heatmap of ecosystem component vs fishery type
# #-----------------------------------------------
# EcoFish                              <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
#                                              by = c("Ecosystem.component_level1", "Fishery.type")]
# EcoFishmat                           <- matrix(nrow = length(unique(subset_byc$Ecosystem.component_level1)),
#                                                ncol = length(unique(subset_byc$Fishery.type)))
# colnames(EcoFishmat)                 <- sort(unique(subset_byc$Fishery.type))  
# rownames(EcoFishmat)                 <- sort(unique(subset_byc$Ecosystem.component_level1))
# 
# for(iRow in c(1:nrow(EcoFish))){
#   subdat                             <- EcoFish[iRow,]
#   EcoFishmat[subset_byc$Ecosystem.component_level1, subset_byc$Fishery.type] <- subset_byc$NrPaps
# }
# r                                    <- raster(EcoFishmat)
# #rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
# rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))
# 
# 
# tiff(paste0(outPath, "EcoFish_heatmap.tiff"), width= 1000, height = 1000, res = 100)
# par(mar=c(19, 1, 5, 7))
# plot(r, axes=F,
#      col=rCols, legend=F, box=F)
# segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
# abline(v=c(0,1))
# abline(v=c(1/4, 2/4, 3/4), lty=2, col="lightgrey", lwd=0.8)
# segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
#          y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
# axis(1, at=seq(0.125, 0.875, length.out=4), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
# axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
# par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
# plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title(main="Fishing type studied per ecosystem components", cex.main=1.5, font.main=2)
# gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
# text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
# text("1", x=0.97, y=0.3, font=3)
# text("190", x=0.97, y=0, font=3)
# text("95", x=0.97, y=0.15, font=3)
# dev.off()
# Error in EcoFishmat[subset_byc$Ecosystem.component_level1, subset_byc$Fishery.type] <- subset_byc$NrPaps : 
#   number of items to replace is not a multiple of replacement length



#-----------------------------------------------
## Table of quality scores  
#-----------------------------------------------

dataUnique        <- subset_byc[!duplicated(subset_byc$SW.ID),]

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
## Methods applied in literature
#-----------------------------------------------

## Methods
Qual              <- aggregate(SW.ID ~ Region + Quality...Methods, dataUnique, function(x) length(unique(x)))
names(Qual)       <- c("Region","Quality","NrPaps")
Qual$Region       <- factor(Qual$Region, levels=c("CS - Mediterranean","CS - Western Waters","CS - North Sea", "CS - Baltic Sea",
                                                  "Mediterranean - non CS","Western Waters - non CS","North Sea - non CS","Baltic Sea - non CS",
                                                  "Barents Sea","Black Sea","NE-Atlantic","Global"))

p <- ggplot(Qual, aes(Region,NrPaps, fill=as.factor(Quality))) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0.01,0), n.breaks = 10) +
  scale_fill_manual(values=c("#440154FF","#FDE725FF","#21908CFF"), name= "Quality methods") +
  labs(x="Quality methods", y="Number of papers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
print(p)
ggsave("QualityMethods.tiff", p, path=outPath, width = 8, height = 5)




#-----------------------------------------------
## Plots of Spatial scale & resolution
#-----------------------------------------------


# ggplot2 version
TempSc                                <- subset_byc[,.(NrPaps = length(unique(SW.ID))),
                                                    by = c("Scale...Temporal", "Resolution...Temporal")]
names(TempSc)                         <- c("Scale", "Resolution", "NrPaps")
TempSc[is.na(TempSc)]                 <- "not specified"
TempSc$Resolution                     <- ifelse(TempSc$Resolution == "snapshot/no repeat sampling", "snapshot", TempSc$Resolution)

Temporder                            <- data.frame(Temp = c("not specified","snapshot","subday","day","week","two week","month","two month","quarter",
                                                             "half year","year","two year","five year","decade","multidecadal"))
TempSc$Scale                          <- factor(TempSc$Scale, levels = Temporder$Temp[-2])
TempSc$Resolution                     <- factor(TempSc$Resolution, levels = Temporder$Temp)

p <- ggplot(TempSc, aes(Scale, NrPaps, fill=Resolution)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0,41), expand = c(0,0), n.breaks = 8) +
  scale_fill_viridis_d() +
  labs(y="Number of papers") +
  theme_bw() +
  theme(legend.position = c(0.1,0.6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.minor.y = element_blank())
print(p)

ggsave("Temporalscale&resolution2.png", p, path=outPath, width = 7, height = 6)







































































# #-----------------------------------------------
# ## Heatmap of ecosystem component vs gear_level 1 for Commercial
# #-----------------------------------------------
# 
# EcoFishGear                          <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
#                                              by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
# EcoFishGear                          <- subset(EcoFishGear, Fishery.type %in% "Commercial")
# EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified",
#                                                ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1))
# rCols                                <- data.table(colcode = viridis(max(EcoFishGear$NrPaps)),
#                                                    value = c(1:max(EcoFishGear$NrPaps)))
# 
# EcoFishmat                           <- matrix(nrow = length(unique(EcoFishGear$Ecosystem.component_level1)),
#                                                ncol = length(unique(EcoFishGear$Gear_level1)))
# colnames(EcoFishmat)                 <- sort(unique(EcoFishGear$Gear_level1))
# rownames(EcoFishmat)                 <- sort(unique(EcoFishGear$Ecosystem.component_level1))
# 
# for(iRow in c(1:nrow(EcoFishGear))){
#   subdat                             <- EcoFishGear[iRow,]
#   EcoFishmat[subdat$Ecosystem.component_level1, subdat$Gear_level1] <- subdat$NrPaps
# }
# r                                    <- raster(EcoFishmat)
# #rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
# rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))
# 
# 
# tiff(paste0(outPath, "EcoFishComm_heatmap.tiff"), width= 1000, height = 1000, res = 100)
# par(mar=c(19, 1, 5, 7))
# plot(r, axes=F, col=rCols, legend=F, box=F)
# segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
# abline(v=c(0,1))
# abline(v=c(1/9, 2/9, 3/9, 4/9, 5/9, 6/9, 7/9, 8/9), lty=2, col="lightgrey", lwd=0.8)
# segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11),
#          y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
# axis(1, at=seq(0.05, 0.95, length.out=9), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
# axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
# par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
# plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title(main="Commercial fishing gears studied per ecosystem component", cex.main=1.5, font.main=2)
# gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
# text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
# text("1", x=0.97, y=0.3, font=3)
# text("152", x=0.97, y=0, font=3)
# text("76", x=0.97, y=0.15, font=3)
# dev.off()
# Error in raster(EcoFishmat) : could not find function "raster"

# #-----------------------------------------------
# ## Heatmap of relation direction for ecosystem components 
# #-----------------------------------------------
# RelDirEco                            <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
#                                              by = c("Direction.of.relationship", "Ecosystem.component_level1")]
# RelDirEcomat                         <- matrix(nrow = length(unique(subset_byc$Ecosystem.component_level1)),
#                                                ncol = length(unique(subset_byc$Direction.of.relationship)))
# colnames(RelDirEcomat)               <- sort(unique(subset_byc$Direction.of.relationship))  
# rownames(RelDirEcomat)               <- sort(unique(subset_byc$Ecosystem.component_level1))
# 
# for(iRow in c(1:nrow(RelDirEco))){
#   subdat                             <- RelDirEco[iRow,]
#   RelDirEcomat[subdat$Ecosystem.component_level1, subdat$Direction.of.relationship] <- subdat$NrPaps
# }
# r                                    <- raster(RelDirEcomat)
# 
# #rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
# rCols                                <- viridis(n=max(RelDirEcomat, na.rm=T))   
# 
# tiff(paste0(outPath, "RelDirEco_heatmap.tiff"), width= 1000, height = 1000, res = 100)
# par(mar=c(19, 1, 5, 7))
# plot(r, axes=F,
#      col=rCols, legend=F, box=F)
# segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
# abline(v=c(0,1))
# abline(v=c(1/5, 2/5, 3/5, 4/5), lty=2, col="lightgrey", lwd=0.8)
# segments(x0=0, x1=1, y0=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), 
#          y1=c(1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11,10/11), col="lightgrey", lty=2, lwd=0.8)
# axis(1, at=seq(1/10, 9/10, length.out=5), labels= colnames(RelDirEcomat), las=3, cex.axis=1.5)
# axis(4, at=seq(1/22, 21/22, length.out=11), labels= rev(rownames(RelDirEcomat)), las=1, pos=1, cex.axis=1.5)
# par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
# plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title(main="Direction of relation per ecosystem component", cex.main=1.5, font.main=2)
# gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
# text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
# text("1", x=0.97, y=0.3, font=3)
# text("120", x=0.97, y=0, font=3)
# text("60", x=0.97, y=0.15, font=3)
# dev.off()



# #-----------------------------------------------
# ## Heatmaps of relation direction for response variable categories.
# #-----------------------------------------------
# RelDirRVC                            <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
#                                              by = c("Direction.of.relationship", "Response.variable_category")]
# 
# RelDirRVCmat                         <- matrix(nrow = length(unique(subset_byc$Response.variable_category)),
#                                                ncol = length(unique(subset_byc$Direction.of.relationship)))
# colnames(RelDirRVCmat)               <- sort(unique(subset_byc$Direction.of.relationship))  
# rownames(RelDirRVCmat)               <- sort(unique(subset_byc$Response.variable_category))
# 
# for(iRow in c(1:nrow(RelDirRVC))){
#   subdat                             <- RelDirRVC[iRow,]
#   RelDirRVCmat[subdat$Response.variable_category, subdat$Direction.of.relationship] <- subdat$NrPaps
# }
# r                                    <- raster(RelDirRVCmat)
# rCols                                <- viridis(n=max(RelDirRVC$NrPaps))
# 
# tiff(paste0(outPath, "RelDirRVC_heatmap.tiff"), width= 1000, height = 1000, res = 100)
# par(mar=c(15, 3, 7, 20))
# plot(r, axes=F,
#      col=rCols, legend=F, box=F)
# segments(x0=seq(0, 1, length.out=ncol(RelDirRVCmat)+1), x1=seq(0, 1, length.out=ncol(RelDirRVCmat)+1), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
# segments(x0=0, x1=1, y0=seq(0,1,length.out=nrow(RelDirRVCmat)+1), y1=seq(0,1, length.out=nrow(RelDirRVCmat)+1), col="lightgrey", lty=2, lwd=0.8)
# segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
# segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
# axis(1, at=seq((1/ncol(RelDirRVCmat))/2, 1-((1/ncol(RelDirRVCmat))/2), length.out=ncol(RelDirRVCmat)), labels= colnames(RelDirRVCmat), las=3, cex.axis=1.5, pos=0)
# axis(4, at=seq((1/nrow(RelDirRVCmat))/2, 1-((1/nrow(RelDirRVCmat))/2), length.out=nrow(RelDirRVCmat)), labels = rev(rownames(RelDirRVCmat)), las=1, cex.axis=1.5, pos=1)
# par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,0))
# plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
# title(main="Direction of relationship per reponse variable (Category)", cex.main=2, font.main=2, line=0)
# gradient.rect(xleft=0.75, xright=0.9, ybottom=0.05, ytop=0.15, col=rCols, gradient="x")
# text("Number of \n papers retained", x=0.825, y=0.2, font=4, cex=1.3)
# text("1", x=0.745, y=0.1, font=3)
# text("135", x=0.915, y=0.1, font=3)
# dev.off()
# 
