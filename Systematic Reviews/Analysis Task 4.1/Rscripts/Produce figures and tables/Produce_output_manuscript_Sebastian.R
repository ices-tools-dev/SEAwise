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
library(colorspace)


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

#Subset to EPT species or task 4.2 papers
subset_byc<-subset_byc[subset_byc$WP4.task%in%c("4.2","4.2 _ 4.3","4.2 _ 4.4"),]
#PET <- rep(NA,nrow(subset_byc)); 
#PET[subset_byc$WP4.task=="4.2" & subset_byc$WP4.task=="4.2 _ 4.3" & subset_byc$WP4.task=="4.2 _ 4.4"] <- "PETspecies";
#subset_byc <- data.frame(subset_byc,PET);

#-----------------------------------------------#
## Barplot for Response variable categories ----
#-----------------------------------------------#

# First manually combine Survival and Mortality studies (all as Mortality)
ResVarCats                            <- subset_byc
ResVarCats$Response.variable_category <- with(ResVarCats, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))                            

ResVarCats                            <- ResVarCats[, .(NrPaps = length(unique(SW.ID))),
                                                    by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats.png"), width=1200, height=750, res=100)
par(mar=c(5, 23, 2, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,265), col=viridis(3)[2])
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=2)
axis(1, at= seq(0,250, 25), labels=seq(0, 250, 25), cex.axis=1.5)
text(x=162, y=0, paste0("Total number of unique papers: ", length(unique(subset_byc$SW.ID))), pos=4)
text(x=ResVarCats$NrPaps+7.5, y=b, ResVarCats$NrPaps, cex = 1.3)
axis(1, at=125, tick=F, line=2, label="Number of papers", cex.axis=2)
dev.off()



#-----------------------------------------------
## Barplots of fishing gears studied
#-----------------------------------------------
Gears                                <- subset_byc[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))

tiff(paste0(outPath, "GearsStudied.png"), width= 2000, height = 1000, res = 100)
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

tiff(paste0(outPath, "EcoComp.png"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(EcoComp$NrPaps, horiz=TRUE, axes=F, xlim=c(0,230))
box()
axis(2, at=b, labels=EcoComp$Ecosystem.component_level1 , las=1, cex.axis=1.2)
axis(1, at= seq(0,220, 20), labels=seq(0, 220, 20), cex.axis=1.2)
title(main="Number of unique papers per ecosystem component (level 1)", cex.main=1.5, font.main=2)
text(x=160, y=0, paste0("Total number of papers retained: ", length(unique(subset_byc$SW.ID))))
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
ggsave("EcoRegion.png", p, path=outPath, width = 8, height = 5)



#-----------------------------------------------
## Barplot for ecosystem component (level 1)
#-----------------------------------------------
EcoMethod                               <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
                                                    by = Sampling.Method.used.for.data.collection]
EcoMethod                              <- EcoMethod[order(NrPaps),,]

tiff(paste0(outPath, "EcoMethod.png"), width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(EcoMethod$NrPaps, horiz=TRUE, axes=F, xlim=c(0,230))
box()
axis(2, at=b, labels=EcoMethod$Sampling.Method.used.for.data.collection, las=1, cex.axis=1.2)
axis(1, at= seq(0,220, 20), labels=seq(0, 220, 20), cex.axis=1.2)
title(main="Number of unique papers per sampling method", cex.main=1.5, font.main=2)
text(x=160, y=0, paste0("Total number of papers retained: ", length(unique(subset_byc$SW.ID))))
text(x=EcoComp$NrPaps+6, y=b, EcoComp$NrPaps)
axis(1, at=110, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

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

ggsave("EcoPress_heatmap.png", p, path=outPath, width = 9, height = 7)


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
ggsave("QualityMethods.png", p, path=outPath, width = 8, height = 5)




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





#-----------------------------------------------#
## Spatial Scales ---- code from Elliot
#-----------------------------------------------#

# is.na - spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Make spatial extent and scale categories
subset_byc$ScaleSpatial <- factor(x = subset_byc$Scale...Spatial..m.,
                              levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000","10,000-50,000", "50,000-100,000", ">100,000"),
                              ordered = TRUE)
subset_byc$ResSpatial <- factor(x = subset_byc$Resolution...Spatial..m.,
                            levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000","10,000-50,000", "50,000-100,000", ">100,000"),
                            ordered = TRUE)

spatResEx_cat <- expand.grid(levels(subset_byc$ScaleSpatial),
                             levels(subset_byc$ScaleSpatial))

colnames(spatResEx_cat) <- c("SpatialExtent_m", "SpatialRes_m")

## Make counts of articles in different combinations of SPATIAL EXTENTS & RESOLUTIONS
spatResEx_count <- aggregate(SW.ID~ScaleSpatial+ResSpatial,
                             data = subset_byc[!duplicated(subset_byc$SW.ID), ],
                             FUN = length)

names(spatResEx_count)[names(spatResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatResEx_count <- merge(x = spatResEx_cat,
                         y = spatResEx_count,
                         by.y = c("ScaleSpatial", "ResSpatial"),
                         by.x = c("SpatialExtent_m", "SpatialRes_m"),
                         all.x = TRUE)

spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste(outPath, "subset_byc_spatialResVExt.png"),
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
subset_byc$ScaleTemporal <- factor(x = subset_byc$Scale...Temporal,
                                   #levels = c("day", "half year", "year", "two year", "five year",  "decade", "multidecadal"),
                                   levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                               ordered = TRUE)
subset_byc$ResTemporal <- factor(x = subset_byc$Resolution...Temporal,
                                 #levels = c("day", "half year", "year", "two year", "five year",  "decade", "multidecadal"),
                                 levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                                 ordered = TRUE)

tempResEx_cat <- expand.grid(levels(subset_byc$ScaleTemporal),
                             levels(subset_byc$ScaleTemporal))

colnames(tempResEx_cat) <- c("TemporalExtent", "TemporalRes")

## Make counts of articles in different combinations of TEMPORAL EXTENTS & RESOLUTIONS
tempResEx_count <- aggregate(SW.ID~ScaleTemporal+ResTemporal,
                             data = subset_byc[!duplicated(subset_byc$SW.ID), ],
                             FUN = length)

names(tempResEx_count)[names(tempResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


tempResEx_count <- merge(x = tempResEx_cat,
                         y = tempResEx_count,
                         by.y = c("ScaleTemporal", "ResTemporal"),
                         by.x = c("TemporalExtent", "TemporalRes"),
                         all.x = TRUE)

tempResEx_count[is.na(tempResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste(outPath, "subset_byc_temporalResVExt.png"),
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
spatempResEx_cat <- expand.grid(levels(subset_byc$ScaleSpatial),
                                levels(subset_byc$ScaleTemporal))

colnames(spatempResEx_cat) <- c("SpatialScale_m", "TemporalScale")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL EXTENTS
spatempEx_count <- aggregate(SW.ID~ScaleSpatial+ScaleTemporal,
                             data = subset_byc[!duplicated(subset_byc$SW.ID), ],
                             FUN = length)
names(spatempEx_count)[names(spatempEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempEx_count <- merge(x = spatempResEx_cat,
                         y = spatempEx_count,
                         by.y = c("ScaleSpatial", "ScaleTemporal"),
                         by.x = c("SpatialScale_m", "TemporalScale"),
                         all.x = TRUE)

spatempEx_count[is.na(spatempEx_count$NumberOfArticles), "NumberOfArticles"] <- 0


## Plot
ggsave(filename = paste(outPath, "subset_byc_spatiotemporalExt.png"),
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
## SpatioTemporal Resolution ---- code from Elliot
#-----------------------------------------------#
## Dependent on code above

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = subset_byc[!duplicated(subset_byc$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempResEx_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialScale_m", "TemporalScale"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste(outPath, "subset_byc_spatiotemporalRes.png"),
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
unique(subset_byc$Response.variable_paper)
unique(subset_byc$Response.variable_category)
table(subset_byc$Response.variable_category)
table(subset_byc$Pressure.variable_category)

#-----------------------------------------------#
## Responses & Ecosystem Components ----
#-----------------------------------------------#
unique(subset_byc$Ecosystem.component_level1)
table(subset_byc$Ecosystem.component_level1)
unique(subset_byc$Ecosystem.component_level2)
table(subset_byc$Ecosystem.component_level2)

#===-
### Create sankey diagram ----
#====-
## Build sankey
subset_bycLinkage <- ggplot(subset_bycLinkageInput,
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

ggsave(plot = subset_bycLinkage,
       filename = paste0(outPath, "subset_bycLinkage.png"),
       device = "png",
       dpi = 300,
       width = 170,
       height = 90,
       units = "mm")



