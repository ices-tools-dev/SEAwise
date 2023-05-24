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
data                                  <- readRDS(file=paste0(datPath, "data.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened.rds"))


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

l


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
# Subset to discarding----
#-----------------------------------------------#

data_allScreened[data_allScreened == ""] <- NA 

#Pressure.type = 'discarding' or Catch_and_bycatch
#subset_byc<-data_allScreened[(data_allScreened$Pressure.type%in%c("Catch_and_bycatch","Discarding") & data_allScreened$Pressure_level%in%c("Bycatch", "Non-target", "Not specified", "NA")),]

subset_byc<-data_allScreened[(data_allScreened$Pressure.type%in%c("Catch_and_bycatch")& data_allScreened$Pressure_level%in%c("Bycatch", "Non-target", "Not specified")),]

subset_dis<-data_allScreened[(data_allScreened$Pressure.type%in%c("Discarding")),]

intersect(names(subset_byc),names(subset_dis))

combined_subset<-rbind(subset_byc, subset_dis)


#-----------------------------------------------
## Barplot for ecosystem component (level 1)
#-----------------------------------------------
EcoComp                               <- combined_subset[, .(NrPaps = length(unique(SW.ID))),
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

EcoComp                               <- combined_subset[, .(NrPaps = length(unique(SW.ID))),
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
## Heatmap of ecosystem component level 1 vs pressure type level 1
#-----------------------------------------------

EcoPress                             <- subset_byc[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Pressure_level")]
EcoPressmat                          <- matrix(nrow = length(unique(subset_byc$Ecosystem.component_level1)),
                                               ncol = length(unique(subset_byc$Pressure_level)))
colnames(EcoPressmat)                <- sort(unique(subset_byc$Pressure_level))  
rownames(EcoPressmat)                <- sort(unique(subset_byc$Ecosystem.component_level1))

for(iRow in c(1:nrow(EcoPress))){
  subdat                             <- EcoPress[iRow,]
  EcoPressmat[subset_byc$Ecosystem.component_level1, subset_byc$Pressure_level] <- subset_byc$NrPaps
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
axis(1, at=seq(1/14, 13/14, length.out=7), labels= c("Bycatch", "Non-target", "Not specified"), las=3, cex.axis=1.5)
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
EcoFish                              <- combined_subset[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type")]
EcoFishmat                           <- matrix(nrow = length(unique(combined_subset$Ecosystem.component_level1)),
                                               ncol = length(unique(combined_subset$Fishery.type)))
colnames(EcoFishmat)                 <- sort(unique(combined_subset$Fishery.type))  
rownames(EcoFishmat)                 <- sort(unique(combined_subset$Ecosystem.component_level1))

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
