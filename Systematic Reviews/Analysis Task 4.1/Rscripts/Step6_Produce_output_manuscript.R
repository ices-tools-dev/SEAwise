#####################################################################################################################-
#####################################################################################################################-
#
#     Script to explore and produce output for the manuscript.
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
par(mar=c(5,5,2,2))
plot(x=c(min(YearRet$Year), max(YearRet$Year)), y=c(0, max(YearRet$NrPaps)), type="n", ann=F, axes=F)
lines(x=as.numeric(colnames(YearRet2)), y=YearRet2["Retained",], type="o", pch=16, lwd=2, cex=1.5)
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=2, "Publication year")
axis(2, at=(max(YearRet$NrPaps)/2), tick=F, line=2, cex.axis=2, labels="Number of papers")
dev.off()


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

tiff(paste0(outPath, "GearsStudiedCommRecr.tiff"), width= 1500, height = 1000, res = 100)
par(mfrow=c(1,2))
par(mar=c(18,8,7,1))
for(iType in seq_along(GearLevels)){
  subdat                             <- Gears[Gears$Fishery.type == GearLevels[iType],]
  subdat                             <- subdat[order(NrPaps, decreasing=T)]
  b                                  <- barplot(subdat$NrPaps, axes=F, ylim=c(0, max(subdat$NrPaps*1.05)), col=viridis(3)[c(2,3)][iType])
  title(GearLevels[iType], cex.main=3, font=2, line=3)
  title(paste0("(n = ", sum(subdat$NrPaps), ")"), font.main=3, cex.main=2, line=1.5)
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


#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Regional differences ----
#-----------------------------------------------#

#-----------------------------------------------#
## Barplot with number of papers by region -----
#-----------------------------------------------#

Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
Regions                               <- Regions[, .(NrPaps = sum(NrPaps)), by="Region"]
Regions                               <- Regions[order(NrPaps, decreasing = F),,]

sum(Regions$NrPaps) #558, suggesting that, out of 549 papers, several of them studied multiple regions


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
  theme(legend.position = c(0.87,0.63),
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
sankeyDat$Gear_level1                <- with(sankeyDat, ifelse(is.na(Gear_level1),"Unknown",Gear_level1))
sankeyDat                            <- sankeyDat[!duplicated(sankeyDat),]

sankeyDat$Fishery.type               <- factor(sankeyDat$Fishery.type, levels = c("Unknown","Scientific","Recreational","Commercial"))

sankeyDat$Response.variable_category <- with(sankeyDat, ifelse(Response.variable_category %in% "Survival","Mortality",Response.variable_category))  

sankeyInput                          <- make_long(sankeyDat, Fishery.type,Gear_level1,Pressure.type,Ecosystem.component_level1,Response.variable_category)


#===-
# Create sankey diagram
#====-
## Set colors
colorpal                             <- viridis(8)[-c(1,2,3)]

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
  geom_sankey_label(size=7) +
  theme_few()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(size=20, colour = "black"),
        legend.position = "none")

## View Sankey
sankey

## Save sankey
ggsave("Sankey5.tiff", sankey, path=outPath,
       width = 400,
       height = 200,
       units = "mm")
x



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Map with regions ----
#-----------------------------------------------#

-----------------------------------------------#
## Creating shapefile with Regions ----
-----------------------------------------------#
## Load ICES regions as downloaded from https://gis.ices.dk/sf/ and mediterranean GSA's from https://www.fao.org/gfcm/data/maps/gsas/en/
ICESareas                            <- st_read(paste0(GISpath, "ICES_Areas_20160601_cut_dense_3857.shp"))
ICESareas                            <- st_transform(ICESareas, crs=3035)
ICESEcors                            <- st_read(paste0(GISpath, "ICES_ecoregions_20171207_erase_ESRI.shp"))
ICESEcors                            <- st_transform(ICESEcors, crs=3035)

ICESEcors$Region                     <- ifelse(ICESEcors$Ecoregion %in% c("Western Mediterranean Sea", "Adriatic Sea",
                                                                   "Ionian Sea and the Central Mediterranean Sea",
                                                                   "Aegean-Levantine Sea"), "Mediterranean Sea",
                                               ifelse(ICESEcors$Ecoregion == "Greater North Sea", "North Sea",
                                                      ifelse(ICESEcors$Ecoregion %in% c("Black Sea", "Norwegian Sea","Barents Sea","Baltic Sea"), ICESEcors$Ecoregion,
                                                             ifelse(ICESEcors$Ecoregion %in% c("Azores", "Oceanic Northeast Atlantic", "Greenland Sea",
                                                                                                      "Icelandic Waters", "Faroes", "Celtic Seas"), "NE-Atlantic", NA))))

ICESareas$Region                     <- ifelse(ICESareas$Area_Full %in% c("27.7.a", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.8.a","27.8.b","27.8.c", "27.8.d.2"), "Western Waters",
                                               ifelse(ICESareas$Area_Full %in% c("27.9.a", "27.9.b.1", "27.9.b.2", "27.8.e.1", "27.8.e.2", "27.6.a", "27.7.c.2",
                                                                                 "27.6.b.2", "27.7.b", "27.7.k.2", "27.7.j.2", "27.7.b", "27.7.j.1"), "NE-Atlantic", NA))

Regions                              <- rbind(subset(ICESEcors[,c("Region", "geometry")], is.na(Region)==F), subset(ICESareas[,c("Region", "geometry")], is.na(Region)==F))

# Select regions that already consist of one area
Regs                                 <- subset(Regions, Region %in% c("Baltic Sea","Barents Sea", "Black Sea", "North Sea", "Norwegian Sea"))

# Combine subregions into on region
for (iReg in c("Mediterranean Sea", "Western Waters", "NE-Atlantic")){
  subdat                             <- subset(Regions, Region == iReg)
  a                                  <- st_sf(st_union(subdat))
  b                                  <- data.frame(Region = iReg)
  b                                  <- st_set_geometry(b, st_geometry(a))
  Regs                               <- rbind(Regs, b)
} # end iReg loop

## Fix some overlaps that should not be there
## NE-Atlantic
WW                                   <- st_difference(subset(Regs, Region == "NE-Atlantic"), subset(Regs, Region == "Western Waters"))
WW$Region.1                          <- NULL

## Update the fixes
Regs2                                <- subset(Regs, !Region %in% "NE-Atlantic")
Regs2                                <- rbind(Regs2, WW)

RegionalSeas                         <- Regs2
st_write(RegionalSeas, paste0(GISpath, "RegionalSeas.shp"))
save(RegionalSeas, file=paste0(GISpath, "RegionalSeas.Rdata"))


#-----------------------------------------------#
## Creating map with Region specific info ----
#-----------------------------------------------#
Regions                              <- data[, .(NrPaps = length(unique(SW.ID))), 
                                              by = Region]
Regions                              <- Regions[order(NrPaps),,]
RegCol                               <- data.frame(colcode = viridis(max(Regions$NrPaps)),
                                                   value = c(1:max(Regions$NrPaps)))
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
text(x=1E6, y=1.18e6, max(Regions$NrPaps))
text(x=0.5e6, y=1.72e6, "Number of papers", font=4, cex=1.2)
dev.off()



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
