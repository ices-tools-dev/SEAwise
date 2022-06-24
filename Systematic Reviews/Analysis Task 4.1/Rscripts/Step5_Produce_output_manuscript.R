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
library(splitstackshape)
library(RColorBrewer)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"


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
# Regional differences ----
#-----------------------------------------------#

#-----------------------------------------------#
## Barplot with number of papers by region -----
#-----------------------------------------------#

Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), by = "Region"]
Regions                               <- Regions[, .(NrPaps = sum(NrPaps)), by="Region"]
Regions                               <- Regions[order(NrPaps, decreasing = F),,]

sum(Regions$NrPaps) #566, suggesting that, out of 549 papers, several of them studied multiple regions


tiff(paste0(outPath, "Regions.tiff"), width=1000, height=750, res=100)
par(mar=c(5, 12, 4, 2))
b                                     <- barplot(Regions$NrPaps, horiz=TRUE, axes=F, xlim=c(0,max(Regions$NrPaps+20)))
box()
axis(2, at=b, labels=Regions$Region, las=1, cex.axis=1.2)
axis(1, at= seq(0,max(Regions$NrPaps), 25), labels=seq(0, max(Regions$NrPaps), 25), cex.axis=1.2)
# title(main="Number of papers per region", cex.main=1.5, font.main=2)
text(x=180, y=0, paste0("Total number of unique papers ", length(unique(data$SW.ID))), pos=4)
text(x=Regions$NrPaps + 8, y=b, Regions$NrPaps)
axis(1, at=max(Regions$NrPaps)/2, tick=F, line=2, label="Number of papers", cex.axis=1.3)
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
# Sankey diagrma ----
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

sankeyInput                          <- make_long(sankeyDat, Fishery.type,Pressure.type,Ecosystem.component_level1,Response.variable_category)


#===-
# Create sankey diagram
#====-
## Set colors
colorpal                             <- brewer.pal(8,"Paired")[-c(2,4,6,8)]
mycolors                             <- c(colorpal[1],4), rep(colorpal[3],7), rep(colorpal[5],11), rep(colorpal[7],13))

## Build sankey
sankey <- ggplot(sankeyInput,
                 mapping = aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(x),
                               label = node)) +
  scale_x_discrete(labels=c("Fishery","Pressure","Ecosystem component","Aspect")) +
  scale_fill_manual(values=colorpal) +
  geom_sankey(flow.fill="grey",
              flow.alpha=0.8) +
  geom_sankey_label(size=7) +
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



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Get citations by paper ----
#-----------------------------------------------#

load("Systematic Reviews/Analysis Task 4.1/Search/search results_citedBy.RData")
