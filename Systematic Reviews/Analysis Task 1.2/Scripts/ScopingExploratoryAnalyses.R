#
##
### This script explores the various databases from the SEAwise reviews
### To make these results comparable to scoping, they are organised according to FAO EAFM categories.
### Analyses by Elliot J. Brown 
##
#
rm(list = ls())

#===
# Libraries and Dependencies ----
#===
library(DT)
library(bslib)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyquant)
library(lubridate)
library(reshape2)
#====

#===
# Look and Feel ----
#===
# swCols <- c("#210384", "#037184", "#00B292", "#00B262", "#86C64E", "#C6E83E", "#032980", "#00A224", "#C1D654", "#F2E541")
swCols <- c("#210384", "#037184", "#00B292", "#00B262", "#00A224", "#86C64E", "#C1D654", "#C6E83E", "#F2E541", "#FF9867", "#F27A41")
csCols <- c("Baltic Sea" = "#037184",
            "North Sea" = "#00B292",
            "Western Waters" = "#00B262",
            "Mediterranean Sea" = "#86C64E")
scale_fill_csCols <- function(...){
  ggplot2:::manual_scale(
    'fill',
    values = csCols,
    ...
  )
}

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

theme_eb <- theme(axis.title = element_text(size = 9),
                  axis.text = element_text(size = 7),
                  legend.text = element_text(size = 7),
                  legend.title = element_text(size = 9))
#====

#===
# Data and Cleaning ----
#===
wp2 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_2_20220829.csv", header = T) # Path when run manually
# wp2 <- read.csv(file = "../../Databases/Database_2_20220829.csv", header = T)
wp2$Person <- NULL
wp2$Exclusion.Criteria <- NULL
wp2$CS...non.CS <- NULL
wp2$Region.CS.and.non.CS <- NULL
wp2 <- wp2[wp2$SW.ID != "", ]

wp3 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_3_20220829.csv", header = T) # Path when run manually
# wp3 <- read.csv(file = "../../Databases/Database_3_20220829.csv", header = T)
wp3$X <- NULL
wp3$Exclusion.Criteria <- NULL
wp3[wp3$Environmental.Drivers.== "Other", "Environmental.Drivers."] <- ifelse(grepl(pattern = c("turbulence|mix|upwelling|wind speed|stratification|salinity, oxygen|thermocline"),
                                                                                    x = wp3[wp3$Environmental.Drivers.== "Other", "Additional.info.about.response"],
                                                                                    ignore.case = T), "Hydrodynamics & wind", "Other")
wp3[wp3$Environmental.Drivers.== "Other", "Environmental.Drivers."] <- ifelse(grepl(pattern = c("precipitation|NAO|NOA"),
                                                                                    x = wp3[wp3$Environmental.Drivers.== "Other", "Additional.info.about.response"],
                                                                                    ignore.case = T), "Climate", "Other")
wp3[wp3$Environmental.Drivers. %in% c("Hydrodynamics & winds", "Climate"), "Driverscategory"] <- "CLIMATE & OCEANOGRAPHY"

wp3[wp3$Environmental.Drivers.== "Other", "Environmental.Drivers."] <- ifelse(grepl(pattern = c("General Productivity|primary production"),
                                                                                    x = wp3[wp3$Environmental.Drivers.== "Other", "Additional.info.about.response"],
                                                                                    ignore.case = T), "Prey availability", "Other")
wp3[wp3$Environmental.Drivers.== "Other", "Environmental.Drivers."] <- ifelse(grepl(pattern = c("grey seal"),
                                                                                    x = wp3[wp3$Environmental.Drivers.== "Other", "Additional.info.about.response"],
                                                                                    ignore.case = T), "Predator pressure", "Other")
wp3[wp3$Environmental.Drivers. %in% c("Prey availability", "Predator pressure"), "Driverscategory"] <- "BIOTIC"


wp4 <- readRDS(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_4_20230131.rds") # Path when run manually
# wp4 <- readRDS(file = "../../Databases/Database_4_20230131.rds")
wp4 <- wp4[is.na(wp4$Exclusion.Criteria),]
wp4$Exclusion.Criteria <- NULL
wp4$Sampling.Method.used.for.data.collection <- ifelse(wp4$Sampling.Method.used.for.data.collection == "other", wp4$Description.Other.Sampling.Method, wp4$Sampling.Method.used.for.data.collection)
wp4$Description.Other.Sampling.Method <- NULL
wp4$Study.type <- ifelse(is.na(wp4$Study.type), "Other", wp4$Study.type)
wp4$Study.type <- ifelse(wp4$Study.type == "combination of field surveys, byctach and over many decades", "Other", wp4$Study.type)
wp4$Study.type <- ifelse(wp4$Study.type == "Modelling/simulation", "Modelling or simulation", wp4$Study.type)


# wp4$Pressure_level <- ifelse(is.na(wp4$Pressure_level), "Not specified", wp4$Pressure_level)


wp5 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_5_20220829.csv", header = T) # Path when run manually
# wp5 <- read.csv(file = "../../Databases/Database_5_20220829.csv", header = T)
wp5$Name <- NULL
wp5 <- wp5[wp5$Exclusion.Criteria == "K",]
wp5$Exclusion.Criteria <- NULL
wp5$Sampling.Method.used.for.data.collection <- ifelse(wp5$Sampling.Method.used.for.data.collection == "Regular & Irregular Fisheries Independent Survey", "Regular Fisheries Independent Survey _ Irregular Fisheries Independent Survey", wp5$Sampling.Method.used.for.data.collection)
wp5$species <- gsub(pattern = "crustaceans", replacement = "Crustaceans", wp5$species)
wp5$species <- gsub(pattern = "various", replacement = "Various", wp5$species)
wp5$species <- gsub(pattern = "Various", replacement = "Species assemblage", wp5$species)
wp5$species <- gsub(pattern = "Scombrus scombrus", replacement = "Scomber scombrus", wp5$species)
wp5$species <- gsub(pattern = "Merlangius merlangius", replacement = "Merlangius merlangus", wp5$species)
wp5$species <- gsub(pattern = "Melanogarmmus aeglefinus", replacement = "Melanogrammus aeglefinus", wp5$species)
wp5$species <- gsub(pattern = "Pelagic species", replacement = "Pelagic assemblage", wp5$species)
wp5$species <- gsub(pattern = "small pelagics", replacement = "Pelagic assemblage", wp5$species)
wp5$species <- gsub(pattern = "Trachurus trachurus", replacement = "Trachurus spp", wp5$species)
wp5$species <- gsub(pattern = "elasmobranch", replacement = "Elasmobranch", wp5$species)
wp5$species <- gsub(pattern = "Flatfish species", replacement = "Species assemblage", wp5$species)
wp5$species <- gsub(pattern = "Lophius budegassa", replacement = "Lophius spp", wp5$species)
wp5$species <- gsub(pattern = "Lophius", replacement = "Lophius spp", wp5$species)
wp5$life.stage <- gsub("juv and adults", "juveniles and adults", wp5$life.stage)
wp5$life.stage <- gsub("Juveniles", "juveniles", wp5$life.stage)
wp5$life.stage <- gsub("all", "full lifecycle", wp5$life.stage)

wp6 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_6_20221207.csv", header = T) # Path when run manually
wp6 <- wp6[wp6$Exclusion.Criteria == "", ]
wp6 <- wp6[wp6$Continent == "Europe", ]
wp6[wp6$SW.ID == "SW6_0600", "Management.Measures.considered..drop.down.list."] <- "Output control "

wp2$WP <- "Socio-economic"
wp3$WP <- "Ecosystem\non Fisheries"
wp4$WP <- "Fisheries\non Ecosystem"
wp5$WP <- "Spatial\nManagement"
wp6$WP <- "Models\nSupporting EBFM"

## Sanitise regions
regionlist <- c("Baltic\nSea" = "baltic",
                "North\nSea" = "north Sea",
                "Western\nWaters" = "western Waters",
                "Mediterranean\nSea" = "mediterranean",
                "Other\nEuropean" = "Norwegian Sea|Barents Sea|Black Sea|European Macaronesia",
                "Global" = "Global|NE-Atlantic")

regionlist2 <- c("Baltic\nSea" = "Finland|Baltic|Baltic Proper|western Baltic|Kattegat|Skagerrak|ICES Area 3|ICES Area III|ICES subarea 3|ICES division 3|ICES subdivision 3|ICES subarea III|ICES division III|ICES subdivision III",
                 "North\nSea" = "North|English Channel|ICES Area 4|ICES Area IV|ICES Area 7.d|ICES Area VIId|ICES subarea 4|ICES division 4|ICES subdivision 4|ICES subarea IV|ICES division IV|ICES subdivision IV|ICES subarea 7.d|ICES division 7.d|ICES subdivision 7.d ICES subarea VIId|ICES division VIId|ICES subdivision VIId",
                 "Western\nWaters" = "Galicia|Cadiz|Portugal|Portuguese|Rias Baixas|Cantabrian|Saint-Brieuc|St Brueic|Galway|Iberian|Ibero|Celtic|Irish|Biscay|Western waters|North East Atlantic|English Channel|ICES Area 6|ICES 6|ICES Area 7|ICES Area 8|ICES subarea 6|ICES subarea 7|ICES subarea 8|ICES division 6|ICES division 7|ICES division 8|ICES subdivision 6|ICES subdivision 7|ICES subdivision 8|ICES Area VI|ICES Area VII|ICES Area VIII|ICES subarea VI|ICES subarea VII|ICES subarea VIII|ICES division VI|ICES division VII|ICES division VIII|ICES subdivision VI|ICES subdivision VII|ICES subdivision VIII",
                 "Mediterranean\nSea" = "Balearic|Corsica|Mallorca|Agean|Aegean|sicily|Catalan|Sacca|Mediterranen|Mediteranean|Ionian|Adriatic|GSA6|GSA17|GSA18|GSA19|GSA20|GSA22")

regionlist2 <- c("Baltic\nSea" = "Baltic|Baltic Proper|western Baltic|Kattegat|Skagerrak|ICES Area 3|ICES Area III|ICES subarea 3|ICES division 3|ICES subdivision 3|ICES subarea III|ICES division III|ICES subdivision III",
                 "North\nSea" = "North|English Channel|ICES Area 4|ICES Area IV|ICES Area 7.d|ICES Area VIId|ICES subarea 4|ICES division 4|ICES subdivision 4|ICES subarea IV|ICES division IV|ICES subdivision IV|ICES subarea 7.d|ICES division 7.d|ICES subdivision 7.d ICES subarea VIId|ICES division VIId|ICES subdivision VIId",
                 "Western\nWaters" = "Celtic|Irish|Biscay|Western waters|North East Atlantic|English Channel|ICES Area 6|ICES Area 7|ICES Area 8|ICES subarea 6|ICES subarea 7|ICES subarea 8|ICES division 6|ICES division 7|ICES division 8|ICES subdivision 6|ICES subdivision 7|ICES subdivision 8|ICES Area VI|ICES Area VII|ICES Area VIII|ICES subarea VI|ICES subarea VII|ICES subarea VIII|ICES division VI|ICES division VII|ICES division VIII|ICES subdivision VI|ICES subdivision VII|ICES subdivision VIII",
                 "Mediterranean\nSea" = "Mediterranean|Ionian|Adriatic|GSA17|GSA18|GSA19|GSA20|GSA22")


wp2$Regions <- "NA"
for(i in 1:length(regionlist)){
  wp2$Regions <- ifelse(grepl(pattern = unname(regionlist[i]),
                              x = wp2$Region,
                              ignore.case = TRUE),
                        names(regionlist[i]),
                        no = wp2$Regions)
}
wp2[wp2$Regions == "", "Regions"] <- "Other\nEuropean"
wp2[wp2$Region == "", "Regions"] <- "Other\nEuropean"
wp3$Regions <- "NA"
for(i in 1:length(regionlist)){
  wp3$Regions <- ifelse(grepl(pattern = unname(regionlist[i]),
                              x = wp3$Region,
                              ignore.case = TRUE),
                        names(regionlist[i]),
                        no = wp3$Regions)
}
wp4$Regions <- "NA"
for(i in 1:length(regionlist)){
  wp4$Regions <- ifelse(grepl(pattern = unname(regionlist[i]),
                              x = wp4$Region,
                              ignore.case = TRUE),
                        names(regionlist[i]),
                        no = wp4$Regions)
}
wp5$Regions <- "NA"
for(i in 1:length(regionlist)){
  wp5$Regions <- ifelse(grepl(pattern = unname(regionlist[i]),
                              x = wp5$Region,
                              ignore.case = TRUE),
                        names(regionlist[i]),
                        no = wp5$Regions)
}


wp6$Regions <- "NA"
for(i in 1:length(regionlist2)){
  wp6$Regions <- ifelse(grepl(pattern = unname(regionlist2[i]),
                              x = wp6$Region..free.text.,
                              ignore.case = TRUE),
                        names(regionlist2[i]),
                        no = wp6$Regions)
}
wp6[wp6$Regions == "NA", "Regions"] <- "Other\nEuropean"


allRev <- list(
  "Socio-economic Interactions with Fishing" = wp2,
  "Ecological Effects on Fishing" = wp3,
  "Ecological Effects of Fishing" = wp4,
  "Spatial Management" = wp5,
  "Management Evaluation" = wp6
)
#====

#===
# Import Scoping Data ----
#===
scopingGeneral <- read.csv(file = "Systematic Reviews/Analysis D1.5/scoping_figures_data.csv", header = T)
scopingGeneral[scopingGeneral$FAO_Category == "scientific basis", "FAO_Category"] <- "Scientific Basis"
scopingGeneral[scopingGeneral$BroadCategory == "Human wel-being", "BroadCategory"] <- "Human well-being"
scopingGeneral$FAO_Category <- capFirst(scopingGeneral$FAO_Category)

scopingLong <- melt(data = scopingGeneral,
                    id.vars = c("BroadCategory", "FAO_Category"),
                    measure.vars = c("BSAC", "BS.scientists", "NSAC", "PELAC", "NS.scientists", "NWWAC", "SWWAC", "WW.scientists", "MEDAC.national", "Med.scientists"),
                    variable.name = "Scope_Group",
                    value.name = "Occurrence")
#====

#===
# Import Governance Data ----
indicators <- read.csv(file = "Systematic Reviews/Analysis D1.5/Indicators_figures_data.csv", header = T)
colnames(indicators) <- c("FAO_Category", "ScopingAllRegionsStandardised", "All Regions\nIndicators", "Baltic Sea\nIndicators", "Baltic Sea\nThresholds", "North Sea\nIndicators", "North Sea\nThresholds", "Western Waters\nIndicators", "Western Waters\nThresholds", "Mediterranean\nIndicators", "Mediterranean\nThresholds", "All Regions\nThresholds")
indicators <- indicators[, c(1:3,length(colnames(indicators)),4:(length(colnames(indicators))-1))]

indicatorLong <- melt(data = indicators,
                      id.vars = "FAO_Category",
                      measure.vars = colnames(indicators)[2:length(colnames(indicators))],
                      variable.name = "Region",
                      value.name = "Occurrence")
#====

#===
# Generate Scoping results figures ----
#===
## Broad Scoping -----
tempdf <- scopingLong[scopingLong$BroadCategory == "Broad" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

broad_scoping <- aggregate(Occurrence~FAO_Category, tempdf[!tempdf$FAO_Category %in% c("Fisheries", "Scientific Basis"),], FUN = "sum")
broad_scoping$OccurPer <- (broad_scoping$Occurrence/sum(broad_scoping$Occurrence))*100

pl_broad <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))])

pl_pc_broad <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]))

pl_pc_broad

ggsave(pl_pc_broad,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_broad.png",
       width = 180,
       height = 90,
       units = "mm")

## Ecological well-being -----
tempdf <- scopingLong[scopingLong$BroadCategory == "Ecological well-being" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_ecoWB <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in\nEcological Well-Being")

pl_pc_ecoWB <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in\nEcological Well-Being")

pl_pc_ecoWB

ggsave(pl_pc_ecoWB,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_ecoWB.png",
       width = 180,
       height = 90,
       units = "mm")

## Fisheries -----
tempdf <- scopingLong[scopingLong$BroadCategory == "Fisheries" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_fisheries <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in Fisheries")

pl_pc_fisheries <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in Fisheries")

pl_pc_fisheries

ggsave(pl_pc_fisheries,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_fisheries.png",
       width = 180,
       height = 90,
       units = "mm")
## Human Well Being -----
tempdf <- scopingLong[scopingLong$BroadCategory == "Human well-being" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_humWB <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in\nHuman Well-Being")

pl_pc_humWB <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in\nHuman Well-Being")

pl_pc_humWB

ggsave(pl_pc_humWB,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_humWB.png",
       width = 180,
       height = 90,
       units = "mm")
## Ability to achieve: External ecological drivers -----
tempdf <- scopingLong[scopingLong$BroadCategory == "External ecological drivers" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_exEcoDr <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in External\nEcological Drivers")

pl_pc_exEcoDr <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in External\nEcological Drivers")

pl_pc_exEcoDr

ggsave(pl_pc_exEcoDr,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_exEcoDr.png",
       width = 180,
       height = 90,
       units = "mm")
## Ability to Achieve: Human drivers -----
tempdf <- scopingLong[scopingLong$BroadCategory == "External human drivers" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_exHumDr <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in External\nHuman Drivers")

pl_pc_exHumDr <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in External\nHuman Drivers")

pl_pc_exHumDr

ggsave(pl_pc_exHumDr,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_exHumDr.png",
       width = 180,
       height = 90,
       units = "mm")

### Topics in Management and Governance ------
tempdf <- scopingLong[scopingLong$BroadCategory == "Fisheries management" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_fishManGov <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Topics in Fisheries\nManagement and Governance")

pl_pc_fishManGov <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Topics in Fisheries\nManagement and Governance")

pl_pc_fishManGov

ggsave(pl_pc_fishManGov,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_fishManGov.png",
       width = 180,
       height = 90,
       units = "mm")
### Topics in landbased and non-fishing maritime activities ------
tempdf <- scopingLong[scopingLong$BroadCategory == "Non-fisheries management" & scopingLong$FAO_Category != "Total",]

tl <- split(tempdf, f = tempdf$Scope_Group)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

pl_nFishManGov <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Scoping Groups") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = swCols[1:length(unique(tempdf$FAO_Category))],
                    name = "Land-based and Non-fisheries\nManagement and Governance")

pl_pc_nFishManGov <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Scope_Group,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Scoping Groups") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        # legend.title = element_text(angle = -90, hjust = 0.5, vjust = 0.5, size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  # guides(fill = guide_legend(title.position="right", title.vjust = 0.5, title.hjust = 0.5))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]),
                    name = "Land-based and Non-fisheries\nManagement and Governance")


pl_pc_nFishManGov

ggsave(pl_pc_nFishManGov,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_nFishManGov.png",
       width = 180,
       height = 90,
       units = "mm")
#====

#===
# Indicator results figures ----
tempdf <- indicatorLong[!indicatorLong$Region %in% c("ScopingAllRegionsStandardised", "All Regions\nThresholds"), ]

tl <- split(tempdf, f = tempdf$Region)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf <- do.call(rbind, tl)

odf <- aggregate(OccurPer ~ FAO_Category, tempdf, FUN = "sum")
odf$FAO_Category <- factor(odf$FAO_Category, levels = odf[order(odf$OccurPer), "FAO_Category"])
tempdf$FAO_Category <- factor(tempdf$FAO_Category, levels = levels(odf$FAO_Category))

broad_indicator <- aggregate(Occurrence~FAO_Category, tempdf[!tempdf$FAO_Category %in% c("Fisheries", "scientific basis"),], FUN = "sum")
broad_indicator$OccurPer <- (broad_indicator$Occurrence/sum(broad_indicator$Occurrence))*100

tempdf <- tempdf[!is.na(tempdf$Occurrence), ]
tempdf <- tempdf[tempdf$Occurrence != 0, ]

pl_indicators <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Region,
                         y = Occurrence,
                         fill = FAO_Category)) +
  xlab("Regions") +
  ylab("Number of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]))

pl_indicators

pl_pc_indicators <- ggplot(tempdf) +
  geom_col(mapping = aes(x = Region,
                         y = OccurPer,
                         fill = FAO_Category),
           width = 0.75) +
  xlab("Regions") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf$FAO_Category))]))

pl_pc_indicators

ggsave(pl_indicators,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_indicators.png",
       width = 180,
       height = 90,
       units = "mm")

ggsave(pl_pc_indicators,
       filename = "Systematic Reviews/Analysis D1.5/Figures/pl_pc_indicators.png",
       width = 180,
       height = 90,
       units = "mm")
#====

#===
# Literature Review Recoding ----
#===
#===
## Add FAO Categories as binary variables set to 0 ----
#===
fao_2 <- data.frame(ScientificBasis = integer(length = nrow(wp2)),
                    Governance = integer(length = nrow(wp2)),
                    ExternalHumanDrivers = integer(length = nrow(wp2)),
                    ExternalEcologicalDrivers = integer(length = nrow(wp2)),
                    RetainedSpecies = integer(length = nrow(wp2)),
                    GeneralEcosystem = integer(length = nrow(wp2)),
                    Fisheries = integer(length = nrow(wp2)),
                    HumanWellBeing = integer(length = nrow(wp2)),
                    Livelihood = integer(length = nrow(wp2))
                    )

fao_3 <- data.frame(ScientificBasis = integer(length = nrow(wp3)),
                    Governance = integer(length = nrow(wp3)),
                    ExternalHumanDrivers = integer(length = nrow(wp3)),
                    ExternalEcologicalDrivers = integer(length = nrow(wp3)),
                    RetainedSpecies = integer(length = nrow(wp3)),
                    GeneralEcosystem = integer(length = nrow(wp3)),
                    Fisheries = integer(length = nrow(wp3)),
                    HumanWellBeing = integer(length = nrow(wp3)),
                    Livelihood = integer(length = nrow(wp3))
)

fao_4 <- data.frame(ScientificBasis = integer(length = nrow(wp4)),
                    Governance = integer(length = nrow(wp4)),
                    ExternalHumanDrivers = integer(length = nrow(wp4)),
                    ExternalEcologicalDrivers = integer(length = nrow(wp4)),
                    RetainedSpecies = integer(length = nrow(wp4)),
                    GeneralEcosystem = integer(length = nrow(wp4)),
                    Fisheries = integer(length = nrow(wp4)),
                    HumanWellBeing = integer(length = nrow(wp4)),
                    Livelihood = integer(length = nrow(wp4))
)

fao_5 <- data.frame(ScientificBasis = integer(length = nrow(wp5)),
                    Governance = integer(length = nrow(wp5)),
                    ExternalHumanDrivers = integer(length = nrow(wp5)),
                    ExternalEcologicalDrivers = integer(length = nrow(wp5)),
                    RetainedSpecies = integer(length = nrow(wp5)),
                    GeneralEcosystem = integer(length = nrow(wp5)),
                    Fisheries = integer(length = nrow(wp5)),
                    HumanWellBeing = integer(length = nrow(wp5)),
                    Livelihood = integer(length = nrow(wp5))
)

fao_6 <- data.frame(ScientificBasis = integer(length = nrow(wp6)),
                    Governance = integer(length = nrow(wp6)),
                    ExternalHumanDrivers = integer(length = nrow(wp6)),
                    ExternalEcologicalDrivers = integer(length = nrow(wp6)),
                    RetainedSpecies = integer(length = nrow(wp6)),
                    GeneralEcosystem = integer(length = nrow(wp6)),
                    Fisheries = integer(length = nrow(wp6)),
                    HumanWellBeing = integer(length = nrow(wp6)),
                    Livelihood = integer(length = nrow(wp6))
)
wp2 <- cbind(wp2, fao_2)
wp3 <- cbind(wp3, fao_3)
wp4 <- cbind(wp4, fao_4)
wp5 <- cbind(wp5, fao_5)
wp6 <- cbind(wp6, fao_6)
#====

#===
## WP2 Application of coding to individual records ----
#===
## Coding logic for relevant WP variables into FAO categories
ScientificBasis <- c()
Governance <- c("Conflict avoidance (Economic, Social, Environmental)", "Improved fishery management", "Multiple objectives", "Expanded creel area")
ExternalHumanDrivers <- c()
ExternalEcologicalDrivers <- c()
RetainedSpecies <- c("Stock recovery")
GeneralEcosystem <- c("Sustainability-Biodiversity protection", "Species protection-Discard reduction")
Fisheries <- c()
HumanWellBeing <- c("Health")
Livelihood <- c("Fleet Efficiency", "Fleet Efficiency ", "Social equity")

wp2$ScientificBasis <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% ScientificBasis, 1, 0)
wp2$Governance <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% Governance, 1, 0)
wp2$ExternalHumanDrivers <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% ExternalHumanDrivers, 1, 0)
wp2$ExternalEcologicalDrivers <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% ExternalEcologicalDrivers, 1, 0)
wp2$RetainedSpecies <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% RetainedSpecies, 1, 0)
wp2$GeneralEcosystem <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% GeneralEcosystem, 1, 0)
wp2$Fisheries <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% Fisheries, 1, 0)
wp2$HumanWellBeing <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% HumanWellBeing, 1, 0)
wp2$Livelihood <- ifelse(wp2$Objective.of.Management.Policy.Clean %in% Livelihood, 1, 0)

wp2_fao <- aggregate(cbind(ScientificBasis, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, Fisheries, HumanWellBeing, Livelihood)~as.factor(as.character(Year)),
                    data = wp2,
                    FUN = "sum")
wp2_fao$WP <- "SocioEconomic"

## remove records that are ambiguous
wp2 <- wp2[!wp2$Objective.of.Management.Policy.Clean %in% c("Not Applicable", "Unspecified", "Multiple objectives"), ]

wp2[rowSums(wp2[, 214:222]) == 0, "Objective.of.Management.Policy.Clean"]
nrow(wp2[wp2$Objective.of.Management.Policy.Clean %in% c("Not Applicable", "Unspecified", "Multiple objectives"), ])
wp2[wp2$Objective.of.Management.Policy.Clean %in% c("Expanded creel area"), ]
#====

#===
## WP3 Application of coding to individual records ----
#===
## Coding logic for relevant WP variables into FAO categories
ScientificBasis <- c()
Governance <- c()
ExternalHumanDrivers <- c("ABIOTIC ANTHROPIC")
ExternalEcologicalDrivers <- c("TEMPERATURE", "FOOD", "CLIMATE & OCEANOGRAPHY", "BIOTIC", "PREDATOR")
RetainedSpecies <- c("COMPETITION & DENSITY DEPENDENCE")
GeneralEcosystem <- c()
Fisheries <- c()
HumanWellBeing <- c()
Livelihood <- c()

wp3$ScientificBasis <- ifelse(wp3$Driverscategory %in% ScientificBasis, 1, 0)
wp3$Governance <- ifelse(wp3$Driverscategory %in% Governance, 1, 0)
wp3$ExternalHumanDrivers <- ifelse(wp3$Driverscategory %in% ExternalHumanDrivers, 1, 0)
wp3$ExternalEcologicalDrivers <- ifelse(wp3$Driverscategory %in% ExternalEcologicalDrivers, 1, 0)
wp3$RetainedSpecies <- ifelse(wp3$Driverscategory %in% RetainedSpecies, 1, 0)
wp3$GeneralEcosystem <- ifelse(wp3$Driverscategory %in% GeneralEcosystem, 1, 0)
wp3$Fisheries <- ifelse(wp3$Driverscategory %in% Fisheries, 1, 0)
wp3$HumanWellBeing <- ifelse(wp3$Driverscategory %in% HumanWellBeing, 1, 0)
wp3$Livelihood <- ifelse(wp3$Driverscategory %in% Livelihood, 1, 0)

wp3_fao <- aggregate(cbind(ScientificBasis, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, Fisheries, HumanWellBeing, Livelihood)~as.factor(as.character(Year)),
                     data = wp3,
                     FUN = "sum")
wp3_fao$WP <- "EcologicalDrivers"

## remove records that are ambiguous
wp3 <- wp3[!wp3$Driverscategory %in% c("Other"), ]

# ## Diagnostics
# unique(wp3$Driverscategory)
# wp3[rowSums(wp3[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, "Driverscategory"]
# 
# unique(wp3[rowSums(wp3[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, "Environmental.Drivers."])
# head(wp3[rowSums(wp3[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, ])
# 
# unique(wp3$Environmental.Drivers.)
# 
# nrow(wp3[wp3$Driverscategory %in% c("Other"), ])
# wp3[wp3$Driverscategory %in% c("Other"), "Additional.info.about.response"]
#====

#===
## WP4 Application of coding to individual records ----
#===
## Coding logic for relevant WP variables into FAO categories
ScientificBasis <- c("placeholder")
Governance <- c("placeholder")
ExternalHumanDrivers <- c("placeholder")
ExternalEcologicalDrivers <- c("placeholder")
RetainedSpecies <- c("4.2|placeholder")
GeneralEcosystem <- c("4.3|4.4|4.5|placeholder")
Fisheries <- c("placeholder") #"4.2", "4.3", "4.4", "4.5") Removed because all elements are related to fisheries across all reviews.
HumanWellBeing <- c("placeholder")
Livelihood <- c("placeholder")

wp4$ScientificBasis <- ifelse(grepl(pattern = ScientificBasis,
                                    x = wp4$WP4.task,
                                    ignore.case = T),1,0)
wp4$Governance <- ifelse(grepl(pattern = Governance,
                               x = wp4$WP4.task,
                               ignore.case = T),1,0)
wp4$ExternalHumanDrivers <- ifelse(grepl(pattern = ExternalHumanDrivers,
                                         x = wp4$WP4.task,
                                         ignore.case = T),1,0)
wp4$ExternalEcologicalDrivers <- ifelse(grepl(pattern = ExternalEcologicalDrivers,
                                              x = wp4$WP4.task,
                                              ignore.case = T),1,0)
wp4$RetainedSpecies <- ifelse(grepl(pattern = RetainedSpecies,
                                    x = wp4$WP4.task,
                                    ignore.case = T),1,0)
wp4$GeneralEcosystem <- ifelse(grepl(pattern = GeneralEcosystem,
                                     x = wp4$WP4.task,
                                     ignore.case = T),1,0)
wp4$Fisheries <- ifelse(grepl(pattern = Fisheries,
                              x = wp4$WP4.task,
                              ignore.case = T),1,0)
wp4$HumanWellBeing <- ifelse(grepl(pattern = HumanWellBeing,
                                   x = wp4$WP4.task,
                                   ignore.case = T),1,0)
wp4$Livelihood <- ifelse(grepl(pattern = Livelihood,
                               x = wp4$WP4.task,
                               ignore.case = T),1,0)

# wp4$ScientificBasis <- ifelse(wp4$WP4.task %in% ScientificBasis, 1, 0)
# wp4$Governance <- ifelse(wp4$WP4.task %in% Governance, 1, 0)
# wp4$ExternalHumanDrivers <- ifelse(wp4$WP4.task %in% ExternalHumanDrivers, 1, 0)
# wp4$ExternalEcologicalDrivers <- ifelse(wp4$WP4.task %in% ExternalEcologicalDrivers, 1, 0)
# wp4$RetainedSpecies <- ifelse(wp4$WP4.task %in% RetainedSpecies, 1, 0)
# wp4$GeneralEcosystem <- ifelse(wp4$WP4.task %in% GeneralEcosystem, 1, 0)
# wp4$Fisheries <- ifelse(wp4$WP4.task %in% Fisheries, 1, 0)
# wp4$HumanWellBeing <- ifelse(wp4$WP4.task %in% HumanWellBeing, 1, 0)
# wp4$Livelihood <- ifelse(wp4$WP4.task %in% Livelihood, 1, 0)

wp4_fao <- aggregate(cbind(ScientificBasis, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, Fisheries, HumanWellBeing, Livelihood)~as.factor(as.character(Year)),
                     data = wp4,
                     FUN = "sum")
wp4_fao$WP <- "FisheriesImpacts"

## remove records that are ambiguous
wp4 <- wp4[!wp4$WP4.task %in% c("None"), ]

# ## Diagnostics
# unique(wp4$WP4.task)
# wp4[rowSums(wp4[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, "WP4.task"]
# 
# unique(wp4[rowSums(wp4[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, "WP4.task"])
# head(wp4[rowSums(wp4[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, ])
# 
# unique(wp4$Environmental.Drivers.)
# 
# nrow(wp4[wp4$WP4.task %in% c("None"), ])
# wp4[wp4$WP4.task %in% c("Other"), "Additional.info.about.response"]
#====

#===
## WP5 Application of coding to individual records ----
#===
## Coding logic for relevant WP variables into FAO categories
ScientificBasis <- c()
Governance <- c("Management")
ExternalHumanDrivers <- c("Climate change", "Pollution", "Habitat change", "Human press. (not fishing)", "Environmental_Habitat/location_Pollution_Fishing", "Habitat/location_Pollution")
ExternalEcologicalDrivers <- c("Environmental", "Habitat/location", "Intra- inter species specific", "depth_salinity_temperature_primary productivity_fishing", "depth_bottom temperature_salinity_fishing effort","Environmental_Habitat/location_Pollution_Fishing")
RetainedSpecies <- c("occurrence", "biomass", "maturity", "Stock Abundance", "other (ratio of eastern to western population sampled per location)", "other (Large Fish Indicator)", "other (Mean Maximum Weight)", "other (Biomass size spectrum slope)", "Environmental_Habitat/location_Pollution_Fishing")
GeneralEcosystem <- c()
Fisheries <- c("Fishing", "fishing", "catch", "Environmental_Habitat/location_Pollution_Fishing")
HumanWellBeing <- c()
Livelihood <- c("socio-economic", "other (economic vars related to value,CPUE, abundance etc. )")

wp5$ScientificBasis <- ifelse(wp5$Driver.pressure.type %in% ScientificBasis, 1, 0)
wp5$Governance <- ifelse(wp5$Driver.pressure.type %in% Governance, 1, 0)
wp5$ExternalHumanDrivers <- ifelse(wp5$Driver.pressure.type %in% ExternalHumanDrivers, 1, 0)
wp5$ExternalEcologicalDrivers <- ifelse(wp5$Driver.pressure.type %in% ExternalEcologicalDrivers, 1, 0)
wp5$RetainedSpecies <- ifelse(wp5$Driver.pressure.type %in% RetainedSpecies, 1, 0)
wp5$GeneralEcosystem <- ifelse(wp5$Driver.pressure.type %in% GeneralEcosystem, 1, 0)
wp5$Fisheries <- ifelse(wp5$Driver.pressure.type %in% Fisheries, 1, 0)
wp5$HumanWellBeing <- ifelse(wp5$Driver.pressure.type %in% HumanWellBeing, 1, 0)
wp5$Livelihood <- ifelse(wp5$Driver.pressure.type %in% Livelihood, 1, 0)

ScientificBasis <- c("placeholder")
Governance <- c("placeholder")
ExternalHumanDrivers <- c("placeholder")
ExternalEcologicalDrivers <- c("depth|salinity|temperature|primary productivity|habitat")
RetainedSpecies <- c("occurrence|biomass|maturity|ratio of eastern to western|Large Fish Indicator|Mean Maximum Weight|Biomass|Stock Abundance|mortality rate|genetic|abundance")
GeneralEcosystem <- c("genetic")
Fisheries <- c("Fishing|fishing|catch")
HumanWellBeing <- c("placeholder")
Livelihood <- c("economic vars")

wp5$ScientificBasis <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(ScientificBasis, wp5$Metric), 1, wp5$ScientificBasis)
wp5$Governance <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(Governance, wp5$Metric), 1, wp5$Governance)
wp5$ExternalHumanDrivers <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(ExternalHumanDrivers, wp5$Metric), 1, wp5$ExternalHumanDrivers)
wp5$ExternalEcologicalDrivers <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(ExternalEcologicalDrivers, wp5$Metric), 1, wp5$ExternalEcologicalDrivers)
wp5$RetainedSpecies <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(RetainedSpecies, wp5$Metric), 1, wp5$RetainedSpecies)
wp5$GeneralEcosystem <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(GeneralEcosystem, wp5$Metric), 1, wp5$GeneralEcosystem)
wp5$Fisheries <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(Fisheries, wp5$Metric), 1, wp5$Fisheries)
wp5$HumanWellBeing <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(HumanWellBeing, wp5$Metric), 1, wp5$HumanWellBeing)
wp5$Livelihood <- ifelse(wp5$Driver.pressure.type == "Multiple" & grepl(Livelihood, wp5$Metric), 1, wp5$Livelihood)

wp5[wp5$SW.ID == "SW5_0671", "ExternalHumanDrivers"] <- 1
wp5[wp5$SW.ID %in% c("SW5_0715"), "Fisheries"] <- 1

wp5_fao <- aggregate(cbind(ScientificBasis, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, Fisheries, HumanWellBeing, Livelihood)~as.factor(as.character(Year)),
                     data = wp5,
                     FUN = "sum")
wp5_fao$WP <- "SpatialDeterminants"

## remove records that are ambiguous
wp5 <- wp5[!wp5$Driver.pressure.type %in% c(""), ]

## Diagnostics
wp5[, (ncol(wp5)-8):ncol(wp5)]
colSums(wp5[, (ncol(wp5)-8):ncol(wp5)])

unique(wp5$Driver.pressure.type)
wp5[rowSums(wp5[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, ]

nrow(wp5[wp5$Driver.pressure.type %in% c("Other"), ])
wp5[wp5$Driver.pressure.type %in% c("Multiple"), "Metric"]
#====

#===
## WP6 Application of coding to individual records ----
#===
## Coding logic for relevant WP variables into FAO categories
ScientificBasis <- c()
Governance <- c("Input|Output|Other|Spatial")
ExternalHumanDrivers <- c()
ExternalEcologicalDrivers <- c("Yes")
RetainedSpecies <- c("Single stock|Multi-stock|sub-stocks|population model|stock")
GeneralEcosystem <- c("Ecosystem model|with species interactions|benthos|seabirds|harbour porpoise|Macrobenthos|biological")
Fisheries <- c("Fishing")
HumanWellBeing <- c()
Livelihood <- c("Yes")

# wp6$ScientificBasis <- ifelse(grepl(ScientificBasis, wp6$), 1, wp6$ScientificBasis)
wp6$Governance <- ifelse(grepl(Governance, wp6$Management.Measures.considered..drop.down.list), 1, wp6$Governance)
# wp6$ExternalHumanDrivers <- ifelse(grepl(ExternalHumanDrivers, wp6$Metric), 1, wp6$ExternalHumanDrivers)
wp6$ExternalEcologicalDrivers <- ifelse(grepl(ExternalEcologicalDrivers, wp6$Environmental.variables...drop.down.list.), 1, wp6$ExternalEcologicalDrivers)
wp6$RetainedSpecies <- ifelse(grepl(RetainedSpecies, wp6$Biological.component..drop.downlist.), 1, wp6$RetainedSpecies)
wp6$GeneralEcosystem <- ifelse(grepl(GeneralEcosystem, wp6$Biological.component..drop.downlist.), 1, wp6$GeneralEcosystem)
wp6$Fisheries <- ifelse(grepl(Fisheries, wp6$Biological.component..drop.downlist.), 1, wp6$Fisheries)
# wp6$HumanWellBeing <- ifelse(grepl(HumanWellBeing, wp6$Metric), 1, wp6$HumanWellBeing)
wp6$Livelihood <- ifelse(grepl(Livelihood, wp6$Economic.component...drop.down.list.), 1, wp6$Livelihood)

wp6_fao <- aggregate(cbind(ScientificBasis, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, Fisheries, HumanWellBeing, Livelihood)~as.factor(as.character(Year)),
                     data = wp6,
                     FUN = "sum")
wp6_fao$WP <- "ManagementEvaluations"

## remove records that are ambiguous
wp6 <- wp6[rowSums(wp6[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) >= 1, ]

# ## Diagnostics
# unique(wp6$Management.Measures.considered..drop.down.list)
# wp6[rowSums(wp6[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0,
#     c("Management.Measures.considered..drop.down.list.", "Environmental.variables...drop.down.list.", "Biological.component..drop.downlist.", "Economic.component...drop.down.list.")]
# 
# unique(wp6[rowSums(wp6[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, "Environmental.Drivers."])
# nrow(wp6[rowSums(wp6[, c("ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood")]) == 0, ])
#====

#===
## Aggregate by FAO Level 2 categories and merge all WPs to remove duplicates ----
#===
# keepcols <- c("DOI", "Title", "WP", "Regions", "Year", "ScientificBasis", "Fisheries", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood") # "ScientificBasis", "Fisheries",
keepcols <- c("DOI", "Title", "WP", "Regions", "Year", "Governance", "ExternalHumanDrivers", "ExternalEcologicalDrivers", "RetainedSpecies", "GeneralEcosystem", "HumanWellBeing", "Livelihood") # "ScientificBasis", "Fisheries",

awp <- rbind(wp2[, keepcols],
             wp3[, keepcols],
             as.data.frame(wp4)[, keepcols],
             wp5[, keepcols],
             wp6[, keepcols])
awp <- awp[awp$Title != "", ]

# awpa <- aggregate(cbind(ScientificBasis, Fisheries, Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title, # + Regions + Year,
awpa <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title, # + Regions + Year,
                  data = awp,
                  FUN = sum,
                  drop = FALSE)

awpa$DOI <- awp[match(x = awpa$Title, table = awp$Title), "DOI"]
awpa$Year <- awp[match(x = awpa$Title, table = awp$Title), "Year"]

awpab <- awpa
awpab[!colnames(awpab) %in% c("Title")] <- lapply(awpab[!colnames(awpab) %in% c("Title")],
                                                  pmin, 1)

unique(awp[rowSums(awp[,6:14]) == 0, "WP"]) 
#====

#===
## Aggregate by Anna's intermedicate FAO categories and merge to remove duplicates ----
#===
HumanDrivers <- c("Governance", "ScientificBasis", "ExternalHumanDrivers")
EcologicalDrivers <- c("ExternalEcologicalDrivers")
HumanWellBeing <- c("Livelihood", "HumanWellBeing")
EcosystemWellBeing <- c("RetainedSpecies", "GeneralEcosystem")
Fisheries <- c("Fisheries")

FAO_IL <- data.frame(HumanDrivers = rep(0, times=nrow(awp)),
                     EcologicalDrivers  = rep(0, times=nrow(awp)),
                     HumanWellBeing = rep(0, times=nrow(awp)),
                     EcosystemWellBeing = rep(0, times=nrow(awp)),
                     Fisheries = rep(0, times=nrow(awp)))


il_awp <- cbind(awp, FAO_IL)
il_awp$HumanDrivers <-  ifelse(rowSums(il_awp[, HumanDrivers]) >=1, 1, 0)
il_awp$EcologicalDrivers <- ifelse(il_awp[, EcologicalDrivers] >=1, 1, 0)
il_awp$HumanWellBeing <- ifelse(rowSums(il_awp[, HumanWellBeing]) >=1, 1, 0)
il_awp$EcosystemWellBeing <- ifelse(rowSums(il_awp[, EcosystemWellBeing]) >=1, 1, 0)
il_awp$Fisheries <-  ifelse(il_awp[, Fisheries] >=1, 1, 0)

nrow(il_awp[rowSums(il_awp[,c("HumanDrivers", "EcologicalDrivers", "HumanWellBeing", "EcosystemWellBeing", "Fisheries")])==0, c("WP", "DOI")])
il_awp[rowSums(il_awp[,c("HumanDrivers", "EcologicalDrivers", "HumanWellBeing", "EcosystemWellBeing", "Fisheries")])==0, ]

unique(il_awp[rowSums(il_awp[,c("HumanDrivers", "EcologicalDrivers", "HumanWellBeing", "EcosystemWellBeing", "Fisheries")])==0, c("WP")])

#====

#===
# Literature Review Analyses
#===
#===
##  Compare results from different reviews ----
#===
## De-duplicate within WP
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title + WP,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)

## Convert to binary, so that each record contributes only once to a category, within a WP.
tempdf[!colnames(tempdf) %in% c("Title", "WP")] <- lapply(tempdf[!colnames(tempdf) %in% c("Title", "WP")],
                                                           pmin, 1)
## Aggregated to WP
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~WP,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)
tempdf$WP <- factor(tempdf$WP, levels = c("Fisheries\non Ecosystem", "Ecosystem\non Fisheries", "Models\nSupporting EBFM", "Spatial\nManagement", "Socio-economic"))


## reshape to long for plotting
tempdf_L <- reshape(tempdf,direction = "long",
                    varying = colnames(tempdf)[!colnames(tempdf) %in% c("WP")],
                    v.names = c("NumberOfRecords"),
                    timevar = c("FAO_EAF_Groups"),
                    times = colnames(tempdf)[!colnames(tempdf) %in% c("WP")])

odf <- aggregate(NumberOfRecords ~ FAO_EAF_Groups, tempdf_L, FUN = "sum")
odf$FAO_EAF_Groups <- factor(odf$FAO_EAF_Groups, levels = odf[order(odf$NumberOfRecords), "FAO_EAF_Groups"])
tempdf_L$FAO_EAF_Groups <- factor(tempdf_L$FAO_EAF_Groups, levels = levels(odf$FAO_EAF_Groups))

fao_wp <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = WP, y = NumberOfRecords,
                         fill = FAO_EAF_Groups),
           position = "stack") +
  xlab("Focus of Each Review") +
  ylab("Number of Records") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_EAF_Groups))]), name = "FAO Category")

fao_wp

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_wp.png",
       plot = fao_wp,
       width = 180,
       height = 90,
       units = "mm")
#====

# #===
# ##  Compare results from different reviews over time ----
# #===
# ## De-duplicate within WP
# tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title + WP,
#                     data = awp,
#                     FUN = sum,
#                     drop = TRUE)
# 
# ## Convert to binary, so that each record contributes only once to a category, within a WP.
# tempdf[!colnames(tempdf) %in% c("Title", "WP")] <- lapply(tempdf[!colnames(tempdf) %in% c("Title", "WP")],
#                                                           pmin, 1)
# ## Aggregated to WP
# tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~WP,
#                     data = awp,
#                     FUN = "sum",
#                     drop = TRUE)
# tempdf$WP <- factor(tempdf$WP, levels = c("Fisheries\non Ecosystem", "Ecosystem\non Fisheries", "Models\nSupporting EBFM", "Spatial\nManagement", "Socio-economic"))
# 
# 
# ## reshape to long for plotting
# tempdf_L <- reshape(tempdf,direction = "long",
#                     varying = colnames(tempdf)[!colnames(tempdf) %in% c("WP")],
#                     v.names = c("NumberOfRecords"),
#                     timevar = c("FAO_EAF_Groups"),
#                     times = colnames(tempdf)[!colnames(tempdf) %in% c("WP")])
# 
# tempdf_L <- tempdf_L[tempdf_L$FAO_EAF_Groups != "ScientificBasis", ]
# 
# fao_wp <- ggplot() +
#   geom_col(data = tempdf_L,
#            mapping = aes(x = WP, y = NumberOfRecords,
#                          fill = FAO_EAF_Groups),
#            position = "stack") +
#   xlab("Focus of Each Review") +
#   ylab("Number of Records") +
#   theme_few() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.75))+
#   scale_fill_manual(values = swCols[1:length(unique(tempdf_L$FAO_EAF_Groups))])
# 
# fao_wp
# #====

#===
##  Compare results by different region ----
#===
## De-duplicate within Regions
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title + Regions,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)

## Convert to binary, so that each record contributes only once to a category, within a Regions.
tempdf[!colnames(tempdf) %in% c("Title", "Regions")] <- lapply(tempdf[!colnames(tempdf) %in% c("Title", "Regions")],
                                                               pmin, 1)
## Aggregated to Regions
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Regions,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)
tempdf$Regions <- factor(tempdf$Regions, levels = c("North\nSea", "Mediterranean\nSea", "Western\nWaters", "Baltic\nSea", "Other\nEuropean","Global"))

## reshape to long for plotting
tempdf_L <- reshape(tempdf,direction = "long",
                    varying = colnames(tempdf)[!colnames(tempdf) %in% c("Regions")],
                    v.names = c("NumberOfRecords"),
                    timevar = c("FAO_EAF_Groups"),
                    times = colnames(tempdf)[!colnames(tempdf) %in% c("Regions")])

tempdf_L <- tempdf_L[tempdf_L$FAO_EAF_Groups != "ScientificBasis", ]

odf <- aggregate(NumberOfRecords ~ FAO_EAF_Groups, tempdf_L, FUN = "sum")
odf$FAO_EAF_Groups <- factor(odf$FAO_EAF_Groups, levels = odf[order(odf$NumberOfRecords), "FAO_EAF_Groups"])
tempdf_L$FAO_EAF_Groups <- factor(tempdf_L$FAO_EAF_Groups, levels = levels(odf$FAO_EAF_Groups))

fao_reg <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = Regions, y = NumberOfRecords,
                         fill = FAO_EAF_Groups),
           position = "stack") +
  xlab("SEAwise Regional Case Studies") +
  ylab("Number of Records") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_EAF_Groups))]), name = "FAO Category")

fao_reg

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_reg.png",
       plot = fao_reg,
       width = 180,
       height = 90,
       units = "mm")

## By percentage
tl <- split(tempdf_L, f = tempdf_L$Regions)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$NumberOfRecords), 0, tl[[i]]$NumberOfRecords/(sum(tl[[i]]$NumberOfRecords, na.rm = T))*100)
}
tempdf_L <- do.call(rbind, tl)

fao_reg_pc <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = Regions, y = OccurPer,
                         fill = FAO_EAF_Groups),
           position = "stack") +
  xlab("SEAwise Regional Case Studies") +
  ylab("Number of Records") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_EAF_Groups))]), name = "FAO Category")

fao_reg_pc

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_reg_pc.png",
       plot = fao_reg_pc,
       width = 180,
       height = 90,
       units = "mm")
#====

#===
##  Compare results by FAO_Group, over time ----
#===
## Aggregated to Year (total across regions)
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~ Title + Year,
                     data = awp,
                     FUN = sum,
                     drop = TRUE)

## Convert to binary, so that each record contributes only once to a category, within a Region per year.
tempdf[!colnames(tempdf) %in% c("Title", "Year")] <- lapply(tempdf[!colnames(tempdf) %in% c("Title", "Year")],
                                                                       pmin, 1)

## Count unique records per year
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~ Year,
                    data = tempdf,
                    FUN = sum,
                    drop = TRUE)


## reshape to long for plotting
tempdf_L <- melt(tempdf,
                 id.vars = c("Year"),
                 measure.vars = colnames(tempdf)[2:length(colnames(tempdf))],
                 variable.name = "FAO_Group",
                 value.name = "Occurrence")

## Reorder by highest occurrence to lowest
odf <- aggregate(Occurrence ~ FAO_Group, tempdf_L, FUN = "sum")
odf$FAO_Group <- factor(odf$FAO_Group, levels = odf[order(odf$Occurrence), "FAO_Group"])
tempdf_L$FAO_Group <- factor(tempdf_L$FAO_Group, levels = levels(odf$FAO_Group))

## Plot
fao_year <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = Year, y = Occurrence,
                         fill = FAO_Group),
           position = "stack") +
  xlab("Year of Publication") +
  ylab("Number of Records") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category")

fao_year

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_year.png",
       plot = fao_year,
       width = 180,
       height = 90,
       units = "mm")

### values for report text
## See: "D1.5_1.6_Percentage calculations for report.R"
#====

#===
##  Compare results by FAO_Group, per Region, over time ----
#===
## De-duplicate within Regions
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Title + Regions + Year,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)

## Convert to binary, so that each record contributes only once to a category, within a Region per year.
tempdf[!colnames(tempdf) %in% c("Title", "Regions", "Year")] <- lapply(tempdf[!colnames(tempdf) %in% c("Title", "Regions", "Year")],
                                                               pmin, 1)
## Aggregated to Region & Year
tempdf <- aggregate(cbind(Governance, ExternalHumanDrivers, ExternalEcologicalDrivers, RetainedSpecies, GeneralEcosystem, HumanWellBeing, Livelihood)~Regions + Year,
                    data = awp,
                    FUN = sum,
                    drop = TRUE)
tempdf$Regions <- factor(tempdf$Regions, levels = c("North\nSea", "Mediterranean\nSea", "Western\nWaters", "Baltic\nSea", "Other\nEuropean","Global"))

## reshape to long for plotting
tempdf_L <- melt(tempdf,
                 id.vars = c("Regions", "Year"),
                 measure.vars = colnames(tempdf)[3:length(colnames(tempdf))],
                 variable.name = "FAO_Group",
                 value.name = "Occurrence")

# tempdf_L <- reshape(tempdf,direction = "long",
#                     varying = colnames(tempdf)[!colnames(tempdf) %in% c("Regions")],
#                     v.names = c("NumberOfRecords"),
#                     timevar = c("FAO_EAF_Groups"),
#                     times = colnames(tempdf)[!colnames(tempdf) %in% c("Regions")])

odf <- aggregate(Occurrence ~ FAO_Group, tempdf_L, FUN = "sum")
odf$FAO_Group <- factor(odf$FAO_Group, levels = odf[order(odf$Occurrence), "FAO_Group"])
tempdf_L$FAO_Group <- factor(tempdf_L$FAO_Group, levels = levels(odf$FAO_Group))

## Remove general European and global studies
tempdf_L <- droplevels(tempdf_L[!tempdf_L$Regions %in% c("Other\nEuropean", "Global"),])

## Plot
fao_reg_year <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = Year, y = Occurrence,
                         fill = FAO_Group),
           position = "stack") +
  xlab("Year of Publication") +
  ylab("Number of Records") +
  facet_wrap(.~Regions) +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category")

# fao_reg_year <- ggplot() +
#   geom_area(data = tempdf_L,
#            mapping = aes(x = Year, y = Occurrence,
#                          fill = FAO_Group,
#                          colour = FAO_Group)) +
#   xlab("Year of Publication") +
#   ylab("Number of Records") +
#   facet_wrap(.~Regions) +
#   theme_few() +
#   theme(axis.title = element_text(size = 9),
#         axis.text = element_text(size = 7),
#         legend.text = element_text(size = 7),
#         legend.title = element_text(size = 9),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   scale_y_continuous(expand = c(0,0)) +
#   scale_colour_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category") +
#   scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category")

fao_reg_year

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_reg_year.png",
       plot = fao_reg_year,
       width = 180,
       height = 120,
       units = "mm")

## By percentage
tl <- split(tempdf_L, f = ~Regions+Year)
for(i in seq_along(tl)){
  tl[[i]]$OccurPer <- ifelse(is.na(tl[[i]]$Occurrence), 0, tl[[i]]$Occurrence/(sum(tl[[i]]$Occurrence, na.rm = T))*100)
}
tempdf_L <- do.call(rbind, tl)

## Plot
fao_reg_year_pc <- ggplot() +
  geom_col(data = tempdf_L,
           mapping = aes(x = Year, y = OccurPer,
                         fill = FAO_Group),
           position = "stack") +
  xlab("Year of Publication") +
  ylab("Percentage of Records") +
  facet_wrap(.~Regions) +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category")

# fao_reg_year_pc <- ggplot() +
#   geom_area(data = tempdf_L,
#             mapping = aes(x = Year, y = OccurPer,
#                           fill = FAO_Group,
#                           colour = FAO_Group)) +
#   xlab("Year of Publication") +
#   ylab("Number of Records") +
#   facet_wrap(.~Regions) +
#   theme_few() +
#   theme(axis.title = element_text(size = 9),
#         axis.text = element_text(size = 7),
#         legend.text = element_text(size = 7),
#         legend.title = element_text(size = 9),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   scale_y_continuous(expand = c(0,0)) +
#   scale_colour_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category") +
#   scale_fill_manual(values = rev(swCols[1:length(unique(tempdf_L$FAO_Group))]), name = "FAO Category")

fao_reg_year_pc

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/fao_reg_year_pc.png",
       plot = fao_reg_year_pc,
       width = 180,
       height = 120,
       units = "mm")
#====

#===
##  Generate Dataframe of Review Results as percentages ----
#===
fate_review <- data.frame(FAO_Category = colnames(awpab)[!colnames(awpab) %in% c("Title", "DOI", "Year")],
                          Review = unname(colSums(awpab[!colnames(awpab) %in% c("Title", "DOI", "Year")])))
fate_review$Review_Percent <- (fate_review$Review/sum(fate_review$Review)*100)
#====

#===
#  Compare reviews non-scientific scoping and indicators----
#===
## Align formatting
broad_scoping <- droplevels(broad_scoping[!broad_scoping$FAO_Category %in% c("Scientific Basis", "Fisheries"),])
broad_scoping$FAO_Category <- as.character(broad_scoping$FAO_Category)
broad_scoping <- broad_scoping[order(broad_scoping$FAO_Category), ]

fate_review <- fate_review[order(fate_review$FAO_Category), ]
fate_review$FAO_CategoryOrig <- fate_review$FAO_Category
fate_review$FAO_Category <- broad_scoping$FAO_Category

broad_indicator <- droplevels(broad_indicator[!broad_indicator$FAO_Category %in% c("scientific basis"),])
broad_indicator$FAO_Category <- as.character(broad_indicator$FAO_Category)
broad_indicator <- rbind(broad_indicator, c("External human drivers", 0, 0.000000))
broad_indicator <- broad_indicator[order(broad_indicator$FAO_Category), ]
broad_indicator$OccurPer <- as.numeric(broad_indicator$OccurPer)

## Generate Single Dataframe

sandf <- data.frame("FAO_Category" = broad_scoping$FAO_Category,
                    "Scoping" = broad_scoping$OccurPer,
                    "Literature Review" = fate_review$Review_Percent,
                    "Indicator Review" = broad_indicator$OccurPer)
# sandf <- sandf[order(sandf$Literature.Review), ]

sandf$FAO_Category <- factor(sandf$FAO_Category, levels = sandf[order(sandf$Literature.Review), "FAO_Category"])

sandfL <- melt(sandf,
               id.vars = "FAO_Category",
               measure.vars = colnames(sandf)[2:4],
               variable.name = "Source",
               value.name = "Percentage.Occurrence")

pl_comb <- ggplot() +
  geom_col(sandfL,
            mapping = aes(x=Source,
                          y=Percentage.Occurrence,
                          fill = FAO_Category),
           width = 0.7) +
  xlab("Source") +
  ylab("Percentage of Occurrences") +
  theme_few() +
  theme(axis.title = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(swCols[1:length(unique(sandfL$FAO_Category))]), name = "FAO Category")

pl_comb

ggsave(filename = "Systematic Reviews/Analysis D1.5/Figures/pl_comb.png",
       plot = pl_comb,
       width = 180,
       height = 120,
       units = "mm")
#====
