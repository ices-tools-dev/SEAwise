#####################################################################################################################
#####################################################################################################################
#
#     Script to read, merge, and analyse data extraction files from SEAwise task 4.1
#
#     By Marie Savinar, adapted by Karin van der Reijden
#     March 2022
#
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

#-----------------------------------------------
# Load libraries and set Paths.
#-----------------------------------------------
library(xlsx) 
library(data.table)
library(RColorBrewer)
library(raster)
library(plotrix)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Data_Extraction_Files/" 
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"


################################################
#-----------------------------------------------
# Read and merge data files
#
#  info:
#  This section reads in and merges the data extraction files. 
#  The most recent versions should be downloaded from the SEAwise sharepoint before starting this analysis/exercise.
#  Some files may require manual processing to ensure correct reading, however, these should be corrected at the sharepoint.
#  
#-----------------------------------------------

# readers                                <- c("Altuna-Etxabe", "Anastasopoulou", "Astarloa", "Basurko", "Binch", "Bluemel", "Brown", 
#                                             "Carbonara", "Festjens", "garcia", "Girardin", "Halouani", "Lefkaditou_Chatzispyrou", "MacMillan", "Papadopoulou", "Potier", 
#                                             "Romagnoni", "Seghers", "Smith", "Spedicato", "Thuenen","Tsagarakis", "Uhlmann_Reid", "VanHoey", "vdReijden")
# Missing: Dinesen and Thorpe.

# ## Create a table from the first reader
# people                                 <- readers[1]
# tab                                    <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1)
# colnames(tab)
# tab$Reader                             <- people
# 
# ## Add all other readers
# for(people in readers[2:length(readers)]) {
#   print(people)
#   tab1                                 <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1) 
#   tab1$Reader                          <- people
#   tab2                                 <- tab1[,colnames(tab1) %in% colnames(tab)] # remove empty / additional columns
#   tab2                                 <- subset(tab2, !is.na(tab2$SW.ID)==T) # remove empty rows
#   tab                                  <- rbind(tab,tab2)
# } # end people-loop
# 
# ## Save as rds-file
# saveRDS(tab, paste0(outPath, "tab.rds"))


################################################
#-----------------------------------------------
# Overview: treated, excluded, contributors
#
#  info:
#  This section provides a short overview of the dataset, including the number of papers processed, excluded and retained. 
#  It shows the number of papers per reviewer, and why the papers got excluded.
#
#  Comments:
#  We should check paper SW4_1905, which is excluded at the moment, for an incorrect reason ("4.3")
#
#-----------------------------------------------
tab                                   <- readRDS(paste0(outPath, "tab.rds"))
tab                                   <- as.data.table(tab)

## check if all rows have a SW.ID
table(is.na(tab$SW.ID))

## how many papers treated : 693 papers (out of XXX?)
length(unique(tab$SW.ID))

## Check who did how many papers
contributors                          <- tab[,.(Papers_read = length(unique(SW.ID))), by="Reader"]

## how many papers retained : 522 papers
retained                              <- unique(subset(tab, is.na(Exclusion.Criteria)==TRUE)$SW.ID)
length(retained)

## how many rejected : 171 papers
excluded                              <- unique(subset(tab, !is.na(Exclusion.Criteria) == TRUE)$SW.ID)
length(excluded)

## Why excluded?
table(tab[SW.ID %in% excluded, Exclusion.Criteria])


################################################
#-----------------------------------------------
# Processing: cleaning / reformatting etc
#
#  info:
#  This section aims to clean the data of the retained papers.
#  It corrects for input-typos /inconsistencies and splits some double-entries over two lines.
#-----------------------------------------------
## Select the retained papers
data                                  <- tab[SW.ID %in% retained,,]

## For easyness, skip the long-text columns.
data                                  <- data[,c(1, 19:29, 32:49, 51)]

## Check the regions (all fine)
table(is.na(data$Region)) # no NAs, so fine
table(data$Region) 

## Check the spatial scale and resolution, and correct for input mistakes
table(is.na(data$Scale...Spatial..m.)) # There are 14 NAs
table(data$Scale...Spatial..m.)
data$Scale...Spatial..m.              <- ifelse(data$Scale...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Scale...Spatial..m. == "100-501", "100-500", 
                                                       ifelse(data$Scale...Spatial..m. == "50-101", "50-100", data$Scale...Spatial..m.)))
table(is.na(data$Resolution...Spatial..m.)) # There are 81 NAs
table(data$Resolution...Spatial..m.)
data$Resolution...Spatial..m.         <- ifelse(data$Resolution...Spatial..m. %in% c("50,000-100,001",  "50,000-100,002",  "50,000-100,003"), "50,000-100,000",
                                                ifelse(data$Resolution...Spatial..m. == "50-101", "50-100", 
                                                       ifelse(data$Resolution...Spatial..m. == "5-11", "5-10", 
                                                              ifelse(data$Resolution...Spatial..m. == "100-501", "100-500", data$Resolution...Spatial..m.))))

## Check the temporal scale and resolution (all fine)
table(data$Scale...Temporal)
table(data$Resolution...Temporal)
table(is.na(data$Scale...Temporal)) # There are 13 NAs
table(is.na(data$Resolution...Temporal)) # There are 44 NAs

## Check the Response variable category
table(is.na(data$Response.variable_category)) # There are 12 NA's. 
## 3 are described as "mortality", so will classify those as mortality. 
## 2 are described as "spatial distribution changes", so will classify to Abundance/biomass/density. The remaining NAs are classified as Other.
data$Response.variable_category       <- ifelse(data$Response.variable_paper == "Mortality" & data$SW.ID == "SW4_0402", "Mortality", # This also effects another record of the same paper, but will do for now
                                                ifelse(data$Response.variable_paper == "spatial distribution changes" & data$SW.ID == "SW4_1094", "Abundance/biomass/density", data$Response.variable_category))
data$Response.variable_category[is.na(data$Response.variable_category)==TRUE] <- "Other"
table(data$Response.variable_category)
data$Response.variable_category       <- ifelse(data$Response.variable_category %in% c("abundance", "Abundance", "Abundance by taxon"), "Abundance/biomass/density",
                                                ifelse(data$Response.variable_category %in% c("other"), "Other",
                                                       ifelse(data$Response.variable_category == "Other_physical", "Physiology", data$Response.variable_category)))


## Check the Pressure types (level 1)
table(is.na(data$Pressure.type)) ## 2 NAs 
# Check SW4_1285 --> incomplete info, remove this row for now (paper has 2 more rows with complete info)
data                                  <- data[!(data$SW.ID == "SW4_1285" & is.na(data$Pressure.type) == TRUE),]
# Check SW4_1478 --> set to "Discarding", as that is what is affecting the birds.
data$Pressure.type                    <- ifelse(data$SW.ID == "SW4_1478" & is.na(data$Pressure.type) == T, "Discarding", data$Pressure.type)
table(data$Pressure.type)

## Check the Pressure types (level 2)
table(is.na(subset(data, Pressure.type == "Catch_and_bycatch")$Pressure_level)) # There are 121 NAs (for rows where non-NA was expected)

table(data$Pressure_level)
data$Pressure_level                  <- ifelse(data$Pressure_level == "target", "Target", data$Pressure_level)
# 'Fix' the input of paper SW4_1621, that states both target and non-target (in data)
b                                    <- subset(data, SW.ID == "SW4_1621")
b$Pressure_level                     <- "Target"
a                                    <- b
a$Pressure_level                     <- "Non-target"
data                                 <- subset(data, !SW.ID == "SW4_1621")
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)

## Check the ecosystem component level 1
table(is.na(data$Ecosystem.component_level1)) # 3 NAs. 
# One should classify as "other" (SW.ID SW4_1968), because litter input is central
# The other two dominantly forcus on fish_teleost and Nephrops (SW.ID SW4_0479)
# 'Fix' the input of paper SW4_0479, that should refer to both Nephrops and fish_teleost flatfish
b                                    <- data[(SW.ID == "SW4_0479" & is.na(data$Ecosystem.component_level1)==T),]
b$Ecosystem.component_level1         <- "Fish_teleost"
b$Ecosystem.component_level2         <- "Flatfish"
a                                    <- b
a$Ecosystem.component_level1         <- "Benthos"
a$Ecosystem.component_level2         <- "Benthic_epifauna"
data                                 <- data[!(data$SW.ID == "SW4_0479"& is.na(data$Ecosystem.component_level1)==T),]
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)
# Reclassify the NA to "other"
data$Ecosystem.component_level1      <- ifelse(data$SW.ID == "SW4_1968", "Other", data$Ecosystem.component_level1)
table(data$Ecosystem.component_level1)

## Check the ecosystem component level 2
table(is.na(subset(data, Ecosystem.component_level1 %in% c("Fish_teleost", "Benthos", "Marine_mammals", "Fish_cartilaginous",
                                                     "Physical_habitats", "Plankton", "Plants", "Reptiles"))$Ecosystem.component_level2)==T) # 269 NA's where there should be none.
table(data$Ecosystem.component_level2)
# 'Fix' the input of paper SW4_0746, that states both benthic epifauna and infauna
b                                    <- subset(data, SW.ID == "SW4_0746")
b$Ecosystem.component_level2         <- "Benthic_epifauna"
a                                    <- b
a$Ecosystem.component_level2         <- "Benthic_infauna"
data                                 <- subset(data, !SW.ID == "SW4_0746")
data                                 <- rbindlist(list(data, a, b), use.names=TRUE)

## Check Fishery types and Gear_level1
table(is.na(data$Fishery.type)) # all fine
table(is.na(data$Gear_level1))  # 170 NAs
table(data$Fishery.type)
data$Fishery.type                    <- ifelse(data$Fishery.type %in% c("commercial", "Artisanal"), "Commercial", data$Fishery.type)
table(data$Gear_level1)
data$Gear_level1                     <- ifelse(data$Gear_level1 == "Demersal_trawls", "Demersal trawls",
                                               ifelse(data$Gear_level1 %in% c("Pelagic _trawls", "Pelagic_trawls"), "Pelagic trawls",
                                                      ifelse(data$Gear_level1 == "Mixed gears", NA,  data$Gear_level1)))


## Check what species are commonly mentioned
length(unique(data$Species.taxonomic.group.s.)) # 436 unique input... let's skip for now.

## Check what pressure variables are commonly mentioned
length(unique(data$Pressure_variable)) # 377 unique input... Let's skip for now.


################################################
#-----------------------------------------------
# Preliminary analysis: produce some plots
#
#  info:
#  This section aims for a quick analysis of the retained papers, for the report.
#  It summarizes the number of unique papers (SW.ID) per (combination of) ecosystem component, fishing type, gear level1, region, response variable. 
#-----------------------------------------------
## Barplot for regions
Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), 
                                             by = Region]
Regions                               <- Regions[order(NrPaps),,]

tiff("Regions.tiff", width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(Regions$NrPaps, horiz=TRUE, axes=F, xlim=c(0,180))
box()
axis(2, at=b, labels=Regions$Region, las=1, cex.axis=1.2)
axis(1, at= seq(0,180, 20), labels=seq(0, 180, 20), cex.axis=1.2)
title(main="Number of unique papers per study region", cex.main=1.5, font.main=2)
text(x=125, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))))
text(x=Regions$NrPaps + 4, y=b, Regions$NrPaps)
axis(1, at=90, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

## Barplot for Response variable categories
ResVarCats                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff("ResVarCats.tiff", width=1000, height=750, res=100)
par(mar=c(5, 15, 4, 2))
b                                     <- barplot(ResVarCats$NrPaps, horiz=TRUE, axes=F, xlim=c(0,245))
box()
axis(2, at=b, labels=ResVarCats$Response.variable_category, las=1, cex.axis=1.2)
axis(1, at= seq(0,250, 20), labels=seq(0, 250, 20), cex.axis=1.2)
title(main="Number of unique papers per response variable category", cex.main=1.5, font.main=2)
text(x=160, y=0, paste0("Total number of papers retained: ", length(unique(data$SW.ID))))
text(x=ResVarCats$NrPaps+6, y=b, ResVarCats$NrPaps)
axis(1, at=125, tick=F, line=2, label="Number of unique (retained) papers", cex.axis=1.3)
dev.off()

## Heatmap of ecosystem component level 1 vs pressure type level 1
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

rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat))

tiff("EcoPress_heatmap.tiff", width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
        col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/6, 2/6, 3/6, 4/6, 5/6), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), 
         y1=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(0.08333335, 0.9166667, length.out=6), labels= c("Catch & bycatch", "Discarding", "Electromagnetic input", "Input of litter", "Physical disturbance", "Visual disturbance"), las=3, cex.axis=1.5)
axis(4, at=seq(0.04166666, 0.9583333, length.out=12), labels= rev(rownames(EcoPressmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing pressure effects on ecosystem components", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("140", x=0.97, y=0, font=3)
text("70", x=0.97, y=0.15, font=3)
dev.off()


## Barplots of fishing gears studied
Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Fishery.type", "Gear_level1")]
Gears$Gear_level1                    <- ifelse(is.na(Gears$Gear_level1)==T, "Not specified", 
                                               ifelse(Gears$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", Gears$Gear_level1))
tiff("GearsStudied.tiff", width= 2000, height = 1000, res = 100)
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

## Heatmap of ecosystem component vs fishery type
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
rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))


tiff("EcoFish_heatmap.tiff", width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
     col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/4, 2/4, 3/4), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), 
         y1=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(0.125, 0.875, length.out=4), labels= colnames(EcoFishmat), las=3, cex.axis=1.5)
axis(4, at=seq(0.04166666, 0.9583333, length.out=12), labels= rev(rownames(EcoFishmat)), las=1, pos=1, cex.axis=1.5)
par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,1))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing type studied per ecosystem components", cex.main=1.5, font.main=2)
gradient.rect(xleft=0.85, xright=0.95, ybottom=0, ytop=0.3, col=rev(rCols), gradient="y")
text("Number of \n papers retained", x=0.9, y=0.35, font=4, cex=1.3)
text("1", x=0.97, y=0.3, font=3)
text("190", x=0.97, y=0, font=3)
text("95", x=0.97, y=0.15, font=3)
dev.off()


## Heatmap of ecosystem component vs fishery type / gear_level 1
EcoFishGear                          <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified", 
                                               ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1)) 
rCols                                <- data.table(colcode = c(colorRampPalette(c("mistyrose", "red"))(35), "red3", "darkred"),
                                                   value = c(1:35, 100, 146))

tiff("EcoGears_heatmap.tiff", width= 1500, height = 500, res = 100)
par(mfrow=c(1,6))
par(mar=c(13, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=12)), axes=F, box=F)
axis(2, tick=F, at=seq(0.04166666, 0.9583333, length.out=12), labels=rev(rownames(EcoFishGM)), las=1, pos=1.2, cex.axis=1.5)

for(iType in unique(EcoFishGear$Fishery.type)){
  subdat                             <- EcoFishGear[Fishery.type == iType,,]
  
  EcoFishGM                          <- matrix(nrow = 12,
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
  segments(x0=0, x1=1, y0=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), 
           y1=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), col="lightgrey", lty=2, lwd=0.8)
  segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
  segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
  axis(1, at=seq((1/ncol(EcoFishGM))/2, 1-((1/ncol(EcoFishGM))/2), length.out=ncol(EcoFishGM)), labels= colnames(EcoFishGM), las=3, cex.axis=1.5, pos=0)
  if(iType == "Scientific"){axis(4, at=seq(0.04166666, 0.9583333, length.out=12), labels= rep("", 12), pos=1)}
  if(iType == "Commercial"){axis(2, at=seq(0.04166666, 0.9583333, length.out=12), labels= rep("", 12), pos=0)}
}

par(mar=c(13, 0, 10, 0))
plot(raster(matrix(NA, ncol=2, nrow=12)), axes=F, box=F)
axis(4, tick=F, at=seq(0.04166666, 0.9583333, length.out=12), labels=rev(rownames(EcoFishGM)), las=1, pos=-0.1, cex.axis=1.5)

par(fig=c(0,1,0,1), new=TRUE, mar=c(0,1,5,0))
plot(c(0,1), c(0,1), type="n", axes=F, ann=F)
title(main="Fishing gear studied per fishing type and ecosystem components", cex.main=2, font.main=2, line=0)
gradient.rect(xleft=0.9, xright=1.01, ybottom=0.05, ytop=0.15, col=rCols$colcode, gradient="x")
text("Number of \n papers retained", x=0.955, y=0.2, font=4, cex=1.3)
text("1", x=0.89, y=0.1, font=3)
text("146", x=1.02, y=0.1, font=3)
text("100", x=1.01, y=0, font=3)
text("35", x=1, y=0.03, font=3)
dev.off()





