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
library(sf)
library(viridis)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Data_Extraction_Files/" 
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
GISpath                               <- "Systematic Reviews/Analysis Task 4.1/GIS"

################################################
#-----------------------------------------------
# Read and merge data files
#
#  info:
#  This section reads in and merges the data extraction files. 
#  The most recent versions should be downloaded from the SEAwise sharepoint before starting this analysis/exercise.
#  Some files may require manual processing to ensure correct reading, however, these should be corrected at the sharepoint.
#  Note: I took the "editable" version of the Uhlmann_Reid file (and changed the name of the file)
#  Note: I manually switched rows 1 and 2 in the Binch-file, otherwise the file is not read correctly.
#  Note: I manually removed the ".xlsx" from the filename for Potier.
#-----------------------------------------------

# readers                                <- c("Altuna-Etxabe", "Anastasopoulou", "Astarloa", "Basurko", "Binch", "Bluemel", "Brown",
#                                             "Carbonara", "Festjens", "garcia", "Girardin", "Halouani", "Lefkaditou_Chatzispyrou", "MacMillan", "Papadopoulou", "Potier",
#                                             "Romagnoni", "Seghers", "Smith", "Spedicato", "Thorpe", "Thuenen","Tsagarakis", "Uhlmann_Reid", "VanHoey", "vdReijden")
# #Missing: Dinesen.

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
## Save as rds-file
# saveRDS(tab, paste0(outPath, "tab.rds"))
# rm(tab1, tab2, people, readers)


################################################
#-----------------------------------------------
# Overview: treated, excluded, contributors
#
#  info:
#  This section provides a short overview of the dataset, including the number of papers processed, excluded and retained. 
#  It shows the number of papers per reviewer, and why the papers got excluded.
#
#  Comments:
#  Thorpe should check the exclusion criteria used. For now, I made a work-around method.
#
#-----------------------------------------------
tab                                   <- readRDS(paste0(outPath, "tab.rds"))
tab                                   <- as.data.table(tab)

## check if all rows have a SW.ID
table(is.na(tab$SW.ID))

## Check if exclusion.criteria make sense
table(tab$Exclusion.Criteria)
# Thorpe has incorrect exclusion criteria: this should be fixed in the data extraction file, but will do it here as well for now.
tab$Exclusion.Criteria                <- ifelse(tab$Reader == "Thorpe" & tab$Exclusion.Criteria %in% c("4.4", "4.3", "4.3 _ 4.4", "None"), NA, 
                                                ifelse(tab$Exclusion.Criteria == "Evidence", "EXCLUDE on evidence", tab$Exclusion.Criteria))

## how many papers treated : 721 papers (out of XXX?)
length(unique(tab$SW.ID))

## Check who did how many papers
contributors                          <- tab[,.(Papers_read = length(unique(SW.ID))), by="Reader"]

## how many papers retained : 543 papers
retained                              <- unique(subset(tab, is.na(Exclusion.Criteria)==TRUE)$SW.ID)
length(retained)

## how many rejected : 178 papers
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
#  Note: I removed some papers as their data extraction was incomplete. these are stored in the "tobechecked" file.
#-----------------------------------------------
## Select the retained papers
data                                  <- tab[SW.ID %in% retained,,]
rm(tab, contributors, retained, excluded)

## For easyness, skip the long-text columns.
data                                  <- data[,c(1, 5, 19:29, 32:49, 51)]

## Check the regions (all fine)
table(is.na(data$Region)) # 1 NA
table(data$Region)

tobechecked                           <- data[is.na(Region)==T,,]
data                                  <- data[is.na(Region)==F,,]

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
table(is.na(data$Response.variable_category)) # There are 15 NA's. 
## 3 are described as "mortality", so will classify those as mortality. 
## 2 are described as "spatial distribution changes", which are classified to Abundance/biomass/density.
## 2 are described as "discards", which are classified as mortality. The remaining NAs are classified as Other.
data$Response.variable_category       <- ifelse(data$Response.variable_paper == "Mortality" & data$SW.ID == "SW4_0402", "Mortality", # This also effects another record of the same paper, but will do for now
                                                ifelse(data$Response.variable_paper == "spatial distribution changes" & data$SW.ID == "SW4_1094", "Abundance/biomass/density", 
                                                       ifelse(data$Response.variable_paper == "discards" & data$SW.ID == "SW4_0535", "Mortality", data$Response.variable_category)))
data$Response.variable_category[is.na(data$Response.variable_category)==TRUE] <- "Other"

table(data$Response.variable_category)
data$Response.variable_category       <- ifelse(data$Response.variable_category %in% c("abundance", "Abundance", "Abundance by taxon"), "Abundance/biomass/density",
                                                ifelse(data$Response.variable_category %in% c("other"), "Other",
                                                       ifelse(data$Response.variable_category == "Other_physical", "Physiology", data$Response.variable_category)))


## Check the Pressure types (level 1)
table(is.na(data$Pressure.type)) ## 3 NAs 
# Check SW4_1285 --> incomplete info, remove this row for now (paper has 2 more rows with complete info)
tobechecked                           <- rbind(tobechecked, data[(SW.ID == "SW4_1285" & is.na(Pressure.type)==TRUE)])
data                                  <- data[!(data$SW.ID == "SW4_1285" & is.na(data$Pressure.type) == TRUE),]
# Check SW4_1478 --> set to "Discarding", as that is what is affecting the birds.
data$Pressure.type                    <- ifelse(data$SW.ID == "SW4_1478" & is.na(data$Pressure.type) == T, "Discarding", data$Pressure.type)
# Check SW4_0476 --> seems to be mixed up with SW4_0513. Should be corrected in the data extraction file.
tobechecked                           <- rbind(tobechecked, data[(SW.ID == "SW4_0476" & is.na(Pressure.type)==TRUE)])
data                                  <- data[!(SW.ID == "SW4_0476" & is.na(Pressure.type)==TRUE)]
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
                                                     "Physical_habitats", "Plankton", "Plants", "Reptiles"))$Ecosystem.component_level2)==T) # 276 NA's where there should be none.
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
table(is.na(data$Gear_level1))  # 181 NAs
table(data$Fishery.type)
data$Fishery.type                    <- ifelse(data$Fishery.type %in% c("commercial", "Artisanal"), "Commercial", data$Fishery.type)
table(data$Gear_level1)
data$Gear_level1                     <- ifelse(data$Gear_level1 == "Demersal_trawls", "Demersal trawls",
                                               ifelse(data$Gear_level1 %in% c("Pelagic _trawls", "Pelagic_trawls"), "Pelagic trawls",
                                                      ifelse(data$Gear_level1 == "Mixed gears", NA,  data$Gear_level1)))


## Check what species are commonly mentioned
length(unique(data$Species.taxonomic.group.s.)) # 453 unique input... let's skip for now.

## Check what pressure variables are commonly mentioned
length(unique(data$Pressure_variable)) # 380 unique input... Let's skip for now.


################################################
#-----------------------------------------------
# Preliminary analysis: produce some plots
#
#  info:
#  This section aims for a quick analysis of the retained papers, for the report.
#  It summarizes the number of unique papers (SW.ID) per (combination of) ecosystem component, fishing type, gear level1, region, response variable. 
#-----------------------------------------------

#-----------------------------------------------
## Barplot for regions
#-----------------------------------------------
Regions                               <- data[, .(NrPaps = length(unique(SW.ID))), 
                                             by = Region]
Regions                               <- Regions[order(NrPaps),,]

tiff(paste0(outPath, "Regions.tiff"), width=1000, height=750, res=100)
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

#-----------------------------------------------
## Barplot for Response variable categories
#-----------------------------------------------
ResVarCats                            <- data[, .(NrPaps = length(unique(SW.ID))),
                                              by = Response.variable_category]
ResVarCats                            <- ResVarCats[order(NrPaps),,]

tiff(paste0(outPath, "ResVarCats.tiff"), width=1000, height=750, res=100)
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

#-----------------------------------------------
## Heatmap of ecosystem component level 1 vs pressure type level 1
#-----------------------------------------------
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

#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoPressmat, na.rm=T))
rCols                                <- viridis(n=max(EcoPressmat, na.rm=T))   


tiff(paste0(outPath, "EcoPress_heatmap.tiff"), width= 1000, height = 1000, res = 100)
par(mar=c(19, 1, 5, 7))
plot(r, axes=F,
        col=rCols, legend=F, box=F)
segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
abline(v=c(0,1))
abline(v=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), lty=2, col="lightgrey", lwd=0.8)
segments(x0=0, x1=1, y0=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), 
         y1=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), col="lightgrey", lty=2, lwd=0.8)
axis(1, at=seq(1/14, 13/14, length.out=7), labels= c("Catch & bycatch", "Discarding", "Electromagnetic input", "Input of litter", "Noise", "Physical disturbance", "Visual disturbance"), las=3, cex.axis=1.5)
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


#-----------------------------------------------
## Barplots of fishing gears studied
#-----------------------------------------------
Gears                                <- data[,.(NrPaps = length(unique(SW.ID))),
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
## Heatmap of ecosystem component vs fishery type
#-----------------------------------------------
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
#rCols                                <- colorRampPalette(c("mistyrose", "darkred"))(max(EcoFishmat, na.rm=T))
rCols                                <- viridis(n=(max(EcoFishmat, na.rm=T)))


tiff(paste0(outPath, "EcoFish_heatmap.tiff"), width= 1000, height = 1000, res = 100)
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


#-----------------------------------------------
## Heatmap of ecosystem component vs fishery type / gear_level 1
#-----------------------------------------------
EcoFishGear                          <- data[, .(NrPaps = length(unique(SW.ID))),
                                             by = c("Ecosystem.component_level1", "Fishery.type", "Gear_level1")]
EcoFishGear$Gear_level1              <- ifelse(is.na(EcoFishGear$Gear_level1)==T, "Not specified", 
                                               ifelse(EcoFishGear$Gear_level1 == "Hooks_and_lines", "Hooks and Lines", EcoFishGear$Gear_level1)) 
rCols                                <- data.table(colcode = viridis(max(EcoFishGear$NrPaps)),
                                                   value = c(1:max(EcoFishGear$NrPaps)))

tiff(paste0(outPath, "EcoGears_heatmap.tiff"), width= 1500, height = 500, res = 100)
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
dev.off()

#-----------------------------------------------
## Region faceted heat maps
#-----------------------------------------------
RegEcoPress                          <- data[, .(NrPaps= length(unique(SW.ID))),
                                             by=c("Region", "Ecosystem.component_level1", "Pressure.type")]
# Leave out the Norwegian Sea, as it only is 1 study.
RegEcoPress                          <- RegEcoPress[!Region == "Norwegian Sea",,]
rCols                                <- data.table(colcode = viridis(max(RegEcoPress$NrPaps)),
                                                   value = c(1: max(RegEcoPress$NrPaps)))

tiff(paste0(outPath, "regEcoPress_heatmaps.tiff"), width=2000, height=2000, res=100)
par(mfrow=c(5,4))
par(mar=c(1,1,4,1))

for(iLoc in c("empty", "CS - Mediterranean", "Mediterranean - non CS", "Black Sea", 
              "empty", "CS - North Sea", "North Sea - non CS", "Barents Sea",
              "empty", "CS - Western Waters", "Western Waters - non CS", "NE-Atlantic", 
              "empty", "CS - Baltic Sea", "Baltic Sea - non CS", "Global", 
              "empty2", "empty3", "empty3", "empty3")){
  
  subdat                             <- RegEcoPress[Region == iLoc,,]
  EcoPress                           <- matrix(ncol=7, nrow=12)
  colnames(EcoPress)                 <- sort(unique(RegEcoPress$Pressure.type))
  rownames(EcoPress)                 <- sort(unique(RegEcoPress$Ecosystem.component_level1))
  
  if(iLoc == "empty") {
    plot(raster(matrix(NA, 1, 1)), axes=F, ann=F, legend=F, box=F)
    axis(2, tick=F, at=seq(0.04166666, 0.9583333, length.out=12), 
         labels=rev(sort(unique(RegEcoPress$Ecosystem.component_level1))), las=1, pos=1.45, cex.axis=2.3)}
  if(iLoc == "empty2"){
    plot(c(0,1), c(0,1), axes=F, type="n", ann=F)
    gradient.rect(xleft=0.45, xright=0.55, ybottom=0.1, ytop=0.7, col=rev(rCols$colcode), gradient="y")
    text(x=0.5, y=0.85, "Number of \n retained papers", font=2, cex=3)
    text("1", x=0.59, y=0.7, font=3, cex=2)
    text("50", x=0.59, y=0.1, font=3, cex=2)}
  if(iLoc == "empty3"){
    plot(raster(matrix(NA, 1, 1)), axes=F, ann=F, legend=F, box=F)
    axis(1, tick=F, at=seq(0.07142857, 0.9285714, length.out=7), 
         labels= c("Catch & bycatch", "Discarding", "Electromagnetic input", "Input of litter", "Noise", "Physical disturbance", "Visual disturbance"), 
         las=3, pos=1.15, cex.axis=3)}
  if(iLoc %in% c("CS - Mediterranean", "Mediterranean - non CS", "Black Sea",
                 "CS - North Sea", "North Sea - non CS", "Barents Sea",
                 "CS - Western Waters", "Western Waters - non CS", "NE-Atlantic", 
                 "CS - Baltic Sea", "Baltic Sea - non CS", "Global")) {
    for(iRow in c(1:nrow(subdat))){
      subdat2                        <- subdat[iRow,]
      EcoPress[subdat2$Ecosystem.component_level1, subdat2$Pressure.type] <- subdat2$NrPaps}
    r                                <- raster(EcoPress)
    
    plot(r, axes=F, legend=F, col=subset(rCols, value %in% values(r))$colcode, box=F)
    segments(x0=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), x1=c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7), y0=0, y1=1, lty=2, col="lightgrey", lwd=0.8)
    segments(x0=0, x1=1, y0=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), 
             y1=c(1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12,10/12, 11/12), col="lightgrey", lty=2, lwd=0.8)
    segments(x0=0, x1=1, y0=c(0,1), y1=c(0,1))
    segments(x0=c(0,1), x1=c(0,1), y0=0, y1=1)
    title(iLoc, font.main=2, cex.main=2, line=0.5)
    axis(1, at=seq(0.07142857, 0.9285714, length.out=7), labels=rep("", 7), pos=0)
    axis(2, at=seq(0.04166666, 0.9583333, length.out=12), labels=rep("", 12), pos=0)
  } # end iLoc if loop
} # end iLoc for loop
dev.off()


#-----------------------------------------------
## Creating shapefile with Regions
#-----------------------------------------------
# ## Load ICES regions as downloaded from https://gis.ices.dk/sf/ and mediterranean GSA's from https://www.fao.org/gfcm/data/maps/gsas/en/
# ICESareas                            <- st_read(dsn=GISpath, layer="ICES_Areas_20160601_cut_dense_3857")
# ICESareas                            <- st_transform(ICESareas, crs=3035)
# ICESEcors                            <- st_read(dsn=GISpath, layer="ICES_ecoregions_20171207_erase_ESRI")
# ICESEcors                            <- st_transform(ICESEcors, crs=3035)
# GSAs                                 <- st_read(dsn=GISpath, layer="GSAs_simplified")
# GSAs                                 <- st_transform(GSAs, crs=3035)
# 
# 
# ICESEcors$Region                     <- ifelse(ICESEcors$Ecoregion %in% c("Western Mediterranean Sea", "Adriatic Sea",
#                                                                    "Ionian Sea and the Central Mediterranean Sea",
#                                                                    "Aegean-Levantine Sea"), "Mediterranean - non CS", 
#                                                ifelse(ICESEcors$Ecoregion == "Greater North Sea", "North Sea - non CS", 
#                                                       ifelse(ICESEcors$Ecoregion %in% c("Black Sea", "Norwegian Sea","Barents Sea"), ICESEcors$Ecoregion,
#                                                              ifelse(ICESEcors$Ecoregion == "Baltic Sea", "Baltic Sea - non CS", 
#                                                                     ifelse(ICESEcors$Ecoregion %in% c("Azores", "Oceanic Northeast Atlantic", "Greenland Sea",
#                                                                                                       "Icelandic Waters", "Faroes", "Celtic Seas"), "NE-Atlantic", NA)))))
# 
# ICESareas$Region                     <- ifelse(ICESareas$Area_Full %in% c("27.4.c", "27.4.b", "27.4.a", "27.7.d"), "CS - North Sea",
#                                                ifelse(ICESareas$Area_Full %in% c("27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.26","27.3.d.27","27.3.d.28.1","27.3.d.28.2","27.3.d.29"), "CS - Baltic Sea",
#                                                       ifelse(ICESareas$Area_Full %in% c("27.7.a", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.8.a","27.8.b","27.8.c", "27.8.d.2"), "CS - Western Waters", 
#                                                              ifelse(ICESareas$Area_Full %in% c("27.9.a", "27.9.b.1", "27.9.b.2", "27.8.e.1", "27.8.e.2", "27.6.a", "27.7.c.2", 
#                                                                                                "27.6.b.2", "27.7.b", "27.7.k.2", "27.7.j.2", "27.7.b", "27.7.j.1"), "NE-Atlantic", NA))))
# GSAs$Region                          <- ifelse(GSAs$SECT_COD %in% c("GSA17", "GSA18", "GSA19", "GSA20", "GSA22"), "CS - Mediterranean", NA)
# 
# Regions                              <- rbind(subset(ICESEcors[,c("Region", "geometry")], is.na(Region)==F), subset(ICESareas[,c("Region", "geometry")], is.na(Region)==F))
# Regions                              <- rbind(Regions, subset(GSAs[,c("Region", "geometry")], is.na(Region)==F))
# 
# Regs                                 <- subset(Regions, Region %in% c("Baltic Sea - non CS", "Barents Sea", "Black Sea", "North Sea - non CS", "Norwegian Sea"))
# 
# for (iReg in c("CS - Mediterranean", "CS - North Sea","CS - Western Waters", "Mediterranean - non CS", "CS - Baltic Sea", "NE-Atlantic")){
#   subdat                             <- subset(Regions, Region == iReg)
#   a                                  <- st_sf(st_union(subdat))
#   b                                  <- data.frame(Region = iReg)
#   b                                  <- st_set_geometry(b, st_geometry(a)) 
#   Regs                               <- rbind(Regs, b)
# } # end iReg loop
# 
# ## Fix some overlaps that should not be there
# ## North Sea
# NS                                   <- st_intersection(subset(Regs, Region == "CS - North Sea"), subset(Regs, Region == "North Sea - non CS"))
# NS$Region.1                          <- NULL
# 
# ## NE-Atlantic
# WW                                   <- st_difference(subset(Regs, Region == "NE-Atlantic"), subset(Regs, Region == "CS - Western Waters"))
# WW$Region.1                          <- NULL
# 
# ## Mediterranean non CS
# MED                                  <- st_difference(subset(Regs, Region == "Mediterranean - non CS"), subset(Regs, Region == "CS - Mediterranean"))
# MED$Region.1                         <- NULL
# 
# ## Baltic non CS
# BALT                                 <- st_difference(subset(Regs, Region == "Baltic Sea - non CS"), subset(Regs, Region == "CS - Baltic Sea"))
# BALT$Region.1                        <- NULL
# 
# ## Update the fixes
# Regs2                                <- subset(Regs, !Region %in% c("CS - North Sea", "NE-Atlantic", "Mediterranean - non CS", "Baltic Sea - non CS"))
# Regs2                                <- rbind(Regs2, NS, WW, MED, BALT)
# 
# SEAwise4.1_regions                   <- Regs2
# st_write(SEAwise4.1_regions, paste0(GISpath, "SEAwise4.1_regions.shp"))
# save(SEAwise4.1_regions, file=paste0(GISpath, "SEAwise4.1_regions.Rdata"))


#-----------------------------------------------
## Creating map with Region specific info
#-----------------------------------------------
RegCol                               <- data.frame(colcode = viridis(max(Regions$NrPaps)),
                                                   value = c(1:max(Regions$NrPaps)))
Regions$Colcode                      <- RegCol$colcode [match(Regions$NrPaps, RegCol$value)]
load(paste0(GISpath, "SEAwise4.1_regions.Rdata"))
SEAwise4.1_regions$colcode           <- Regions$Colcode [match(SEAwise4.1_regions$Region, Regions$Region)]
Centerpoints                         <- st_coordinates(st_centroid(SEAwise4.1_regions))
SEAwise4.1_regions$Xloc              <- Centerpoints[,1]
SEAwise4.1_regions$Yloc              <- Centerpoints[,2]
SEAwise4.1_regions$NrPaps            <- Regions$NrPaps [match(SEAwise4.1_regions$Region, Regions$Region)]
SEAwise4.1_regions$textcol           <- ifelse(SEAwise4.1_regions$NrPaps >50, "black", "white")

## Change some points to a better location
SEAwise4.1_regions$Xloc              <- ifelse(SEAwise4.1_regions$Region == "North Sea - non CS", 4280000,
                                               ifelse(SEAwise4.1_regions$Region == "Baltic Sea - non CS", 4800000, 
                                                      ifelse(SEAwise4.1_regions$Region == "CS - Mediterranean", 5059132, 
                                                             ifelse(SEAwise4.1_regions$Region == "CS - Baltic Sea", 4851499, SEAwise4.1_regions$Xloc))))
SEAwise4.1_regions$Yloc              <- ifelse(SEAwise4.1_regions$Region == "North Sea - non CS", 3885000,
                                               ifelse(SEAwise4.1_regions$Region == "Mediterranean - non CS", 1248183, 
                                                      ifelse(SEAwise4.1_regions$Region == "CS - Baltic Sea", 3728215, SEAwise4.1_regions$Yloc)))

## Create plot
tiff(paste0(outPath, "RegMap.tiff"), width = 1000, height = 1000, res = 100)
par(mar=c(1,1,4,1))
plot(st_geometry(SEAwise4.1_regions), col=SEAwise4.1_regions$colcode, border=F)
title(main = "Retained papers per region", cex.main=2, font.main=2)
text(SEAwise4.1_regions$NrPaps, x= SEAwise4.1_regions$Xloc, y=SEAwise4.1_regions$Yloc, col=SEAwise4.1_regions$textcol, font=2)
text(x= 1E6, y=6E6, "Global studies:", font=3, cex=1.2)
text(x=1e6, y=5.85e6, paste0(subset(Regions, Region == "Global")$NrPaps), font=2)
gradient.rect(xleft=0, xright=1E6, ytop=1.5e6, ybottom=1.25E6, col=RegCol$colcode, gradient="x")
text(x=0, y=1.18e6, "1")
text(x=1E6, y=1.18e6, max(Regions$NrPaps))
text(x=0.5e6, y=1.72e6, "Number of \n papers retained", font=4, cex=1.2)
dev.off()

#-----------------------------------------------
## Time series for Ecocomp
#-----------------------------------------------
YearEco                              <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Ecosystem.component_level1")]
YearEco2                             <- matrix(nrow=length(unique(data$Ecosystem.component_level1)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearEco2)                   <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearEco2)                   <- sort(unique(data$Ecosystem.component_level1))
EcoCols                              <- data.frame(Eco = sort(unique(data$Ecosystem.component_level1)),
                                                   col = turbo(12))

for(iRow in c(1:nrow(YearEco))){
  subdat                             <- YearEco[iRow,]
  YearEco2[subdat$Ecosystem.component_level1, as.character(subdat$Year)] <- subdat$NrPaps
}
YearEco2[is.na(YearEco2)] <- 0

tiff(paste0(outPath, "YearEco.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearEco$Year), max(YearEco$Year)), y=c(0, max(YearEco$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearEco2)){
  lines(x=as.numeric(colnames(YearEco2)), y=YearEco2[iRow,], type="o", pch=16, lwd=2, col=subset(EcoCols, Eco == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=EcoCols$col, legend=rownames(YearEco2), bty="n", title="Ecosystem component")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per Ecosystem component", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearEco$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
## Time series for Region
#-----------------------------------------------
YearReg                              <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Region")]
YearReg2                             <- matrix(nrow=length(unique(data$Region)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearReg2)                   <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearReg2)                   <- sort(unique(data$Region))
RegCols                              <- data.frame(Reg = sort(unique(data$Region)),
                                                   col = turbo(length(unique(data$Region))))

for(iRow in c(1:nrow(YearReg))){
  subdat                             <- YearReg[iRow,]
  YearReg2[subdat$Region, as.character(subdat$Year)] <- subdat$NrPaps
}
YearReg2[is.na(YearReg2)] <- 0

tiff(paste0(outPath, "YearReg.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearReg$Year), max(YearReg$Year)), y=c(0, max(YearReg$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearReg2)){
  lines(x=as.numeric(colnames(YearReg2)), y=YearReg2[iRow,], type="o", pch=16, lwd=2, col=subset(RegCols, Reg == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=RegCols$col, legend=rownames(YearReg2), bty="n", title="Region")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per Region", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearReg$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
## Time series for Pressure type
#-----------------------------------------------
YearPres                             <- data[,.(NrPaps = length(unique(SW.ID))),
                                             by = c("Year", "Pressure.type")]
YearPres2                            <- matrix(nrow=length(unique(data$Pressure.type)),
                                               ncol=(max(data$Year) - min(data$Year) + 1),
                                               NA)
colnames(YearPres2)                  <- as.character(seq(from=min(data$Year), to= max(data$Year), 1))                                                
rownames(YearPres2)                  <- sort(unique(data$Pressure.type))
PresCols                             <- data.frame(Pres = sort(unique(data$Pressure.type)),
                                                   col = turbo(length(unique(data$Pressure.type))))

for(iRow in c(1:nrow(YearPres))){
  subdat                             <- YearPres[iRow,]
  YearPres2[subdat$Pressure.type, as.character(subdat$Year)] <- subdat$NrPaps
}
YearPres2[is.na(YearPres2)] <- 0

tiff(paste0(outPath, "YearPres.tiff"), width = 1000, height=750, res=100)
par(mar=c(5,5,7, 2))
plot(x=c(min(YearPres$Year), max(YearPres$Year)), y=c(0, max(YearPres$NrPaps)), type="n", ann=F, axes=F)
box()
for(iRow in rownames(YearPres2)){
  lines(x=as.numeric(colnames(YearPres2)), y=YearPres2[iRow,], type="o", pch=16, lwd=2, col=subset(PresCols, Pres == iRow)$col)
} # end iRow-loop
legend("topleft", cex=1.2, fill=PresCols$col, legend=rownames(YearPres2), bty="n", title="Pressure types")
axis(1, at=seq(1965, 2025, 5), labels=seq(1965, 2025, 5), cex.axis=1.2)
axis(2, las=1, cex.axis=1.2)
title("Publication years of retained papers \n per pressure type", font.main=2, cex.main=2)
axis(1, at=(max(data$Year) - ((max(data$Year) - min(data$Year))/2)), tick=F, line=2, cex.axis=1.5, "Publication year")
axis(2, at=(max(YearPres$NrPaps)/2), tick=F, line=2, cex.axis=1.5, labels="Number of retained papers")
dev.off()

#-----------------------------------------------
