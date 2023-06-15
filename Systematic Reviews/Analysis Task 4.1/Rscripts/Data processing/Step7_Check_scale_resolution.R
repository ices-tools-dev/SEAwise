#####################################################################################################################-
#####################################################################################################################-
#
#     Step 7. Check spatial and temporal scale and resolution
#
#     By Esther Beukhof
#     June 2023
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#
library(ggplot2)
library(colorspace)
library(ggthemes)
library(openxlsx)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/"


#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 6.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_methods.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa_PressVar_RespVar_methods.rds"))



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Check missing values ----
#-----------------------------------------------#

# Spatial scale
table(data$Scale...Spatial..m., useNA = "always") #22 rows with NA

data_spatSca_NA <- subset(data, is.na(data$Scale...Spatial..m.)) #select rows with NA

length(unique(data_spatSca_NA$SW.ID)) #8 papers

data_spatSca_NA[,c(1,4:7)] #many also NA for spatial resolution, but not for temporal scale and resolution

table(data_spatSca_NA$SW.ID, data_spatSca_NA$Sampling.Method.used.for.data.collection) #several papers on behavioural observations and irregular surveys

table(data_spatSca_NA$SW.ID, data_spatSca_NA$Study.type) #mostly lab experiments or field experiment/observations


# Spatial resolution
table(data$Resolution...Spatial..m., useNA = "always") #111 rows with NA

data_spatRes_NA <- subset(data, is.na(data$Resolution...Spatial..m.)) #select rows with NA

length(unique(data_spatRes_NA$SW.ID)) #34 papers - quite a lot!

data_spatRes_NA[,c(1,4:7)] #quite some with no temporal resolution either, but include papers with many rows

table(data_spatRes_NA$SW.ID, data_spatRes_NA$Sampling.Method.used.for.data.collection) #mix of methods

table(data_spatRes_NA$SW.ID, data_spatRes_NA$Study.type) #mix of study types

table(data_spatRes_NA$Study.type, data_spatRes_NA$Scale...Spatial..m.) #many at very large spatial scale


# Temporal scale
table(data$Scale...Temporal, useNA = "always") #19 rows with NA

data_tempSca_NA <- subset(data, is.na(data$Scale...Temporal)) #select rows with NA

length(unique(data_tempSca_NA$SW.ID)) #7 papers

data_tempSca_NA[,c(1,4:7)] #quite some also NA for temporal resolution

table(data_tempSca_NA$SW.ID, data_tempSca_NA$Sampling.Method.used.for.data.collection) #mix of methods

table(data_tempSca_NA$SW.ID, data_tempSca_NA$Study.type) #mix of study types


# Temporal resolution
table(data$Resolution...Temporal, useNA = "always") #65 rows with NA

data_TempRes_NA <- subset(data, is.na(data$Resolution...Temporal)) #select rows with NA

length(unique(data_TempRes_NA$SW.ID)) #18 papers

data_TempRes_NA[,c(1,4:7)] #quite some with for spatial resolution as well

table(data_TempRes_NA$SW.ID, data_TempRes_NA$Sampling.Method.used.for.data.collection) #mix of methods

table(data_TempRes_NA$SW.ID, data_TempRes_NA$Study.type) #mix of study types

table(data_TempRes_NA$Study.type, data_TempRes_NA$Scale...Temporal) #mix of temporal scales


# In total...
length(unique(data$SW.ID[is.na(data$Scale...Spatial..m.) | is.na(data$Scale...Temporal) |
                           is.na(data$Resolution...Spatial..m.) | is.na(data$Resolution...Temporal)]))
#45 papers for which spatial and/or temporal scale and/or resolution is not reported



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Plotting ----
#-----------------------------------------------#

#-----------------------------------------------#
## Spatial Scales ----
#-----------------------------------------------#

## Make spatial extent and scale categories
data$ScaleSpatial <- factor(x = data$Scale...Spatial..m.,
                              levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                              ordered = TRUE)
data$ResSpatial <- factor(x = data$Resolution...Spatial..m.,
                            levels = c("0-5", "5-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", "5,000-10,000", "10,000-50,000", "50,000-100,000", ">100,000"),
                            ordered = TRUE)

spatResEx_cat <- expand.grid(levels(data$ScaleSpatial),
                             levels(data$ScaleSpatial))

colnames(spatResEx_cat) <- c("SpatialExtent_m", "SpatialRes_m")

## Make counts of articles in different combinations of SPATIAL EXTENTS & RESOLUTIONS
spatResEx_count <- aggregate(SW.ID~ScaleSpatial+ResSpatial,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)

names(spatResEx_count)[names(spatResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatResEx_count <- merge(x = spatResEx_cat,
                         y = spatResEx_count,
                         by.y = c("ScaleSpatial", "ResSpatial"),
                         by.x = c("SpatialExtent_m", "SpatialRes_m"),
                         all.x = TRUE)

spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "spatialResVExt_beforeChecking.png"),
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
## Dependent on code above


## Make temporal extent and scale categories
data$ScaleTemporal <- factor(x = data$Scale...Temporal,
                               levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                               ordered = TRUE)
data$ResTemporal <- factor(x = data$Resolution...Temporal,
                             levels = c("snapshot/no repeat sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                             ordered = TRUE)

tempResEx_cat <- expand.grid(levels(data$ScaleTemporal),
                             levels(data$ScaleTemporal))

colnames(tempResEx_cat) <- c("TemporalExtent", "TemporalRes")

## Make counts of articles in different combinations of TEMPORAL EXTENTS & RESOLUTIONS
tempResEx_count <- aggregate(SW.ID~ScaleTemporal+ResTemporal,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)

names(tempResEx_count)[names(tempResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


tempResEx_count <- merge(x = tempResEx_cat,
                         y = tempResEx_count,
                         by.y = c("ScaleTemporal", "ResTemporal"),
                         by.x = c("TemporalExtent", "TemporalRes"),
                         all.x = TRUE)

tempResEx_count[is.na(tempResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "temporalResVExt_beforeChecking.png"),
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
               scale_x_discrete(drop = FALSE, 
                                labels = c("snapshot/\nrepeat sampling", levels(data$ScaleTemporal)[-1])) +
               scale_y_discrete(drop = FALSE,
                                labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
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
spatempResEx_cat <- expand.grid(levels(data$ScaleSpatial),
                                levels(data$ScaleTemporal))

colnames(spatempResEx_cat) <- c("SpatialScale_m", "TemporalScale")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL EXTENTS
spatempEx_count <- aggregate(SW.ID~ScaleSpatial+ScaleTemporal,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)
names(spatempEx_count)[names(spatempEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempEx_count <- merge(x = spatempResEx_cat,
                         y = spatempEx_count,
                         by.y = c("ScaleSpatial", "ScaleTemporal"),
                         by.x = c("SpatialScale_m", "TemporalScale"),
                         all.x = TRUE)

spatempEx_count[is.na(spatempEx_count$NumberOfArticles), "NumberOfArticles"] <- 0


## Plot
ggsave(filename = paste0(outPath, "spatiotemporalExt_beforeChecking.png"),
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
         scale_y_discrete(drop = FALSE,
                          labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
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
                              data = data[!duplicated(data$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempResEx_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialScale_m", "TemporalScale"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "spatiotemporalRes_beforeChecking.png"),
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
         scale_y_discrete(drop = FALSE,
                          labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
         ylab("Temporal Resolution") +
         xlab("Spatial Resolution (m)") +
         theme_few() +
         theme(text = element_text(size = 9), 
               # axis.text.x = element_text(hjust = 0.5, vjust = 1),
               axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         )
)



#####################################################################################################################-
#####################################################################################################################-
#-----------------------------------------------#
# Correct potentially spurious scales and/or resolutions (identified by plots) ----
#-----------------------------------------------#

#-----------------------------------------------#
## Spatial scale vs. resolution ----
#-----------------------------------------------#

### Spatial resolution larger than spatial extent ----
idx             <- which(data$ScaleSpatial < data$ResSpatial)
datSpatIssue    <- data[idx,]
unique(datSpatIssue$SW.ID) #3 papers

# Correct papers manually

## SW4_0528 

# Paper says canyon 'runs 110 km wide' but fished area is smaller, so spatial scale of 50,000-100,000 seems right.
# Spatial resolution of >100,000m then is not right, as it should be the median distance between the moorings within the canyon.
# Map in paper suggests this is around 5-10 km on average, so 5,000-10,000m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0528"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_0528"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0528"] <- "5,000-10,000"


## SW4_0868

# Spatial scale is over 100,000 m based on the map in the paper.
# According to the same map, median distance between trawl hauls is about 20 km, so 10,000-50,000 m instead of >100,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0868"]                         <- ">100,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0868"]                                <- ">100,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0868"] <- ">100,000"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0868"]                         <- "10,000-50,000"
data$ResSpatial[data$SW.ID %in% "SW4_0868"]                                       <- "10,000-50,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0868"] <- "10,000-50,000"


## SW4_1284

# Using the map in the paper, the largest distance between two reefs is around 20, so 10,000-50,000 m, instead of 50,000-100,000 m.
# Four samples were taken, three at reef A with around 2km distance, and one at reef B 20 km away. So median is around 2 km, i.e. 1,000-5,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1284"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1284"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1284"] <- "10,000-50,000"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1284"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_1284"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1284"] <- "1,000-5,000"


# Check again whether there are rows where spatial resolution is larger than spatial extent
idx             <- which(data$ScaleSpatial < data$ResSpatial)
datSpatIssue    <- data[idx,]
unique(datSpatIssue$SW.ID) #none



### Spatial extent very large and spatial resolution very small ----
datSpatIssue    <- subset(data, ScaleSpatial > "10,000-50,000" & ResSpatial < "500-1,000")
datSpatIssue    <- subset(datSpatIssue, !Study.type %in% "Modelling/simulation") #drop modelling studies, as they often use high spatial resolution at large scale
unique(datSpatIssue$SW.ID) #26 papers


# Correct papers manually

## SW4_0014
# Map in paper has no scale,but help of Google Maps, spatial scale of the study area is around 100 km, so spatial scale is fine.
# Spatial resolution of 0-5 m seems very, very small, but it could well be the distance between two individual pots.
# As inference is done largely at the pot level, this seems thus fine.

## SW4_0044
# Spatial scale is fine, but resolution is much larger due to large distances between the three areas where they deloyed the ROV.
# So resolution is 50,000-100,000 m, and not 100-500 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0044"]                         <- "50,000-100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0044"]                                       <- "50,000-100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0044"] <- "50,000-100,000"

## SW4_0307
# Four sampling stations are far apart, so spatial resolution is 50,000-100,000 m, instead of 50-100 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0307"]                         <- "50,000-100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0307"]                                       <- "50,000-100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0307"] <- "50,000-100,000"

## Sw4_0502
# Small spatial resolution seems correct, but spatial extent is slightly smaller based on map in paper:
# 10,000-50,000 m rather than 50,000-100,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0502"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0502"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0502"] <- "10,000-50,000"

## SW4_0589
# Spatial extent is smaller than reported based on map in paper, 50,000-100,000 m instead of >100,000 m.
# Spatial resolution is likely 5,000-10,000 m (instead of ) with some further away, and others closer together.
# Temporal extent is set as five years while only snapshot sampling was done in 2013. Yet, it's related to 
# VMS data from several years prior to sampling as well as closing of areas five years ago, so this if fine. But no need to
#  make distinction between 'fishing frequency' and 'fishing intensity'.
# Dab is set as a target species, but this was a non-target species.
# Fishing dependent data - no, all were from irregular fisheries independent survey.

data$Species.taxonomic.group.s.[data$SW.ID %in% "SW4_0589" & data$Pressure_level %in% "Non-target"] <- "Hippoglossoides platessoides _ Limanda limanda"
data$Species.taxonomic.group.s.[data$SW.ID %in% "SW4_0589" & data$Pressure_level %in% "Target"] <- "Pleuronectes platessa _ Nephrops norvegicus"

data_allScreened$Species.taxonomic.group.s.[data_allScreened$SW.ID %in% "SW4_0589" & data_allScreened$Pressure_level %in% "Non-target"] <- "Hippoglossoides platessoides _ Limanda limanda"
data_allScreened$Species.taxonomic.group.s.[data_allScreened$SW.ID %in% "SW4_0589" & data_allScreened$Pressure_level %in% "Target"] <- "Pleuronectes platessa _ Nephrops norvegicus"

data$Sampling.Method.used.for.data.collection[data$SW.ID %in% "SW4_0589"] <- "Irregular Fisheries Independent Survey"
data_allScreened$Sampling.Method.used.for.data.collection[data_allScreened$SW.ID %in% "SW4_0589"] <- "Irregular Fisheries Independent Survey"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0589"]                         <- "5,00-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_0589"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0589"] <- "5,000-10,000"

data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0589"]                         <- "50,000-100,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0589"]                                <- "50,000-100,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0589"] <- "50,000-100,000"

data$Pressure_variable[data$SW.ID %in% "SW4_0589"] <- "Trawling frequency"
data_allScreened$Pressure_variable[data_allScreened$SW.ID %in% "SW4_0589"] <- "Trawling frequency"

### Drop duplicated rowIDs
rowIDs <- data$ROWID[data$SW.ID %in% "SW4_0589"]
rowIDkeep <- c()
RV <- unique(data$Response.variable_paper[data$SW.ID %in% "SW4_0589"])
for(i in 1:length(RV)){
        subdat <- subset(data, SW.ID %in% "SW4_0589" & Response.variable_paper %in% RV[i])
        rowIDkeep <- c(rowIDkeep, subdat$ROWID[c(1,2)])
}
rowIDdrop <- rowIDs[!rowIDs %in% rowIDkeep]
data <- subset(data, !ROWID %in% rowIDdrop)

rowIDs <- data_allScreened$ROWID[data_allScreened$SW.ID %in% "SW4_0589"]
rowIDkeep <- c()
RV <- unique(data_allScreened$Response.variable_paper[data_allScreened$SW.ID %in% "SW4_0589"])
for(i in 1:length(RV)){
        subdat <- subset(data_allScreened, SW.ID %in% "SW4_0589" & Response.variable_paper %in% RV[i])
        rowIDkeep <- c(rowIDkeep, subdat$ROWID[c(1,2)])
}
rowIDdrop <- rowIDs[!rowIDs %in% rowIDkeep]
data_allScreened <- subset(data_allScreened, !ROWID %in% rowIDdrop)

## SW4_0614
# Experiments done on cod to mimic pulse trawl effects. In discussion some claims made that they do not expect pulse trawl fisheries 
# will not negatively impact the cod population in the North Sea. So in that sense large spatial scale of >100,000 is OK.

## SW4_0670
# Questionnaires have been handed out to "about 500 anglers, who lived on the Italian Northern Adriatic coast or visited it frequently".
# Not clear where they exactly live or where approached but this might well be very close to one another (same fishing spot) or far away.
# Resolution of 0-5 m seems thus on the very low side, so let's go for 1,000-5,000 m as intermediate assumed distance.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0670" & data$Sampling.Method.used.for.data.collection %in% "Interview/questionnaire"]   <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_0670" & data$Sampling.Method.used.for.data.collection %in% "Interview/questionnaire"]                 <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0670" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Interview/questionnaire"] <- "1,000-5,000"

## SW4_0688
# No map in paper, unknown what the distance between shipwrecks - perhaps not shown on purpose.
# Spatial resolution of 0-5 m seems way too small. As scale is likely indeed >100km, median resolution is likely rather
# 5-10 km as distance between the 18 wrecks studied.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0688"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_0688"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0688"] <- "5,000-10,000"

## SW4_0717
# High-resolution tagging data so large spatial scale and small spatial resolution are correct.

## SW4_0725
# Several ROV dives were undertaken in multiple canyons, so spatial resolution is likely 1,000-5,000 m rather than 10-50 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0725"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_0725"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0725"] <- "1,000-5,000"

## SW4_0777
# No scale on map but distance between samples likely 1,000-5,000 m instead of 50-100m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0777"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_0777"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0777"] <- "1,000-5,000"

## SW4_0792
# Based on map in paper, spatial resolution likely 1,000-5,000 m instead of 0-5 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0792"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_0792"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0792"] <- "1,000-5,000"

## SW4_0802
# Based on map in paper, spatial scale 10,000-50,000 m rather than 50,000-100,000 m.
# Spatial resolution 100-500 m instead of 0-5 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0802"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0802"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0802"] <- "10,000-50,000"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0802"]                         <- "100-500"
data$ResSpatial[data$SW.ID %in% "SW4_0802"]                                       <- "100-500"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0802"] <- "100-500"

## SW4_0936
# Ring-reading is done in two harbours >100 km apart. So both scale and resolution are >100,000 m.
# Inference is also made based on comparing these two harbours rather than observations within harbours.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0936" & data$Description.Other.Sampling.Method %in% "Ring Reading"]                                      <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0936" & data$Description.Other.Sampling.Method %in% "Ring Reading"]                                                    <- ">100,000"  
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0936" & data_allScreened$Description.Other.Sampling.Method %in% "Ring Reading"]  <- ">100,000"

## SW4_0955
# Simulated dynamics so makes sense spatial resolution is small, however, the model does not seem to have a spatial element.
# So perhaps best to turn spatial resolution NA.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0955"]                         <- NA
data$ResSpatial[data$SW.ID %in% "SW4_0955"]                                       <- NA
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0955"] <- NA

## SW4_0979
# Long-term line transect survey to observe cetaceans, spanning over a >100 km stretch. It is not mentioned what the distance is between
# transects, but this is likely not 50-100 m. Instead go for 1,000-5,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0979"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_0979"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0979"] <- "1,000-5,000"

## SW4_1036
# Island is according to the map in the paper not longer than 10-15 km, so spatial extent is likely 10,000-50,000 m, instead of >100,000 m.
# It also states that the surveyed area is 500 km2. Not clear how the survey was set up, e.g. in terms of transects, but it states that the
# surveys tries "to equally cover the study area (about 500 km2, within 6 nautical miles from the coast) each year". The spatial resolution
# may therefore indeed be quite high, so let's keep that 0-5 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1036"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1036"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1036"] <- "10,000-50,000"

## SW4_1099
# Sampling states were spaced with 5 km distance, so spatial resolution should be 1,000-5,000 m instead of 100-500 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1099"]                         <- "1,000-5,000"
data$ResSpatial[data$SW.ID %in% "SW4_1099"]                                       <- "1,000-5,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1099"] <- "1,000-5,000"

## SW4_1156
# Large spatial area, so spatial extent is correct. Unknown from the paper where exact strandings were reported, so not clear what the
# spatial resolution is. Inference is done at the scale of of coastlines of different areas, so let's take this as resolution:
# Most coastlines are over >100 km, so let's take spatial resolution as >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1156"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1156"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1156"] <- ">100,000"

## SW4_1167
# Fishing data from one port were used to assess fishing at the scale of the Tyrrhenian Sea, so spatial scale of >100,000 m seems correct.
# Spatial resolution is taken as the assumed distance between fishing locations at sea, 5,000-10,000 m instead of 0-5 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1167"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_1167"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1167"] <- "5,000-10,000"

## SW4_1224
# There are two experiments reported based on wild fish caught during two experimental angling at different sites in the south of Majorca Island:
# - Injury and short-term mortality (4-5h after catching). 
# - Longer-term mortality after 10 days in tank.
# There's no map, so not clear what the size of the overall study area is. Inference seems to be at the large scale, so >100,000 m seems correct.
# Resolution of the tank is 0-5m and left blank for the injury and direct mortality measurements. As angling was done at different sites, let's
# take a resolution of 5,000-10,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1224"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_1224"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1224"] <- "5,000-10,000"

## SW4_1305
# Two trial done in two lochs, one 7.5 km long the other 14 km. Distance between lochs is >100,000 km. 
# Not clear what the distance between fleets of pots were within lochs. Results are presented for each loch separately.
# So let's take the size of each loch as spatial extent 5,000-10,000 m instead of >100,000. 
# Resolution of 5-10 m seems fine.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1305"]                         <- "5,000-10,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1305"]                                <- "5,000-10,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1305"] <- "5,000-10,000"

## SW4_1355
# Several sites have been quite intensively sampled that are quite far apart. In the end, inference is done at the large scale, not at the scale
# of the individual sites. So spatial scale of >100,000 m is correct. Spatial resolution of 0-5 might be too small, but hard to say from the
# small map in the paper. Let's take 100-500 m instead.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1355"]                         <- "100-500"
data$ResSpatial[data$SW.ID %in% "SW4_1355"]                                       <- "100-500"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1355"] <- "100-500"

## SW4_1386
# Square size is 5 x 5 km, so spatial resolution should be either 1,000-5,000 or 5,000-10,000 m.
# As part of the inference is made at level of EUNIS habitat types, let's take the larger resolution of 5,000-10,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1386"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_1386"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1386"] <- "5,000-10,000"

## SW4_1722
# Spatial extent is large (southern North Sea) with a few sites across this large area where repeated samples were taken with 300 m distance
# in between, hence the resolution of 100-500 m. Yet the distance between sites is >100 km. Results are presented per site (based on
# sediment type) and also extrapolated to the larger scale, so let's take that as the resolution: >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1722"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1722"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1722"] <- ">100,000"

## SW4_1947
# Relatively old paper from 1972 on mackerel fisheries in the North Sea, reporting in 'statistical rectangles fished'. ICES' website says these have
# been used 'since the 1970s', so let's take these as resolution: 50,000-100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1947"]                         <- "50,000-100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1947"]                                       <- "50,000-100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1947"] <- "50,000-100,000"

# Check what is left after cleaning
datSpatIssue    <- subset(data, ScaleSpatial > "10,000-50,000" & ResSpatial < "500-1,000")
datSpatIssue    <- subset(datSpatIssue, !Study.type %in% "Modelling/simulation") #drop modelling studies, as they often use high spatial resolution at large scale
unique(datSpatIssue$SW.ID) #4 papers



#-----------------------------------------------#
## Temporal scale vs. resolution ----
#-----------------------------------------------#

### Temporal resolution larger than temporal extent ----

# Identify rows where temporal resolution is greater than temporal extent
idx             <- which(data$ScaleTemporal < data$ResTemporal)
datTempIssue    <- data[idx,]
unique(datTempIssue$SW.ID) #6 papers


# Correct papers manually

## SW4_0115

# Reported treatment duration from Table 1 in paper indicates that the temporal scale is subday, instead of day.
# Time between control and impact treatment is indeed multiple days, but inference is done based on what happens within a days's time of
# catching fish during normal fishing operations. So take 'subday' as temporal resolution, instead of 'two weeks'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "subday"
data$ScaleTemporal[data$SW.ID %in% "SW4_0115"]                            <- "subday"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "subday"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0115"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "subday"


## SW4_0738

# Temporal scale is multi-decadal, instead of decadal.
# Observations are in blocks of around two decades each. So temporal resolution was correctly reported - multidecadal.
data$Scale...Temporal[data$SW.ID %in% "SW4_0738"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0738"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0738"] <- "multidecadal"


## SW4_0783

# Temporal scale is multidecadal, instead of year.
# Temporal resolution is year, instead of multidecadal.
data$Scale...Temporal[data$SW.ID %in% "SW4_0783"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0783"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0783"] <- "multidecadal"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0783"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0783"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0783"] <- "year"


## SW4_0829

# Four months between the first and last field sampling, so temporal scale should be half year, instead of subday.
# Paper doesn't give exact dates of sampling, only the month in which it took place. Yet inference seems to be done at
# temporal resolution of one or more months, 'month' as temporal resolution is fine.
data$Scale...Temporal[data$SW.ID %in% "SW4_0829"]                         <- "half year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0829"]                            <- "half year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0829"] <- "half year"


## SW4_1224

# There are two experiments reported:
# - Injury and short-term mortality (4-5h after catching): temporal scale is thus subday and not halfyear, and
#   resolution is subday as well, and not week. 
data$Scale...Temporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "half year"]                                     <- "subday"
data$ScaleTemporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "half year"]                                        <- "subday"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1224" & data_allScreened$Scale...Temporal %in% "half year"] <- "subday"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "subday"]                                     <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "subday"]                                               <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1224" & data_allScreened$Scale...Temporal %in% "subday"] <- "subday"

# - Longer-term mortality after 10 days in tank: temporal scale of week is fine then, temporal resolution should
#   be subday (and not month), as fish were monitored three times a day.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "week"]                                     <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "week"]                                               <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1224" & data_allScreened$Scale...Temporal %in% "week"] <- "subday"


## SW4_1320
# Paper states there was 24h between control and impact sampling, so temporal scale is day, as already reported.
# It doesn't state it clearly but replicate samples seems to have been taken on the same day. So temporal
# resolution is subday, instead of month. This also makes sense in terms of inference by the authors.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1320"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_1320"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1320"] <- "subday"


# Check again whether there are rows where temporal resolution is larger than temporal extent
idx             <- which(data$ScaleTemporal < data$ResTemporal)
datTempIssue    <- data[idx,]
unique(datTempIssue$SW.ID) #none



### Temporal extent very large and temporal resolution very small ----
datTempIssue    <- subset(data, ScaleTemporal > "five year" & ResTemporal %in% c("subday","day","week")) #snapshots are OK in combination with large temporal scale
unique(datTempIssue$SW.ID) #16 papers


# Correct papers manually

## SW4_0199
# Temporal scale is indeed decade yet, within areas, the temporal scale is just a few years (2006-2009 for all, except one: 2015-2017). So scale
# should be five year rather than decade. It doesn't not seem like the same persons were interviewed multiple times or that they wanted to
# to get a time series. Temporal resolution is there a snapshot instead of week.
data$Scale...Temporal[data$SW.ID %in% "SW4_0199"]                         <- "five year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0199"]                            <- "five year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0199"] <- "five year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0199"]                         <- "snapshot/no repeat sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0199"]                                   <- "snapshot/no repeat sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0199"] <- "snapshot/no repeat sampling"

## Sw4_0272
# Temporal scale is indeed multidecadal. Temporal resolution is indeed subday, as time step in model was 1200 sec.

## Sw4_0365
# Paper looked at VMS records across 10 year period, so decadal scale is correct. Although VMS pings are indeed at subday resolution, inference is made
# across the entire 10 years period, as theu authors also write themselves that they base their analysis on a "contemporary snapshot (last 10 years).
# So let's change resolution to snapshot.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0365"]                         <- "snapshot/no repeat sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0365"]                                   <- "snapshot/no repeat sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0365"] <- "snapshot/no repeat sampling"

## SW4_0579
# They used two types of data: tagging and bycatch data - the latter spanning over a decade and not being reported separately, while tagging only
# spanned a few years. Therefore add extra row for bycatch data and change temporal scale for the tagging data.
newRow     <- data[data$SW.ID %in% "SW4_0579",]
newRow_all <- data_allScreened[data_allScreened$SW.ID %in% "SW4_0579",]

newRow$Sampling.Method.used.for.data.collection     <- "Fisheries Dependent Data"
newRow_all$Sampling.Method.used.for.data.collection <- "Fisheries Dependent Data"

# The tagging data were collected in two separate years within a few months time. Inference is done by not including this two year separation.
# So rather take months as temporal scale.
data$Scale...Temporal[data$SW.ID %in% "SW4_0579"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_0579"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0579"] <- "two month"

data             <- rbind(data, newRow)
data_allScreened <- rbind(data_allScreened, newRow_all)

## SW4_0592
# Two cruises performed within two months time, so temporal scale is two months instead of multidecadal.
# Unclear at which temporal resolution sampling was, but results are interpreted as a snapshot, rathern than 'subday'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0592"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_0592"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0592"] <- "two month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0592"]                         <- "snapshot/no repeat sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0592"]                                   <- "snapshot/no repeat sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0592"] <- "snapshot/no repeat sampling"

## SW4_0705
# Model ran for 9 years and one parameter was tuned with weekly temperature measurements, so temporal scale and resolution are correct.
# Also matches with inference made by authors, as they specifically look at temporal aggregation.

# SW4_0751
# One of the types of data used experimental fishing nets similar to fisheries operating in the area. Data are fishery independent, and not dependent.
# This survey has been running for multiple decades instead of 'half year', with resolution of year instead of week.
# Landings data for a few years, so temporal scale should be five year instead of multidecadal. Inference seems to be done by year, so scale is year instead of week.

# Survey data
data$Sampling.Method.used.for.data.collection[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries dependent survey"]           <- "Regular Fisheries Independent Survey"
data$Study.type[data$SW.ID %in% "SW4_0751" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "Fisheries independent survey"

data_allScreened$Sampling.Method.used.for.data.collection[data_allScreened$SW.ID %in% "SW4_0751" & data_allScreened$Study.type %in% "Fisheries dependent survey"]           <- "Regular Fisheries Independent Survey"
data_allScreened$Study.type[data_allScreened$SW.ID %in% "SW4_0751" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "Fisheries independent survey"

data$Scale...Temporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"]              <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"]                 <- "multidecadal"
data_allScreened$Scale...Temporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"] <- "multidecadal"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"]             <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"]                       <- "year"
data_allScreened$Resolution...Temporal[data$SW.ID %in% "SW4_0751" & data$Study.type %in% "Fisheries independent survey"] <- "year"

# Landings data
data$Scale...Temporal[data$SW.ID %in% "SW4_0751"]                         <- "five year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0751"]                            <- "five year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0751"] <- "five year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0751"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0751"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0751"] <- "year"

## SW4_0887
# Catches indeed recorded on a weekly basis, but only during the fishing season of five weeks. Inference is also made by year.
# So temporal resolution is year instead of week. Data also span multiple decades instead of decadal.
data$Scale...Temporal[data$SW.ID %in% "SW4_0887"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0887"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0887"] <- "multidecadal"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0887"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0887"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0887"] <- "year"

## Sw4_0915
# Temporal scale is indeed decade and resolution day due to logbook data being used from a 10 year period.

## SW4_0919
# Focus on 2006-2008 so temporal scale is two year instead of multidecadal. Hydrodynamic model output is hour subday of day.
data$Scale...Temporal[data$SW.ID %in% "SW4_0919"]                         <- "two year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0919"]                            <- "two year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0919"] <- "two year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0919"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0919"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0919"] <- "subday"

## SW4_0941
# Modelling study with long time scale and daily resolution.

## SW4_1036
# Daily observations from July-Sept for 10 years, so temporal scale is indeed decade.
# Yet observations are not interpreted on a daily but on a yearly basis. So temporal resolution is year instead of day.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1036"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_1036"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1036"] <- "year"

## SW4_1199
# Fishing pressure data span multiple decades, but benthos sampling was done in July & August in one year, so resolution is
# snapshot rather than day. Inference is however done at the multidecadal scale based on the fishing data. So scale is correct.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1199"]                         <- "snapshot/no repeat sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1199"]                                   <- "snapshot/no repeat sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1199"] <- "snapshot/no repeat sampling"

## Sw4_1465
# Inference done by year, so temporal resolution is year instead of subday.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1465"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_1465"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1465"] <- "year"

## SW4_1841
# Paper indicates that only trawl survey data have been used - not fisheries dependent catch data with temporal
# scale and resolution of multidecadal and subday respectively. Remove therefore these lines.
data             <- data[!(data$SW.ID %in% "SW4_1841" & data$Resolution...Temporal %in% "subday"),]
data_allScreened <- data_allScreened[!(data_allScreened$SW.ID %in% "SW4_1841" & data_allScreened$Resolution...Temporal %in% "subday"),]

## SW4_1947
# Inference done and data analysed by year, so temporal resolution is year instead of subday.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1947"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_1947"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1947"] <- "year"



#-----------------------------------------------#
## Spatial vs. temporal scale ----
#-----------------------------------------------#

## Large spatial scale vs. small temporal scale ----
datLargeSpSmallTe <- subset(data, ScaleSpatial %in% ">100,000" & ScaleTemporal < "two week")
unique(datLargeSpSmallTe$SW.ID) #3 papers

# Correct papers manually

## SW4_0679
# Paper only states that two days of sampling were done in the Tyrrhenian Sea, but not where exactly.
# So reviewer likely reported the scale of the entire Tyrrhenian Sea.
# Spatial resolution was for the same reason kept blank. This is fully understandable.
# Temporal scale was reported as day, however there were 3.5 months in between day 1 and day 2.
# So temporal scale should rather be 'two month'. 
data$Scale...Temporal[data$SW.ID %in% "SW4_0679"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_0679"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0679"] <- "two month"


## SW4_1224
# Already assessed in code above.


## SW4_1714
# Spatial and temporal scale are more or less correct: paper states that sampling was done within 12 days time,
# so temporal scale is rather 'two week' than 'week', at several stations in the southern North Sea, where
# the map indicates that these are several tens of km apart and the overall area being >100 km.
data$Scale...Temporal[data$SW.ID %in% "SW4_1714"]                         <- "two week"
data$ScaleTemporal[data$SW.ID %in% "SW4_1714"]                            <- "two week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1714"] <- "two week"



#-----------------------------------------------#
# Plotting after checking and corrections ----
#-----------------------------------------------#

#-----------------------------------------------#
## Spatial Scales ----
#-----------------------------------------------#

## Make counts of articles in different combinations of SPATIAL EXTENTS & RESOLUTIONS
spatResEx_count <- aggregate(SW.ID~ScaleSpatial+ResSpatial,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)

names(spatResEx_count)[names(spatResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatResEx_count <- merge(x = spatResEx_cat,
                         y = spatResEx_count,
                         by.y = c("ScaleSpatial", "ResSpatial"),
                         by.x = c("SpatialExtent_m", "SpatialRes_m"),
                         all.x = TRUE)

spatResEx_count[is.na(spatResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "spatialResVExt.png"),
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

## Make counts of articles in different combinations of TEMPORAL EXTENTS & RESOLUTIONS
tempResEx_count <- aggregate(SW.ID~ScaleTemporal+ResTemporal,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)

names(tempResEx_count)[names(tempResEx_count) %in% "SW.ID"] <- "NumberOfArticles"


tempResEx_count <- merge(x = tempResEx_cat,
                         y = tempResEx_count,
                         by.y = c("ScaleTemporal", "ResTemporal"),
                         by.x = c("TemporalExtent", "TemporalRes"),
                         all.x = TRUE)

tempResEx_count[is.na(tempResEx_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "temporalResVExt.png"),
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
               scale_x_discrete(drop = FALSE, 
                                labels = c("snapshot/\nrepeat sampling", levels(data$ScaleTemporal)[-1])) +
               scale_y_discrete(drop = FALSE,
                                labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
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

## Make counts of articles in different combinations of SPATIAL & TEMPORAL EXTENTS
spatempEx_count <- aggregate(SW.ID~ScaleSpatial+ScaleTemporal,
                             data = data[!duplicated(data$SW.ID), ],
                             FUN = length)
names(spatempEx_count)[names(spatempEx_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempEx_count <- merge(x = spatempResEx_cat,
                         y = spatempEx_count,
                         by.y = c("ScaleSpatial", "ScaleTemporal"),
                         by.x = c("SpatialScale_m", "TemporalScale"),
                         all.x = TRUE)

spatempEx_count[is.na(spatempEx_count$NumberOfArticles), "NumberOfArticles"] <- 0


## Plot
ggsave(filename = paste0(outPath, "spatiotemporalExt.png"),
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
               scale_y_discrete(drop = FALSE,
                                labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
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

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = data[!duplicated(data$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempResEx_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialScale_m", "TemporalScale"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Plot
ggsave(filename = paste0(outPath, "spatiotemporalRes.png"),
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
               scale_y_discrete(drop = FALSE,
                                labels = c("snapshot/\nrepeat sampling", levels(data$ResTemporal)[-1])) +
               ylab("Temporal Resolution") +
               xlab("Spatial Resolution (m)") +
               theme_few() +
               theme(text = element_text(size = 9), 
                     # axis.text.x = element_text(hjust = 0.5, vjust = 1),
                     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
               )
)



#-----------------------------------------------#
# Check again missing values ----
#-----------------------------------------------#

datNA <- data[is.na(data$Scale...Spatial..m.) | is.na(data$Scale...Temporal) | is.na(data$Resolution...Spatial..m.) | is.na(data$Resolution...Temporal),]
length(unique(datNA$SW.ID)) #45 papers

# Rename NAs as 'Not specified'
data[c("Scale...Spatial..m.","Scale...Temporal","Resolution...Spatial..m.","Resolution...Temporal")][is.na(data[c("Scale...Spatial..m.","Scale...Temporal","Resolution...Spatial..m.","Resolution...Temporal")])] <- "Not specified"
data_allScreened[c("Scale...Spatial..m.","Scale...Temporal","Resolution...Spatial..m.","Resolution...Temporal")][is.na(data_allScreened[c("Scale...Spatial..m.","Scale...Temporal","Resolution...Spatial..m.","Resolution...Temporal")])] <- "Not specified"



#-----------------------------------------------#
# Save dataset ----
#-----------------------------------------------#

# Drop some columns to return to original columns
data$ScaleSpatial <- data$ResSpatial <- data$ScaleTemporal <- data$ResTemporal <- data$ROWID <- NULL

# Add new ROWID
data$ROWID             <- c(1:nrow(data))
data_allScreened$ROWID <- c(1:nrow(data_allScreened))

# Save
saveRDS(data, paste0(datPath,"data_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))
write.xlsx(data, file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods_ScaleRes.xlsx"))

saveRDS(data_allScreened, paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))
write.xlsx(data_allScreened, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.xlsx"))
