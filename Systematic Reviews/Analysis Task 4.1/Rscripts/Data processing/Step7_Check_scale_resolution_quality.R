#####################################################################################################################-
#####################################################################################################################-
#
#     Step 7. Check spatial and temporal scale and resolution, and quality
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
# Correct naming ----
#-----------------------------------------------#

data$Resolution...Temporal[data$Resolution...Temporal %in% "snapshot/no repeat sampling"] <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$Resolution...Temporal %in% "snapshot/no repeat sampling"] <- "snapshot/no repeated sampling"



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
                               levels = c("subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                               ordered = TRUE)
data$ResTemporal <- factor(x = data$Resolution...Temporal,
                             levels = c("snapshot/no repeated sampling", "subday", "day", "week", "two week", "month", "two month", "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"),
                             ordered = TRUE)

tempResEx_cat <- expand.grid(levels(data$ScaleTemporal),
                             levels(data$ResTemporal))

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
                                labels = levels(data$ScaleTemporal)) +
               scale_y_discrete(drop = FALSE,
                                labels = levels(data$ResTemporal)) +
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

# Make spatio-temperal extent category matrix in long form
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
## SpatioTemporal Resolution ----
#-----------------------------------------------#
## Dependent on code above

# Make spatio-temperal resolution category matrix in long form
spatempRes_cat <- expand.grid(levels(data$ResSpatial),
                                levels(data$ResTemporal))

colnames(spatempRes_cat) <- c("SpatialRes_m", "TemporalRes")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = data[!duplicated(data$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempRes_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialRes_m", "TemporalRes"),
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
                   mapping = aes(x = SpatialRes_m,
                                 y = TemporalRes,
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
# Spatial resolution is likely indeed 5,000-10,000 m with some further away, and others closer together.
# Temporal extent is set as five years while only snapshot sampling was done in 2013. Data are linked to 
# VMS data from several years prior to sampling, as well as closing of areas five years ago. As we focus
# on the response (i.e. benthos and fish sampling), supplementary info suggests sampling was done in 5 days
# with 3-4 stations a day. So let's take week as temporal scale and subday as temporal resolution.
# But no need to make distinction between 'fishing frequency' and 'fishing intensity'.
# Dab is set as a target species, but this was a non-target species.
# Fishing dependent data - no, all were from irregular fisheries independent survey.

data$Species.taxonomic.group.s.[data$SW.ID %in% "SW4_0589" & data$Pressure_level %in% "Non-target"] <- "Hippoglossoides platessoides _ Limanda limanda"
data$Species.taxonomic.group.s.[data$SW.ID %in% "SW4_0589" & data$Pressure_level %in% "Target"] <- "Pleuronectes platessa _ Nephrops norvegicus"

data_allScreened$Species.taxonomic.group.s.[data_allScreened$SW.ID %in% "SW4_0589" & data_allScreened$Pressure_level %in% "Non-target"] <- "Hippoglossoides platessoides _ Limanda limanda"
data_allScreened$Species.taxonomic.group.s.[data_allScreened$SW.ID %in% "SW4_0589" & data_allScreened$Pressure_level %in% "Target"] <- "Pleuronectes platessa _ Nephrops norvegicus"

data$Sampling.Method.used.for.data.collection[data$SW.ID %in% "SW4_0589"] <- "Irregular Fisheries Independent Survey"
data_allScreened$Sampling.Method.used.for.data.collection[data_allScreened$SW.ID %in% "SW4_0589"] <- "Irregular Fisheries Independent Survey"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0589"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_0589"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0589"] <- "5,000-10,000"

data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0589"]                         <- "50,000-100,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0589"]                                <- "50,000-100,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0589"] <- "50,000-100,000"

data$Scale...Temporal[data$SW.ID %in% "SW4_0589"]                         <- "week"
data$ScaleTemporal[data$SW.ID %in% "SW4_0589"]                            <- "week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0589"] <- "week"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0589"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0589"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0589"] <- "subday"

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
## First add a temporary temporal scale option with snapshot so that it is directly comparable to temporal resolution
data$ScaleTemporalSnapshot   <- factor(data$Scale...Temporal, levels = c("snapshot/no repeated sampling", "subday", "day", "week", "two week", "month", "two month", 
                                                                         "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"), ordered = TRUE)
idx                          <- which(data$ScaleTemporalSnapshot < data$ResTemporal)
datTempIssue                 <- data[idx,]
unique(datTempIssue$SW.ID) #6 papers


# Correct papers manually

## SW4_0115

# Reported treatment duration from Table 1 in paper indicates that the temporal scale is subday, instead of day.
# Time between repeated trials is either one week in May/June or 3 months compared to the one in Aug/Sept.
# So temporal resolution on average one month
data$Scale...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "subday"
data$ScaleTemporal[data$SW.ID %in% "SW4_0115"]                            <- "subday"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "subday"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "month"
data$ResTemporal[data$SW.ID %in% "SW4_0115"]                                   <- "month"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "month"


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
# - Injury and short-term mortality (4-5h after catching): temporal scale is thus subday and not halfyear. 
#   Experiments were done in two groups between Feb and Oct, but it is unclear what the time is between the two groups
#   or between repeated observations. But it could well be that there was on average a month in between, so keep that as resolution.
data$Scale...Temporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "half year"]                                     <- "subday"
data$ScaleTemporal[data$SW.ID %in% "SW4_1224" & data$Scale...Temporal %in% "half year"]                                        <- "subday"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1224" & data_allScreened$Scale...Temporal %in% "half year"] <- "subday"

# - Longer-term mortality after 10 days in tank: temporal scale of week is fine then, temporal resolution is probably again
#   one month, so can be kept as it is.


## SW4_1320
# Paper states there was 24h between control and impact sampling, so temporal scale is day, as already reported.
# It doesn't state it clearly but replicate samples seems to have been taken on the same day. So temporal
# resolution is subday, instead of month. This also makes sense in terms of inference by the authors.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1320"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_1320"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1320"] <- "subday"


# Check again whether there are rows where temporal resolution is larger than temporal extent
data$ScaleTemporalSnapshot   <- factor(data$Scale...Temporal, levels = c("snapshot/no repeated sampling", "subday", "day", "week", "two week", "month", "two month", 
                                                                         "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"), ordered = TRUE)
idx             <- which(data$ScaleTemporalSnapshot < data$ResTemporal)
datTempIssue    <- data[idx,]
unique(datTempIssue$SW.ID) #two left



### Temporal extent very large and temporal resolution very small ----
datTempIssue    <- subset(data, ScaleTemporal > "five year" & ResTemporal %in% c("subday","day","week")) #snapshots are OK in combination with large temporal scale
unique(datTempIssue$SW.ID) #16 papers


# Correct papers manually

## SW4_0199
# Temporal scale is indeed decade yet, with most areas between 2006-2009 and one area where they did a second round from 2015-2017.
# So temporal resolution is around 8.5 years, so five years.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0199"]                         <- "five year"
data$ResTemporal[data$SW.ID %in% "SW4_0199"]                                   <- "five year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0199"] <- "five year"

## Sw4_0272
# Temporal scale is indeed multidecadal. Temporal resolution is indeed subday, as time step in model was 1200 sec.

## Sw4_0365
# Paper looked at VMS records across 10 year period, so decadal scale is correct. Although VMS pings are indeed at subday resolution, inference is made
# across the entire 10 years period, as the authors also write themselves that they base their analysis on a "contemporary snapshot (last 10 years).
# So let's change resolution to snapshot.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0365"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0365"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0365"] <- "snapshot/no repeated sampling"

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
# Multiple studies are reported here. One of them is based on two cruises performed within two months time, so add a
# row where temporal scale is two months and temporal resolution is one month (i.e. time between cruises), although
# it is not clear whether one cruise only sampled one zone, and the other cruise only sampled the other, or whether both
# cruises visited both zones.
newRow     <- data[data$SW.ID %in% "SW4_0592",]
newRow_all <- data_allScreened[data_allScreened$SW.ID %in% "SW4_0592",]

newRow$Scale...Temporal[newRow$SW.ID %in% "SW4_0592"]                         <- "two month"
newRow$ScaleTemporal[newRow$SW.ID %in% "SW4_0592"]                            <- "two month"
newRow_all$Scale...Temporal[newRow_all$SW.ID %in% "SW4_0592"]                 <- "two month"

newRow$Resolution...Temporal[newRow$SW.ID %in% "SW4_0592"]                    <- "month"
newRow$ResTemporal[newRow$SW.ID %in% "SW4_0592"]                              <- "month"
newRow_all$Resolution...Temporal[newRow_all$SW.ID %in% "SW4_0592"]            <- "month"

# A second study uses VMS data from 2005-2013 = 8 years so temporal scale of 'five year', to estimate overall resuspension.
# Although VMS data are at subday resolution, the inference is done on a seasonal basis, so let's take this as 'half year'.
# Also ensure data are marked as fisheries dependent data
data$Scale...Temporal[data$SW.ID %in% "SW4_0592"]                         <- "five year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0592"]                            <- "five year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0592"] <- "five year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0592"]                         <- "half year"
data$ResTemporal[data$SW.ID %in% "SW4_0592"]                                   <- "half year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0592"] <- "half year"

data$Sampling.Method.used.for.data.collection[data$SW.ID %in% "SW4_0592"]                         <- "Fisheries Dependent Data"
data_allScreened$Sampling.Method.used.for.data.collection[data_allScreened$SW.ID %in% "SW4_0592"] <- "Fisheries Dependent Data"

data$Study.type[data$SW.ID %in% "SW4_0592"]                         <- "Fisheries dependent survey"
data_allScreened$Study.type[data_allScreened$SW.ID %in% "SW4_0592"] <- "Fisheries dependent survey"

# Combine new row to main data
data                <- rbind(data, newRow)
data_allScreened    <- rbind(data_allScreened, newRow_all)

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
# Fishing pressure data span one decade, but benthos sampling (i.e. the response) was done in July & August in one year. It is unclear when the 
# 14 samples were taken exactly, only that three replicates were taken per station. Likely replicates has been taken on the
# same day, but multiple cruises with perhaps one or weeks in between them, so let's take 'week' as temporal resolution
data$Scale...Temporal[data$SW.ID %in% "SW4_1199"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_1199"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1199"] <- "two month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1199"]                         <- "week"
data$ResTemporal[data$SW.ID %in% "SW4_1199"]                                   <- "week"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1199"] <- "week"

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
# Check again missing values ----
#-----------------------------------------------#

datNA <- data[is.na(data$Scale...Spatial..m.) | is.na(data$Scale...Temporal) | is.na(data$Resolution...Spatial..m.) | is.na(data$Resolution...Temporal),]
length(unique(datNA$SW.ID)) #45 papers
unique(datNA$SW.ID)

# Check manually

## SW4_0013
# For data used (logbook and observer) not clear what spatial resolution is. But inference seems to be done at the scale of the entire area.
# Model predictions are done at scale of fishing areas, of which the centres are 5,000-10,000 m apart, looking at the map in the paper.
# Map also shows that spatial scale is 10,000-50,000 m rather than 5,000-10,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0013"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0013"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0013"] <- "10,000-50,000"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0013"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_0013"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0013"] <- "5,000-10,000"

## SW4_0098
# Paper states that model was developed for period 2010-2014, so temporal scale is five year. 
# Model provides snapshot so that is the resolution.
data$Scale...Temporal[data$SW.ID %in% "SW4_0098"]                         <- "five year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0098"]                            <- "five year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0098"] <- "five year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0098"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0098"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0098"] <- "snapshot/no repeated sampling"

## SW4_0100
# Experiment done in small tanks, so spatial scale is 0-5 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0100"]                         <- "0-5"
data$ScaleSpatial[data$SW.ID %in% "SW4_0100"]                                <- "0-5"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0100"] <- "0-5"

## SW4_0193
# Analysis does not take any spatial aspects into account, but it done for the overall population that spans northwest Europe.
# So spatial scale is indeed >100,000 m. Let's take resolution than also at this distance. 
# Predictions are made for multiple decades by year. So temporal resolution is year.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0193"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0193"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0193"] <- ">100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0193"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0193"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0193"] <- "year"

## Sw4_0251
# Spatial scale around 2 km based on map of study area, so 1,000-5,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0251"]                         <- "1,000-5,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_0251"]                                <- "1,000-5,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0251"] <- "1,000-5,000"

## Sw4_0272
# 1-D model that goes up to 100 m depth, but not horizontal. Otherwise model and inference represents entire regional sea, spatial 
# scale is correct. Have this also as the resolution then.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0272"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0272"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0272"] <- ">100,000"

## SW4_0303
# Location between areas where trial were done seem to be >100 km apart, so take this as spatial resolution.
# Trials done in 2 years with around 1 year in between, so temporal scale and resolution are two year and year respectively.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0303"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0303"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0303"] <- ">100,000"

data$Scale...Temporal[data$SW.ID %in% "SW4_0303"]                         <- "two year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0303"]                            <- "two year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0303"] <- "two year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0303"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0303"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0303"] <- "year"

## Sw4_0330
# Temporal resolution is year.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0330"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0330"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0330"] <- "year"

## SW4_0355
# Trawling time was 45 minutes, so could well be that trawling distance was around 1-5 km. 
# But nowhere mentioned what the distance trawled was nor the total area size where experimental trawling took place.
# So leave spatial scale and resolution blank.

## Sw4_0359
# For the impact of Posidonia beds, mean hours of fishing per year per seabed habitat type were assessed. VMS data were used for that
# from 2007-2015, so temporal scale of a decade, and temporal resolution of one year.
# For seabottom impacts, sensor recordings were used from a selection of fishing hauls from a database with recordings from 2004 to 2013.
# So scale is decade but the resolution is unknown.
data$Scale...Temporal[data$SW.ID %in% "SW4_0359"]                         <- "decade"
data$ScaleTemporal[data$SW.ID %in% "SW4_0359"]                            <- "decade"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0359"] <- "decade"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0359" & data$Ecosystem.component_level1 %in% "Plants"]                                     <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0359" & data$Ecosystem.component_level1 %in% "Plants"]                                               <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0359" & data_allScreened$Ecosystem.component_level1 %in% "Plants"] <- "year"

## SW4_0402
# Spatial scale is indeed Ionian Sea, thus very large, but it is not mentioned/presented where the eight trawls within this large area were taken.
# Spatial resolution is thus unknown.

## SW4_0429
# Experiment performed in small tanks, so both scale and resolution 0-5 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0429"]                         <- "0-5"
data$ScaleSpatial[data$SW.ID %in% "SW4_0429"]                                <- "0-5"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0429"] <- "0-5"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0429"]                         <- "0-5"
data$ResSpatial[data$SW.ID %in% "SW4_0429"]                                       <- "0-5"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0429"] <- "0-5"

## SW4_0516
# Spatial resolution is large due to strandings occurring across entire coastline of Sicily, so 50,000-100,000 m.
# Although observations could have been ranging from subday to multiple years, the data are presents on a year and on a six-year period basis.
# Let's take five year as resolution.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0516"]                         <- "50,000-100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0516"]                                       <- "50,000-100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0516"] <- "50,000-100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0516"]                         <- "five year"
data$ResTemporal[data$SW.ID %in% "SW4_0516"]                                   <- "five year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0516"] <- "five year"

## SW4_0611
# Experiment performed in small tanks, so both scale and resolution 0-5 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_0611"]                         <- "0-5"
data$ScaleSpatial[data$SW.ID %in% "SW4_0611"]                                <- "0-5"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0611"] <- "0-5"

data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0611"]                         <- "0-5"
data$ResSpatial[data$SW.ID %in% "SW4_0611"]                                       <- "0-5"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0611"] <- "0-5"

## SW4_0665
# Add temporal resolution of month just as for the other rows (as it is the same observations, just from another area).
data$Scale...Temporal[data$SW.ID %in% "SW4_0665"]                         <- "month"
data$ScaleTemporal[data$SW.ID %in% "SW4_0665"]                            <- "month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0665"] <- "month"

## SW4_0679
# No information on the exact location of hauls is given, so spatial resolution is indeed unknown.

## SW4_0943
# EwE model without spatial component, so indeed no spatial resolution. Yet, interpretation of results is basin-wide, so let's say >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0943"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0943"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0943"] <- ">100,000"

## SW4_0955
# Model simulations with input from bottom trawl survey data. Model simulations do not contain spatial component but interpretation
# is done at scale of the entire basin. So resolution is >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0955"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0955"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0955"] <- ">100,000"

## SW4_0961
# Fisheries dependent data were collected to derive to an annual index of fishing effort by three fishing grounds.
# Take distance between these fishing grounds as spatial resolution, so >100,000
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_0961" & data$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"]                                     <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_0961" & data$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"]                                                   <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_0961" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"] <- ">100,000"

##SW4_1182
# Study includes many variables from many different sources for which the spatial resolution is not provided.
# Results are interpreted at the basin level, so let's take resolution as >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1182"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1182"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1182"] <- ">100,000"

## SW4_1233
# Temporal resolution of CPR is monthly. Temporal resolution of stock assessment data is year.
# No spatial component is included in the analysis, but results are interpreted at the basin scale, so spatial resolution is >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1233"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1233"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1233"] <- ">100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1233" & data$Description.Other.Sampling.Method %in% "Continuous Plankton Recorder"]                                     <- "month"
data$ResTemporal[data$SW.ID %in% "SW4_1233" & data$Description.Other.Sampling.Method %in% "Continuous Plankton Recorder"]                                               <- "month"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1233" & data_allScreened$Description.Other.Sampling.Method %in% "Continuous Plankton Recorder"] <- "month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1233" & data$Description.Other.Sampling.Method %in% "ICES stock assessment database"]                                     <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_1233" & data$Description.Other.Sampling.Method %in% "ICES stock assessment database"]                                               <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1233" & data_allScreened$Description.Other.Sampling.Method %in% "ICES stock assessment database"] <- "year"

## SW4_1238
# Paper does not mention when the tows exactly have been taken, so temporal scale is unknown. 

## SW4_1271
# Hauls were collected during several cruises between 1991 and 2007. So let's take five year as temporal resolution.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1271"]                                     <- "five year"
data$ResTemporal[data$SW.ID %in% "SW4_1271"]                                               <- "five year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1271"]             <- "five year"

## SW4_1332
# No information provided on the exact location of trawl hauls, other than it being in the eastern Ionian Sea.
# So spatial resolution is unknown.

## SW4_1382
# EwE model where it is unclear what the spatial and resolution is. No spatial component to the model, but inference made at the level of the
# entire region, so let's take spatial resolution of >100,000 m. Temporal scale is a decade but no comparison made with other decades. Keep it blank.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1382"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1382"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1382"] <- ">100,000"

## Sw4_1394
# Model based on two stations that are >100,000 m apart, so take that as spatial resolution.
# Stations were sampled in two years, and model inference is also done at the year level (e.g. recovery times).
# So take year as temporal resolution.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1394"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1394"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1394"] <- ">100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1394"]                                     <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_1394"]                                               <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1394"]             <- "year"

## SW4_1457
# Although information on gillnet location was collected, this is not presented in the paper.
# Spatial resolution is therefore unknown.

## SW4_1469
# Size of fishing ground is provided in map without scale. Measured distance from Google maps indicates spatial scale to be 50,000-100,000 m.
# Location of actual hauls is not provided, so spatial resolution is unknown.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1469"]                         <- "50,000-100,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1469"]                                <- "50,000-100,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1469"] <- "50,000-100,000"

## SW4_1528
# No map of the actual hauls used from the IBTS are provided, but sampling protocol is taken 1-2 hauls per rectangle.
# So let's take the width of a rectangle as the spatial resolution, being between 50,000-100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1528"]                         <- "50,000-100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1528"]                                       <- "50,000-100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1528"] <- "50,000-100,000"

## SW4_1531
# EwE model without spatial component, so indeed no spatial resolution. Yet, interpretation of results is basin-wide, so let's say >100,000 m.
# They explored impact of a management measure before and after a certain year, with 10-20 years of data, but ran simulations and grouped
# survey data in bins of 5 years. So take five years as temporal resolution.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1531"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1531"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1531"] <- ">100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1531"]                         <- "five year"
data$ResTemporal[data$SW.ID %in% "SW4_1531"]                                   <- "five year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1531"] <- "five year"

## SW4_1662
# Model without spatial component, but some inferences are made in the discussion on a wider area (e.g. softbottom habitats in North Sea).
# So let's take distance from spatial scale as resolution: 100,000 m.
# No clear temporal component in model, other than that they look at trawling frequency per hour but also per year. Some other variables are in day
# Therefore leave temporal resolution blank as this is not clear and homogeneous. Temporal scale is in the same way not clear either.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1662"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1662"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1662"] <- ">100,000"

## SW4_1663
# For the visual diving surveys, number of surveys per year varied between 4 and 20. Let's take one month as temporal resolution.
# Data one size structure were taken from three tows but unclear when they were taken, and neither how far apart they were with data from the
# compared tow from 2000. So leave temporal resolution blank.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1663" & data$Scale...Temporal %in% "decade"]                                     <- "month"
data$ResTemporal[data$SW.ID %in% "SW4_1663" & data$Scale...Temporal %in% "decade"]                                               <- "month"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1663" & data_allScreened$Scale...Temporal %in% "decade"] <- "month"

## SW4_1684
# Spatial resolution estimated from the map to be 10,000-50,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1684"]                         <- "10,000-50,000"
data$ResSpatial[data$SW.ID %in% "SW4_1684"]                                       <- "10,000-50,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1684"] <- "10,000-50,000"

## SW4_1689
# Total study area is presented, but not the exact location of observations. Spatial resolution is therefore unknown.

## SW4_1707
# Global meta-analysis, with data from Europe, North America and Oceania. Let's therefore take the same large distance for spatial scale
# as for resolution: 100,000 m.
# Studies range from 1983 to 1998, so temporal scale is multi-decadal. Temporal resolution is unknown.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1707"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1707"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1707"] <- ">100,000"

data$Scale...Temporal[data$SW.ID %in% "SW4_1707"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_1707"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1707"] <- "multidecadal"

## SW4_1719
# Repeated observations were taken either daily or or two weeks, depending on the treatment. So take week as temporal resolution (as middle ground).
# Paper does not provide exact locations of the plots neither at which reef (and size of the reef) measurements were done. So spatial resolution unknown.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1719"]                         <- "two week"
data$ResTemporal[data$SW.ID %in% "SW4_1719"]                                   <- "two week"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1719"] <- "two week"

## SW4_1727
# Paper does not mention in which year(s?) trap experiments were done, but the months are provided, which are
# spread out through the year: Feb to Sep. Let's assume they were done in the same year, with on average one month 
# in between them = temporal resolution.
# Temporal scale should be the duration of the experiment which is two days, so 'day'
data$Scale...Temporal[data$SW.ID %in% "SW4_1727"]                         <- "day"
data$ScaleTemporal[data$SW.ID %in% "SW4_1727"]                            <- "day"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1727"] <- "day"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1727"]                         <- "month"
data$ResTemporal[data$SW.ID %in% "SW4_1727"]                                   <- "month"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1727"] <- "month"

## SW4_1788
# Based on 54 interviews conducted in one month at two different locations. Time between interviews unknown,
# but likely in the order of days or week. Choose 'week' as temporal resolution
data$Resolution...Temporal[data$SW.ID %in% "SW4_1788"]                         <- "week"
data$ResTemporal[data$SW.ID %in% "SW4_1788"]                                   <- "week"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1788"] <- "week"

## SW4_1803
# Distance between hauls within experimental areas is unknown, but distance between the two experimental areas,
# based on map, is 5,000-10,000 m, so take that as spatial resolution.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1803"]                         <- "5,000-10,000"
data$ResSpatial[data$SW.ID %in% "SW4_1803"]                                       <- "5,000-10,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1803"] <- "5,000-10,000"

## Sw4_1833
# Measure peninsula on Google maps, spatial scale is 5,000-10,000 m. Spatial resolution is unknown, other than it is smaller than the spatial scale.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1833"]                         <- "5,000-10,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1833"]                                <- "5,000-10,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1833"] <- "5,000-10,000"

## SW4_1867
# The coordinates are provided and with help from Google maps, stations are >100,000 m apart, so this is the spatial resolution.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1867"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1867"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1867"] <- ">100,000"

## SW4_1942
# Paper describes that some information has been collected weekly, presumably throughout the year - other information annually 
# within the second quarter, likely with higher interval, e.g. weekly or so. Take temporal resolution as week.
# Data source is market sampling where samples can come from all over the fishing distributional area. So spatial resolution is unknown.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1942"]                         <- "two week"
data$ResTemporal[data$SW.ID %in% "SW4_1942"]                                   <- "two week"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1942"] <- "two week"

## SW4_1952
# EwE model without spatial component, but inference done at the scale of entire basin, so take >100,000 m as spatial resolution.
# Model developed for period 2004-2005, where underlying data were averaged across years. So temporal resolution is snapshot.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1952"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1952"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1952"] <- ">100,000"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1952"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1952"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1952"] <- "snapshot/no repeated sampling"

## SW4_1953
# Inference done by comparing the three areas, not hauls within each area. So take distance between areas as spatial resolution,
# which is based on map >100,000 m.
data$Resolution...Spatial..m.[data$SW.ID %in% "SW4_1953"]                         <- ">100,000"
data$ResSpatial[data$SW.ID %in% "SW4_1953"]                                       <- ">100,000"
data_allScreened$Resolution...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1953"] <- ">100,000"

## SW4_1994
# Based on map, spatial scale is 10,000-50,000 m. Exact location of sampling not provided, so spatial resolution is unknown.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1994"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1994"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1994"] <- "10,000-50,000"

# Check how many still are missing
datNA <- data[is.na(data$Scale...Spatial..m.) | is.na(data$Scale...Temporal) | is.na(data$Resolution...Spatial..m.) | is.na(data$Resolution...Temporal),]
length(unique(datNA$SW.ID)) #17 papers
unique(datNA$SW.ID)

# Rename remaining NAs as 'Not specified'
data$Scale...Spatial..m.[is.na(data$Scale...Spatial..m.)]             <- "Not specified"
data$Scale...Temporal[is.na(data$Scale...Temporal)]                   <- "Not specified"
data$Resolution...Spatial..m.[is.na(data$Resolution...Spatial..m.)]   <- "Not specified"
data$Resolution...Temporal[is.na(data$Resolution...Temporal)]         <- "Not specified"
 
data_allScreened$Scale...Spatial..m.[is.na(data_allScreened$Scale...Spatial..m.) & is.na(data_allScreened$Exclusion.Criteria)]             <- "Not specified"
data_allScreened$Scale...Temporal[is.na(data_allScreened$Scale...Temporal) & is.na(data_allScreened$Exclusion.Criteria)]                   <- "Not specified"
data_allScreened$Resolution...Spatial..m.[is.na(data_allScreened$Resolution...Spatial..m.) & is.na(data_allScreened$Exclusion.Criteria)]   <- "Not specified"
data_allScreened$Resolution...Temporal[is.na(data_allScreened$Resolution...Temporal) & is.na(data_allScreened$Exclusion.Criteria)]         <- "Not specified"



#-----------------------------------------------#
# Temporal extent equal to temporal resolution ----
#-----------------------------------------------#

# First add a temporary temporal scale option with snapshot so that it is directly comparable to temporal resolution
data$ScaleTemporalSnapshot   <- factor(data$Scale...Temporal, levels = c("snapshot/no repeated sampling", "subday", "day", "week", "two week", "month", "two month", 
                                                                         "quarter", "half year", "year", "two year", "five year", "decade", "multidecadal"), ordered = TRUE)
idx             <- which(data$ScaleTemporalSnapshot == data$ResTemporal)
datTempIssue    <- data[idx,]
length(unique(datTempIssue$SW.ID)) #45 papers
unique(datTempIssue$SW.ID)


## SW4_0054
# They use beam trawl swept area ratio from 2009-2010, and pulse trawl data from 2016-2018 - all based on VMS data.
# Several vessels that were active during that time were selected. In the end, results are presented without any
# temporal effect - only the variation between vessels is quantified - so a snapshot as temporal resolution.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0054"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0054"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0054"] <- "snapshot/no repeated sampling"

## SW4_0059
# Model of scallop abundance includes multiple fishing seasons and also predicts the abundance per season per year.
# So temporal resolution is year.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0059"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0059"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0059"] <- "year"

## SW4_0064
# Multiple samples were taken on the same day, so temporal resolution should be 'subday' rather than 'day'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0064"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0064"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0064"] <- "subday"

## SW4_0190
# Modelling done at the level of year, so temporal resolution is 'year' rather than 'multidecadal'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0190"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0190"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0190"] <- "year"

## SW4_0251
# Time between two treatments is about a week, so in a way, the temporal resolution could also
# be seen as 'week'. However, each treatment lasted one day with multiple samples taken.
# So keep temporal scale as 'week' and change resolution from 'week' to 'subday'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0251"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0251"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0251"] <- "subday"

## SW4_0284
# Three sample were taken in 2015 and three in 2016, with each set of samples taken on the same day.
# So temporal scale is indeed a year. In the analysis, all samples are taken as repeated observations.
# As the between sample is either subday or year, the median would then be the average of the middle two values,
# so 'half year'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0284"]                         <- "half year"
data$ResTemporal[data$SW.ID %in% "SW4_0284"]                                   <- "half year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0284"] <- "half year"

## SW4_0396
# Sampling took place on 5 days in May and 5 days in December, so temporal scale should be 'half year' instead of 'two month'.
# Analysis looked at both differences within a day (morning vs. afternoon) and between months, so temporal resolution
# in the analysis is both subday and 'half year'. Main outcome of the paper is the total estimate of discard that is
# consumed annually, and in the discussion, the focus is more on this as well as on the seasonally differences rather than
# difference between morning and afternoon. So in that sense, we set temporal resolution also to 'half year'. 
data$Scale...Temporal[data$SW.ID %in% "SW4_0396"]                         <- "half year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0396"]                            <- "half year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0396"] <- "half year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0396"]                         <- "half year"
data$ResTemporal[data$SW.ID %in% "SW4_0396"]                                   <- "half year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0396"] <- "half year"

## SW4_0476
# Sampling took place over six days, with majority of hauls taken on the same day. So change temporal scale
# from 'subday' to week and leave temporal resolution as 'subday'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0476"]                         <- "week"
data$ScaleTemporal[data$SW.ID %in% "SW4_0476"]                            <- "week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0476"] <- "week"

## SW4_0493
# The fisheries dependent data refers to the VMS data, but these are only used to determine the pressure - not the response.
# Remove these rows.
# Regarding sampling the response, all sampling was done in one month, so that is the temporal scale.
# Unclear what the time is between hauls within that month, but it is likely in the order of one day or a few days.
# So take 'day' as temporal resolution
data               <- subset(data, !(SW.ID %in% "SW4_0493" & Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"))
data_allScreened   <- subset(data_allScreened, !(SW.ID %in% "SW4_0493" & Sampling.Method.used.for.data.collection %in% "Fisheries Dependent Data"))

data$Scale...Temporal[data$SW.ID %in% "SW4_0493"]                         <- "month"
data$ScaleTemporal[data$SW.ID %in% "SW4_0493"]                            <- "month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0493"] <- "month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0493"]                         <- "day"
data$ResTemporal[data$SW.ID %in% "SW4_0493"]                                   <- "day"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0493"] <- "day"

## SW4_0620
# Paper uses data on incidentally caught turtles during fishing operations from 2001-2012, but no temporal
# patterns are studied - they specifically tested if time would have a significant effect on the results, and
# it didn't - so data from all years were aggregated. 
# Temporal resolution has been coded as decade (like the scale), but Suppl Info shows that  multiple turtles were
# caught per year for each region. Therefore let's take month as temporal resolution
data$Resolution...Temporal[data$SW.ID %in% "SW4_0620"]                         <- "month"
data$ResTemporal[data$SW.ID %in% "SW4_0620"]                                   <- "month"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0620"] <- "month"

## SW4_0624
# The exact time period of benthos sampling is unclear - they only say samples were available from 1994 onwards.
# Study is from 2016, so it assumed that the sampling is multidecadal rather than 'five year'. The VMS data were however
# from a five year period, but key thing here is the benthos sampling. So temporal scale is multidecadal.
# Benthos samples are taken annually, but in the analysis, samples from all years are grouped. So inference is done as
# a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0624"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0624"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0624"] <- "multidecadal"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0624"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0624"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0624"] <- "snapshot/no repeated sampling"

## SW4_0633
# The different datasets used lie 10 years apart, so the temporal scale should be 'decade' rather than 'year'.
# Observations are >100 fishing operations within 2 years' time, so temporal resolution could be 'week'.
# Yet, in the analysis, all data are combined and no time effect is studied. So inference is like a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0633"]                         <- "decade"
data$ScaleTemporal[data$SW.ID %in% "SW4_0633"]                            <- "decade"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0633"] <- "decade"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0633"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0633"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0633"] <- "snapshot/no repeated sampling"

## SW4_0638
# Temporal scale is indeed multidecadal but data are modelled and results presented by year, so
# temporal resolution should be 'year' instead of 'multidecadal'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0638"]                         <- "year"
data$ResTemporal[data$SW.ID %in% "SW4_0638"]                                   <- "year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0638"] <- "year"

## SW4_0689
# Data from 2002 and 2011 are also compared to the 1970s. So keep temporal resolution of decadal,
# but change temporal scale from 'decadal' to 'multidecadal'.
data$Scale...Temporal[data$SW.ID %in% "SW4_0689"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0689"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0689"] <- "multidecadal"

## SW4_0702
# Keep temporal scale and resolution at 'five year', because they compare benthic communities between
# 2007 and 2011.

## SW4_0738
# Temporal scale and resolution are indeed both multidecadal due to >60 years of data analysed in
# blocks of two decades.

## SW4_0760
# Temporal scale and resolution are indeed both multidecadal as they compare samples from 1983
# with samples from 2012.

## SW4_0793
# Samples are taken in May and October in the same year, so temporal scale is indeed 'half year'.
# Samples were taken on the same day in either May or October. So resolution is either subday or half year.
# As the samples are analysed combinedly without looking at temporal effects, so inference is done as a 'snapshot'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0793"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0793"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0793"] <- "snapshot/no repeated sampling"

## SW4_0798
# Model is built so simulate ecosystem from 1985 to 2008, so temporal scale is 'multidecadal' instead of 'year'.
# Temporal resolution can remain as 'year', as results are predicted by year.
data$Scale...Temporal[data$SW.ID %in% "SW4_0798"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_0798"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0798"] <- "multidecadal"

## SW4_0995
# Samples were indeed taken in a month's time, so temporal scale of 'month' is correct. 
# How many hauls were conducted within the sampling month is unknown, but it is of course less than a month, so could be days or weeks.
# They are all treated similarly without accounting for temporal effects, so inference is as a 'snapshot'. 
# Therefore, let's go for 'week' as temporal resolution.
data$Resolution...Temporal[data$SW.ID %in% "SW4_0995"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_0995"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0995"] <- "snapshot/no repeated sampling"

## SW4_1034
# Samples were taken between June 2010 and January 2011, so temporal scale is 'half year' instead of 'year'.
# Within areas, sampling occurred usually on the same day or the day after, but temporal effects are not assessed, 
# so inference is as a 'snapshot'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1034"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1034"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1034"] <- "snapshot/no repeated sampling"

## SW4_1164
# Sampling occurred during 59 survey days from June to September 2007, so temporal scale is 'two month' instead of 'year'.
# On average sampling probably took place every two days, but temporal effects are not assessed however, 
# so inference done as a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_1164"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_1164"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1164"] <- "two month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1164"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1164"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1164"] <- "snapshot/no repeated sampling"

## SW4_1184
# Sampling was done within two weeks time, so temporal scale is 'two weeks' instead of 'year'.
# Exact sampling days not known, but considering that 27 stations were sampled four times, repeated
# observations took probably place on the same day.
# But temporal effects are not assessed, so inference done as a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_1184"]                         <- "two week"
data$ScaleTemporal[data$SW.ID %in% "SW4_1184"]                            <- "two week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1184"] <- "two week"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1184"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1184"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1184"] <- "snapshot/no repeated sampling"

## SW4_1208
# Sampling was done within one months' time, so temporal scale is 'month' instead of 'year'.
# Sampling occurred every few days, but temporal effects are not assessed, so inference done as a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_1208"]                         <- "month"
data$ScaleTemporal[data$SW.ID %in% "SW4_1208"]                            <- "month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1208"] <- "month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1208"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1208"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1208"] <- "snapshot/no repeated sampling"

## SW4_1246
# Samples collected between 1990 and 2008, so temporal scale is 'multidecadal' instead of 'decadal'.
# 120 samples were collected in 18 years' time, so on average one every two months, but no temporal effects assessed, 
# so inference is as a 'snapshot'.
data$Scale...Temporal[data$SW.ID %in% "SW4_1246"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_1246"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1246"] <- "multidecadal"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1246"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1246"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1246"] <- "snapshot/no repeated sampling"

## SW4_1264
# Sampling on average with or 1-2 days in between, but no temporal effects assessed, so inference is as a 'snapshot'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1264"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1264"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1264"] <- "snapshot/no repeated sampling"

## SW4_1321
# Samples were collected with over a year in between, so temporal scale of 'year' is fine. Main effect studied
# is however seasonal, as winter and summer are compared rather than year effects. So set temporal resolution
# as 'half year' instead of 'year'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1321"]                         <- "half year"
data$ResTemporal[data$SW.ID %in% "SW4_1321"]                                   <- "half year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1321"] <- "half year"

## SW4_1375
# Data span almost a century and some of the periods compare lie multiple decades apart.
# So correct that both temporal scale and resolution are 'multidecadal'.

## SW4_1445
# Approximately one year between first and last sample, so temporal scale should be 'year' instead of 'five year'.
# No temporal effects are assessed, so 'snapshot' as inference.
data$Scale...Temporal[data$SW.ID %in% "SW4_1445"]                         <- "year"
data$ScaleTemporal[data$SW.ID %in% "SW4_1445"]                            <- "year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1445"] <- "year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1445"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1445"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1445"] <- "snapshot/no repeated sampling"

## SW4_1477
# Model run for 10 years, so temporal scale should be 'decade' instead of 'year'.
data$Scale...Temporal[data$SW.ID %in% "SW4_1477"]                         <- "decade"
data$ScaleTemporal[data$SW.ID %in% "SW4_1477"]                            <- "decade"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1477"] <- "decade"

## SW4_1478
# Temporal scale is one decade instead of year, as temporal simulation runs and data input range from 1994 to 2003.
data$Scale...Temporal[data$SW.ID %in% "SW4_1478"]                         <- "decade"
data$ScaleTemporal[data$SW.ID %in% "SW4_1478"]                            <- "decade"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1478"] <- "decade"

## SW4_1502
# Data collected during 1981-2000, so temporal scale is indeed multidecadal. It is not reported
# what the time is between observations, but these are likely highly variable as they are based on recapture.
# 105 captures occurred in 19 years' time, so on average 5-6 per year. But, no trends or temporal effects are assessed, 
# so inference is as a 'snapshot'.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1502"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1502"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1502"] <- "snapshot/no repeated sampling"

## SW4_1531
# Temporal scale is multiple decades rather than five years, so change this for one row where this is the case.
data$Scale...Temporal[data$SW.ID %in% "SW4_1531"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_1531"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1531"] <- "multidecadal"

## SW4_1560
# Input data is taken from only one year, so that is the correct temporal scale. However, most results
# presented as snapshot, so temporal resolution should thus be 'snapshot/no repeated sampling'.
# The exception are time series of mean trophic level: these should have multidecadal as scale, and
# resolution can remain to be year.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1560" & !(data$Response.variable_paper %in% "Trophic level of fisheries")] <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1560" & !(data$Response.variable_paper %in% "Trophic level of fisheries")]           <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1560" & !(data_allScreened$Response.variable_paper %in% "Trophic level of fisheries")] <- "snapshot/no repeated sampling"

data$Scale...Temporal[data$SW.ID %in% "SW4_1560" & data$Response.variable_paper %in% "Trophic level of fisheries"]                         <- "multidecadal"
data$ScaleTemporal[data$SW.ID %in% "SW4_1560" & data$Response.variable_paper %in% "Trophic level of fisheries"]                            <- "multidecadal"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1560" & data_allScreened$Response.variable_paper %in% "Trophic level of fisheries"] <- "multidecadal"

## SW4_1566
# For interviews, temporal scale was from May-August = four months = rounded off 'two month'.
# 162 interviews over 4 months = likely 1-2 per day, probably more but perhaps with days without interviews in between.
# But inference as snapshot.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1566" & data$Study.type %in% "Questionnaire/interview"]                                     <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1566" & data$Study.type %in% "Questionnaire/interview"]                                               <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1566" & data_allScreened$Study.type %in% "Questionnaire/interview"] <- "snapshot/no repeated sampling"

# For onboard observers data collection, it is unknown when these took place, but likely within the same four months or longer,
# as they joined on 59 vessels. Inference as snapshot
data$Resolution...Temporal[data$SW.ID %in% "SW4_1566" & data$Study.type %in% "Fisheries dependent survey"]                                     <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1566" & data$Study.type %in% "Fisheries dependent survey"]                                               <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1566" & data_allScreened$Study.type %in% "Fisheries dependent survey"] <- "snapshot/no repeated sampling"

## SW4_1586
# Before After experiment where experiment duration is one year, so temporal scale of one year is correct.
# Sampling in each year happened within 2-3 months, with 6 transects being sampled three times.
# Temporal resolution could thus either be one year (time between Before and After, same as temporal scale) or
# days or weeks when considering repeated observations taken within each treatment. Inference is done at the level
# of year, so choose that. This means that temporal scale and resolution can remain as 'year'.

## SW4_1651
# Temporal scale of survey is not day, but decade, and resolution year and not day. But survey presented in blocks of five year each, so choose that
data$Scale...Temporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"]        <- "decade"
data$ScaleTemporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"]           <- "decade"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1651" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "decade"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "five year"
data$ResTemporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"]            <- "five year"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1651" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "five year"

# Temporal scale of discard trial is indeed one day. Inference as snapshot so choose that as temporal resolution
data$Resolution...Temporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Stomach Contents Analyses"]  <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1651" & data$Sampling.Method.used.for.data.collection %in% "Stomach Contents Analyses"]            <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1651" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Stomach Contents Analyses"] <- "snapshot/no repeated sampling"

## SW4_1684
# All samples were taken within two weeks' time in Sept 1998 and Sept 1999. Temporal scale is indeed year,
# but resolution can be either (sub)day because of the sampling within each year, or year because of the repeated
# sampling between years. The others present the results separately but also combined for the two years, and stress
# the variability between years. Therefore keep the temporal resolution as year.

## SW4_1687
# Samples were collected during Q3, so temporal scale is two months and not year. Samples were likely collected
# on the same day or with one or more days in between. Inference done at snapshot level.
data$Scale...Temporal[data$SW.ID %in% "SW4_1687"]                         <- "two month"
data$ScaleTemporal[data$SW.ID %in% "SW4_1687"]                            <- "two month"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1687"] <- "two month"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1687"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1687"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1687"] <- "snapshot/no repeated sampling"

## SW4_1710
# Both temporal scale and resolution are indeed 'five year', as there are 7 years between the first and second sampling event,
# and the authors infer the data based on the difference between the sampling events, despite multiple samples being taken
# at each event.

## SW4_1721
# Temporal scale and resolution are indeed year for the comparison of biodiversity from the trawl data.

## SW4_1811
# Samples were collected in 1993, 1994 and 1995, so temporal scale is indeed two year, but temporal resolution
# is year instead of two year. However, inference as snapshot, so choose that.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1811"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1811"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1811"] <- "snapshot/no repeated sampling"

## SW4_1837
# Temporal scale is indeed day, but temporal resolution is subday and not day, as multiple samples were taken.
# But, inference as snapshot so choose that.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1837"]                         <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1837"]                                   <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1837"] <- "snapshot/no repeated sampling"

## SW4_1961
# First change sampling method long-term monitoring program
data$Sampling.Method.used.for.data.collection[data$SW.ID %in% "SW4_1961" & data$Scale...Temporal %in% "five year"] <- "Regular Fisheries Independent Survey"
data_allScreened$Sampling.Method.used.for.data.collection[data_allScreened$SW.ID %in% "SW4_1961" & data_allScreened$Scale...Temporal %in% "five year"] <- "Regular Fisheries Independent Survey"

# Change temporal resolution of survey from month to snapshot as this is the level of inference, as annual effects are not assessed.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"]           <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1961" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Regular Fisheries Independent Survey"] <- "snapshot/no repeated sampling"

# Samples from 'snapshot' survey were collected during two weeks in 2016, so change temporal scale from month to two weeks.
# Multiple samples were taken on the same day, but inference done as if it was a snapshot, so choose that.
data$Scale...Temporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "two week"
data$ScaleTemporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"]           <- "two week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1961" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "two week"

data$Resolution...Temporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "snapshot/no repeated sampling"
data$ResTemporal[data$SW.ID %in% "SW4_1961" & data$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"]           <- "snapshot/no repeated sampling"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1961" & data_allScreened$Sampling.Method.used.for.data.collection %in% "Irregular Fisheries Independent Survey"] <- "snapshot/no repeated sampling"

## SW4_2033
# Temporal scale and resolution are indeed day.


## Note that temporal extent and scale can indeed be the same in case there are only two repeated sampling events:
# at the beginning and end of the temporal scale.

## ISSUE: for experiments, we should take duration of manipulation as temporal scale.
# However, the time between repeated experiments can be longer than this, fx when they need to visit multiple stations at sea.
# We earlier on said that resolution can never be larger than scale... So how to deal with this?
# Example is SW4_1727
## DECISION: we should allow this to be the case for experiments, as this is how we instructed the reviewers.
## CHECK again all papers for when temporal resolution is higher than scale.
## DONE.



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
## SpatioTemporal Resolution ----
#-----------------------------------------------#

# Make spatio-temperal resolution category matrix in long form
spatempRes_cat <- expand.grid(levels(data$ResSpatial),
                              levels(data$ResTemporal))

colnames(spatempRes_cat) <- c("SpatialRes_m", "TemporalRes")

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = data[!duplicated(data$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempRes_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialRes_m", "TemporalRes"),
                          all.x = TRUE)

spatempRes_count[is.na(spatempRes_count$NumberOfArticles), "NumberOfArticles"] <- 0

## Make counts of articles in different combinations of SPATIAL & TEMPORAL RESOLUTIONS
spatempRes_count <- aggregate(SW.ID~ResSpatial+ResTemporal,
                              data = data[!duplicated(data$SW.ID), ],
                              FUN = length)
names(spatempRes_count)[names(spatempRes_count) %in% "SW.ID"] <- "NumberOfArticles"


spatempRes_count <- merge(x = spatempRes_cat,
                          y = spatempRes_count,
                          by.y = c("ResSpatial", "ResTemporal"),
                          by.x = c("SpatialRes_m", "TemporalRes"),
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
                         mapping = aes(x = SpatialRes_m,
                                       y = TemporalRes,
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
# Check quality columns ----
#-----------------------------------------------#

# Check quality columns for NA values
table(data$Quality...Methods, useNA = "always") #11 NAs
table(data$Quality...Temporal, useNA = "always") #11 NAs
table(data$Quality...Spatial..relative.1.3., useNA = "always") #20 NAs

# Check quality NAs in more detail and add where needed
dat.qu.na                             <- subset(data_allScreened, is.na(Exclusion.Criteria))
dat.qu.na                             <- subset(dat.qu.na, is.na(Quality...Methods) | is.na(Quality...Temporal) | is.na(Quality...Spatial..relative.1.3.))

## SW4_0193
# Not of the three quality metrics are provided (resolution was missing too), so do here manually
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0193"]                           <- 2
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0193"]   <- 2

data$Quality...Temporal[data$SW.ID %in% "SW4_0193"]                           <- 2
data_allScreened$Quality...Temporal[data_allScreened$SW.ID %in% "SW4_0193"]   <- 2

data$Quality...Methods[data$SW.ID %in% "SW4_0193"]                           <- 3
data_allScreened$Quality...Methods[data_allScreened$SW.ID %in% "SW4_0193"]   <- 3

## SW4_0402
# Quality metrics not filled in, and spatial resolution unknown. Assign quality metrics manually here
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0402"]                           <- "Could not be defined"
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0402"]   <- "Could not be defined"

data$Quality...Temporal[data$SW.ID %in% "SW4_0402"]                           <- 2
data_allScreened$Quality...Temporal[data_allScreened$SW.ID %in% "SW4_0402"]   <- 2

data$Quality...Methods[data$SW.ID %in% "SW4_0402"]                           <- 3
data_allScreened$Quality...Methods[data_allScreened$SW.ID %in% "SW4_0402"]   <- 3

## SW4_0429
# Spatial quality was not filled in because spatial scale and resolution was missing -> add manually here
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0429"]                           <- 2
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0429"]   <- 2

## SW4_0465
# Quality not metrics not filled in -> add here manually
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0465"]                           <- 1
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0465"]   <- 1

data$Quality...Temporal[data$SW.ID %in% "SW4_0465"]                           <- 2
data_allScreened$Quality...Temporal[data_allScreened$SW.ID %in% "SW4_0465"]   <- 2

data$Quality...Methods[data$SW.ID %in% "SW4_0465"]                           <- 3
data_allScreened$Quality...Methods[data_allScreened$SW.ID %in% "SW4_0465"]   <- 3

## SW4_0537
# Temporal and method quality metrics missing -> add manually here
data$Quality...Temporal[data$SW.ID %in% "SW4_0537"]                           <- 2
data_allScreened$Quality...Temporal[data_allScreened$SW.ID %in% "SW4_0537"]   <- 2

data$Quality...Methods[data$SW.ID %in% "SW4_0537"]                           <- 3
data_allScreened$Quality...Methods[data_allScreened$SW.ID %in% "SW4_0537"]   <- 3

## SW4_0611
# Spatial quality metric not provided, probably because spatial scale and resolution were initially neither -> add spatial quality here
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0611"]                           <- 2
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0611"]   <- 2

## SW4_0978
# Was accidentally entered in the wrong column, so add here manually
data$Quality...Spatial..relative.1.3.[data$SW.ID %in% "SW4_0978"]                           <- 3
data_allScreened$Quality...Spatial..relative.1.3.[data_allScreened$SW.ID %in% "SW4_0978"]   <- 3

# Check if any NAs are still left
table(is.na(data$Quality...Methods))
table(is.na(data$Quality...Temporal))
table(is.na(data$Quality...Methods))

table(is.na(data_allScreened$Quality...Methods[is.na(data_allScreened$Exclusion.Criteria)]))
table(is.na(data_allScreened$Quality...Temporal[is.na(data_allScreened$Exclusion.Criteria)]))
table(is.na(data_allScreened$Quality...Methods[is.na(data_allScreened$Exclusion.Criteria)]))



#-----------------------------------------------#
# Save dataset ----
#-----------------------------------------------#

# Drop some columns to return to original columns
data$ScaleSpatial <- data$ResSpatial <- data$ScaleTemporal <- data$ResTemporal <- data$ROWID <- NULL

# Sort papers by SW.ID
data                   <- data[order(data$SW.ID),]
data_allScreened       <- data_allScreened[order(data_allScreened$SW.ID),]

# Add new ROWID
data$ROWID             <- c(1:nrow(data))
data_allScreened$ROWID <- c(1:nrow(data_allScreened))

# Save
saveRDS(data, paste0(datPath,"data_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))
write.xlsx(data, file=paste0(datPath, "data_correctTaxa_PressVar_RespVar_Methods_ScaleRes.xlsx"))

saveRDS(data_allScreened, paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))
write.xlsx(data_allScreened, file=paste0(datPath,"data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.xlsx"))
