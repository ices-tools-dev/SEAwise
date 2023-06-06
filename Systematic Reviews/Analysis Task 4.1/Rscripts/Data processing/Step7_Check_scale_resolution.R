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

# Identify rows where spatial resolution is larger than spatial extent
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



#-----------------------------------------------#
## Temporal scale vs. resolution ----
#-----------------------------------------------#

# Identify rows where temporal resolution is greater than temporal extent
idx             <- which(data$ScaleTemporal < data$ResTemporal)
datTempIssue    <- data[idx,]
unique(datTempIssue$SW.ID) #6 papers
#SW4_0115 is a lab experiment paper where the duration of the experiment (subday) is shorter than the time between control vs. impact treatments.
#Similarly, SW4_1224 has 10-day experiments with a month time in between repeated experiments.
#So it is possible that temporal resolution is greater than temporal scale for experiments.


# Correct papers manually

## SW4_0115

# Reported treatment duration from Table 1 in paper indicates that the temporal scale is subday, instead of day.
# Time between control and impact treatment is maximum 1 week, and not two weeks.
data$Scale...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "subday"
data$ScaleTemporal[data$SW.ID %in% "SW4_0115"]                            <- "subday"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "subday"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0115"]                         <- "week"
data$ResTemporal[data$SW.ID %in% "SW4_0115"]                                   <- "week"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0115"] <- "week"


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
# Paper doesn't give exact dates of sampling, only the month in which it took place. Stations are at least 10 km
# apart, but likely possible they did several on the same say, so choose 'subday' instead of 'month' as temporal resolution.
data$Scale...Temporal[data$SW.ID %in% "SW4_0829"]                         <- "half year"
data$ScaleTemporal[data$SW.ID %in% "SW4_0829"]                            <- "half year"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_0829"] <- "half year"

data$Resolution...Temporal[data$SW.ID %in% "SW4_0829"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_0829"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_0829"] <- "subday"


## SW4_1224

# Duration of tank experiments was 10 days, so temporal scale of week is fine.
# Time between experiments was likely a month, as the angling sessions + tank experiment were done from Feb to Oct.
# So temporal resolution of month is also fine.


## SW4_1320
# Paper states there was 24h between control and impact sampling, so temporal scale is day, as already reported.
# It doesn't state it clearly but replicate samples seems to have been taken on the same day. So temporal
# resolution is subday, instead of month.
data$Resolution...Temporal[data$SW.ID %in% "SW4_1320"]                         <- "subday"
data$ResTemporal[data$SW.ID %in% "SW4_1320"]                                   <- "subday"
data_allScreened$Resolution...Temporal[data_allScreened$SW.ID %in% "SW4_1320"] <- "subday"


# Check again whether there are rows where temporal resolution is larger than temporal extent
idx             <- which(data$ScaleTemporal < data$ResTemporal)
datTempIssue    <- data[idx,]
unique(datTempIssue$SW.ID) #3, but these have been checked and are OK.



#-----------------------------------------------#
## Spatial vs. temporal scale ----
#-----------------------------------------------#

# Large spatial scale vs. small temporal scale
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
# Several angling sessions "south of Majorca island" - hard to believe that sites were >100 km apart.
# So set spatial scale to 10,000 - 50,000, instead of >100,000 m.
data$Scale...Spatial..m.[data$SW.ID %in% "SW4_1224"]                         <- "10,000-50,000"
data$ScaleSpatial[data$SW.ID %in% "SW4_1224"]                                <- "10,000-50,000"
data_allScreened$Scale...Spatial..m.[data_allScreened$SW.ID %in% "SW4_1224"] <- "10,000-50,000"


## SW4_1714
# Spatial and temporal scale are more or less correct: paper states that sampling was done within 12 days time,
# so temporal scale is rather 'two week' than 'week', at several stations in the southern North Sea, where
# the map indicates that these are several tens of km apart and the overall area being >100 km.
data$Scale...Temporal[data$SW.ID %in% "SW4_1714"]                         <- "two week"
data$ScaleTemporal[data$SW.ID %in% "SW4_1714"]                            <- "two week"
data_allScreened$Scale...Temporal[data_allScreened$SW.ID %in% "SW4_1714"] <- "two week"


# Large spatial scale vs. small temporal resolution
datLargeSpSmallTe <- subset(data, ScaleSpatial %in% ">100,000" & ResTemporal < "day")
length(unique(datLargeSpSmallTe$SW.ID)) #121 papers
