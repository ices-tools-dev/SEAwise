#####################################################################################################################-
#####################################################################################################################-
#
#     Script to produce a final version of the database to be published alongside manuscript
#
#     This version includes all columns except those SEAwise specific or deemed irrelevant for publication.
#     It includes all papers for which data were extracted, i.e. all papers excluded based on the exclusion criteria
#     are not included.
#
#     Author(s): Esther Beukhof
#     November 2024
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths ----
#-----------------------------------------------#

library(openxlsx)


datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"
outPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/Manuscript/Database/"


#-----------------------------------------------#
# Read in data ----
#
#  info:
#  This section depends on the processed data file produced under Rscripts/Data processing
# 
#-----------------------------------------------#

data_allScreened                      <- readRDS(file=paste0(datPath, "data_AllScreened_correctTaxa_PressVar_RespVar_Methods_ScaleRes.rds"))



#-----------------------------------------------#
# Remove excluded papers ---- 
#-----------------------------------------------#

data                                  <- subset(data_allScreened, is.na(Exclusion.Criteria))



#-----------------------------------------------#
# Check bibliographical columns ---- 
#
# Check whether information is complete
# Remove irrelevant or incomplete columns where needed
#
#-----------------------------------------------#

# To do checks, make cropped version where one row is one paper
data.ID                               <- data[!duplicated(data$SW.ID),]

### Authors, title, year etc. ----
table(!is.na(data.ID$Authors)) #no NAs
table(!is.na(data.ID$Title)) #no NAs
table(!is.na(data.ID$Year)) #no NAs
table(!is.na(data.ID$Source.title)) #no NAs
table(!is.na(data.ID$Volume)) #one NAs
table(!is.na(data.ID$Issue)) #182 NAs, but quite common for journals not to have issue number, only volume
table(!is.na(data.ID$Page.start)) #85 NAs, but common for some journals to only have online version, e.g. PLoS ONE, Scientific Reports
table(!is.na(data.ID$Page.end)) #85 NAs, same as above
table(!is.na(data.ID$DOI)) #13 NAs
table(!is.na(data.ID$Abstract)) #no NAs
table(!is.na(data.ID$Language)) #no NAs
table(!is.na(data.ID$Document.Type)) #no NAs
table(data.ID$Database, useNA = "always") #no NAs


### Search ID ----
table(data.ID$SearchID, useNA = "always") #not useful outside SEAwise and in combo with other SEAwise reviews, so drop
colRemove                             <- "SearchID"


### Link ----

# Check what information is in database
length(unique(data.ID$Link)) #only 27 links provided
table(data.ID$Reader[!is.na(data.ID$Link)]) #by two reviewers
colRemove                             <- c(colRemove,"Link") #so drop column


### Open access ----

# Check what information is in database
table(data.ID$Open.Access, data.ID$Database, useNA = "always") #two times Web of Science? Check below
#Seems to be NA when not open access according to the green/bronze/gold system
#Fine to keep, but could be removed to reduce number of columns


#-----------------------------------------------#
# Adapt Region column ---- 
#-----------------------------------------------#
# Add regions by combining CS areas
data$RegionSEAwise                    <- data$Region
data$Region                           <- with(data, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                           ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                  ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                         ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))

data_allScreened$RegionSEAwise        <- data_allScreened$Region
data_allScreened$Region               <- with(data_allScreened, ifelse(RegionSEAwise %in% c("CS - North Sea","North Sea - non CS"),"North Sea",
                                                                       ifelse(RegionSEAwise %in% c("CS - Baltic Sea","Baltic Sea - non CS"),"Baltic Sea",
                                                                              ifelse(RegionSEAwise %in% c("CS - Western Waters","Western Waters - non CS"),"Western Waters",
                                                                                     ifelse(RegionSEAwise %in% c("CS - Mediterranean", "Mediterranean - non CS"),"Mediterranean Sea", RegionSEAwise)))))


#-----------------------------------------------#
# Other columns to be removed ---- 
#-----------------------------------------------#

### Comments ----

table(is.na(data.ID$Comments)) #438 NAs, so mostly NAs
colRemove                             <- c(colRemove, "Comments") #so drop column


### Exclusion criteria ----

table(data.ID$Exclusion.Criteria, useNA = "always") #correct, only NAs -> remove column
colRemove                             <- c(colRemove, "Exclusion.Criteria") #so drop column


### WP4 task ----

table(data.ID$WP4.task, useNA = "always") #no NAs
# Only relevant to SEAwise project, so remove
colRemove                             <- c(colRemove, "WP4.task") #so drop column


### More columns ----

colRemove                             <- c(colRemove, "Reader","ROWID","RegionSEAwise") #so drop column


#-----------------------------------------------#
# Remove columns ---- 
#-----------------------------------------------#

data.all                              <- data
keepCol                               <- names(data.all)[which(!names(data.all) %in% colRemove)]
data                                  <- subset(data.all, select = keepCol)


#-----------------------------------------------#
# Formatting ---- 
#-----------------------------------------------#

# Check structure
str(data)

# Ensure numeric columns are numeric
## Volume
sort(unique(data$Volume)) #includes non-numeric values so cannot be made numeric

## Issue
sort(unique(data$Issue)) #includes non-numeric values so cannot be made numeric

## Page numbers
sort(unique(data$Page.start)) #includes non-numeric values so cannot be made numeric
sort(unique(data$Page.end)) #includes non-numeric values so cannot be made numeric

## Quality spatial
sort(unique(data$Quality...Spatial..relative.1.3.)) #includes non-numeric values so cannot be made numeric


#-----------------------------------------------#
# Column names ---- 
#-----------------------------------------------#

# Adjust some column names
names(data)[names(data) %in% "Scale...Spatial..m."]        <- "Scale.Spatial"
names(data)[names(data) %in% "Scale...Temporal"]           <- "Scale.Temporal"

names(data)[names(data) %in% "Resolution...Spatial..m."]   <- "Resolution.Spatial"
names(data)[names(data) %in% "Resolution...Temporal" ]     <- "Resolution.Temporal" 

names(data)[names(data) %in% "Quality...Spatial..relative.1.3."]   <- "Quality.Spatial"
names(data)[names(data) %in% "Quality...Temporal"]                 <- "Quality.Temporal"
names(data)[names(data) %in% "Quality...Methods" ]                 <- "Quality.Methods" 

names(data)[names(data) %in% "Species.taxonomic.group.s." ]        <- "Species.taxonomic.group" 

names(data)[names(data) %in% "Pressure_variable" ]                 <- "Pressure.variable" 


#-----------------------------------------------#
# Ordering ---- 
#-----------------------------------------------#

# Decide on order of columns
orderCol   <- c("SW.ID",
                "Authors",
                "Title",
                "Year",                                    
                "Source.title",
                "Volume",
                "Issue",
                "Page.start",                              
                "Page.end",
                "DOI",
                "Abstract",
                "Language",                                
                "Document.Type",
                "Open.Access",
                "Database",
                "Region",                                  
                "Scale.Spatial",
                "Scale.Temporal",
                "Resolution.Spatial",
                "Resolution.Temporal",
                "Study.type", 
                "Sampling.Method.used.for.data.collection",
                "Description.Other.Sampling.Method",
                "Analytical.method.used.for.inference",
                "Quality.Spatial",       
                "Quality.Temporal",
                "Quality.Methods",
                "Concluding.statement.or.quotable.quote",
                "Ecosystem.component_level1",
                "Ecosystem.component_level2",
                "Ecosystem.component_level3",
                "Ecosystem.component_benthos_sediment",    
                "Species.taxonomic.group",
                "Pressure.type",
                "Pressure_level",
                "Pressure.variable",
                "Pressure.variable_category",
                "Pressure_other",
                "Fishery.type",
                "Gear_level1",
                "Gear_level2",
                "Target.species_metier",
                "Response.variable_paper",
                "Response.variable_category",
                "Direction.of.relationship",
                "Magnitude.of.relationship" )

# Change order of columns
data       <- data[,orderCol]

# Ensure rows are ordered by SW.ID
data       <- data[order(data$SW.ID),]


#-----------------------------------------------#
# Save ---- 
#-----------------------------------------------#

# Unformatted version
write.csv(data, file = paste0(datPath, "Database for manuscript/Data extracted review.csv"))
write.xlsx(data, file = paste0(datPath, "Database for manuscript/Data extracted review_unformatted.xlsx"))

# Create formatted version as Excel file
## Create workbook
wb                                    <- buildWorkbook(data, startRow=2)

## Create and add style for header row
headerStyle                           <- createStyle(fontColour = "white", bgFill = "black")
addStyle(wb = wb, sheet = 1,
         style = headerStyle,
         rows = 2, cols = c(1:ncol(data)))

## Insert headings in the top row
writeData(wb, 1, "Bibliographic data", startCol = 1, startRow = 1)
writeData(wb, 1, "Geography and methodology", startCol = 16, startRow = 1)
writeData(wb, 1, "Ecosystem component", startCol = 29, startRow = 1)
writeData(wb, 1, "Pressure", startCol = 34, startRow = 1)
writeData(wb, 1, "Gear", startCol = 39, startRow = 1)
writeData(wb, 1, "Impact measured", startCol = 43, startRow = 1)

## Add style for top row
topStyle1                            <- createStyle(fgFill = "#C4BD97", halign = "center") 
addStyle(wb, 1, style = topStyle1, rows = 1, cols = c(1:15))

topStyle2                            <- createStyle(fgFill = "#BDD7EE", halign = "center") 
addStyle(wb, 1, style = topStyle2, rows = 1, cols = c(15:28))

topStyle3                            <- createStyle(fgFill = "#F8CBAD", halign = "center") 
addStyle(wb, 1, style = topStyle3, rows = 1, cols = c(29:33))

topStyle4                            <- createStyle(fgFill = "#C6E0B4", halign = "center") 
addStyle(wb, 1, style = topStyle4, rows = 1, cols = c(34:38))

topStyle5                            <- createStyle(fgFill = "#FFE699", halign = "center") 
addStyle(wb, 1, style = topStyle5, rows = 1, cols = c(39:42))

topStyle6                            <- createStyle(fgFill = "#CCC0DA", halign = "center") 
addStyle(wb, 1, style = topStyle6, rows = 1, cols = c(43:46))


## Merge cells of top row
mergeCells(wb, 1, cols = c(1:15), rows = 1)
mergeCells(wb, 1, cols = c(16:28), rows = 1)
mergeCells(wb, 1, cols = c(29:33), rows = 1)
mergeCells(wb, 1, cols = c(34:38), rows = 1)
mergeCells(wb, 1, cols = c(39:42), rows = 1)
mergeCells(wb, 1, cols = c(43:46), rows = 1)

## Save
saveWorkbook(wb, file = paste0(datPath, "Database for manuscript/Data extracted review.xlsx"), overwrite = TRUE)

