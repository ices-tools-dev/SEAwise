#-----------------------------------------------#
# Check response variable categories and see whether they can be merged ----
#-----------------------------------------------#

# Reduce data so that there are no duplicates for each paper regarding the ecosystem component, response variable and direction
datDeDupl                             <- data[!duplicated(data[,c("SW.ID","Ecosystem.component_level1","Response.variable_category","Direction.of.relationship")])]


##### Biodiversity & Community composition ----

# Check data categorized as biodiversity
biodiv                               <- subset(data, Response.variable_category %in% "Biodiversity")
table(biodiv$Response.variable_paper) #looks good, all related to biodiversity

# Check data categorized as community composition
comcom                               <- subset(data, Response.variable_category %in% "Community composition")
comcom$Response.variable_paper
# have made changes to the original data extraction files on 24 & 27 June 2022

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Biodiversity"])) #50
length(unique(data$SW.ID[data$Response.variable_category %in% "Community composition"]))#72

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Biodiversity"]) #mostly negative
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Community composition"]) #mostly multiple, also some negative

# Merging? Possible, but the direction is often not so straightforward for community composition as it is for biodiversity (e.g. richness increases 
# or decreases in response to fishing). The composition may change in response to fishing, but it is often not possible to say whether there was a
# positive or negative relationship.
# It also seems reviewers have distinguished between the two response variables (as anticipated), and for cases this was not done, this has
# been changed in the data extraction files.
# Based on above two reasons, OK to keep the two separate.


##### Mortality & survival ----

# Check number of papers with either biodiversity or comm comp as response variable
length(unique(data$SW.ID[data$Response.variable_category %in% "Mortality"])) #65
length(unique(data$SW.ID[data$Response.variable_category %in% "Survival"])) #25

# Check direction of impact (note this is not equal to number of papers!)
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Mortality"]) #mostly positive
table(datDeDupl$Direction.of.relationship[datDeDupl$Response.variable_category %in% "Survival"]) #mostly negative, but also some 'no impact'

# Merging? Possible by changing response variable to 'mortality' for survival studies and reverse the direction of the relationship (this because 
# there are fewer survival than mortality studies).
# Although some survival studies might particularly have focussed context-wise on survival rather than mortality, the two are exchangeable:
# x% survival = 1-x% mortality.

# Merge by converting Survival to mortality
SurvToMort <- data[data$Response.variable_category %in% "Survival",]
table(SurvToMort$Direction.of.relationship)

SurvToMort$Direction.of.relationship <- with(SurvToMort, ifelse(Direction.of.relationship %in% "Positive","Negative",
                                                                ifelse(Direction.of.relationship %in% "Negative","Positive",Direction.of.relationship)))
table(SurvToMort$Direction.of.relationship)

SurvToMort$Response.variable_category <- "Mortality"