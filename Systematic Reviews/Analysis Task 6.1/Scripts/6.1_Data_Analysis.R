
#
##
### This file reads the outcomes of the data extraction and analyse it
##
#

library(wesanderson)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggraph)
library(igraph)
library(tidyverse)
library(treemap)
library(RColorBrewer)
library(data.table)
library(raster)
library(plotrix)
library(sf)
library(dplyr)
library(scales)
library(reshape2)
library(gridExtra)
library(ggsankey)

rm(list=ls())

###### LOAD THE DATA ####################################
datPath                               <- "datafiles/" 
outPath                               <- "Routput/"

tab <- read.csv(file=paste0(datPath,'WP6_SEAwise_DataExtraction_ALL_final.csv'),header = TRUE, na.strings=c("","NA"))
colnames(tab)

# rename columns 
colnames(tab) <- c(colnames(tab)[1:31], 'Model.name', 'Model.name.explicit', 'Biological.component', 'Species', 
                   "Population.str", "Fleet.comp", "Fleet.categ", "Economic.comp", 
                   "Spatial.cont", "Environmental.vars", "Process", "Management.meas",
                   "Used.in.man", "Indicators.type", 'Indicators.name', "Implemented.in.soft", 
                   "Software.name", "Conclusive.com")

colors =  wes_palette(name = "Zissou1", 5, type="discrete")


# how many papers treated : 673 papers (out of 802)
treated_papers <- unique(tab$SW.ID)
length(treated_papers)
npapers <- length(treated_papers)


# Making a list of unique papers
basis <- tab[,c(1,18)]
basis <- basis[!duplicated(basis$SW.ID), ]

# how many retained :  687 papers
ExCriteria <- as.data.frame(table(basis$Exclusion.Criteria))

# how many rejected
excluded_2 <- subset(basis, !is.na(basis$Exclusion.Criteria))
dim(excluded_2)


# Select the retained papers
data  <- subset(tab, is.na(tab$Exclusion.Criteria))
nkept <- length(unique(data$SW.ID))
data2 <- data[!duplicated(data$SW.ID), ]


# NOTE: data is the full database, data2 is reduced (excluding multiple lines for same SW6s)

####################################################################
######### CREATE the tables and do plots by field ##################
####################################################################


# EXCLUDED papers #################


#------- Excluded in screening phase

Excluded_screening <- c(19, 158, 178, 422) 
Criteria           <-  c('Language', 'Article type', 'Not relevant to marine ecosystem', 
                         'Not relevant to fisheries management evaluation models')
  
names(Excluded_screening) <- Criteria
dotchart(sort(Excluded_screening), col=(colors[2:5]), pt.cex = 3, cex =1.1, cex.main=1, main =paste0(sum(Excluded_screening), ' out of 1537 articles (', label_percent() (sum(Excluded_screening)/1537), ') excluded during screening'))
dev.copy(png, file=paste0(outPath, 'ExcludedScreen.png'), width=25, height=12, unit='cm', res=300)
dev.off()

#-------- Excluded in extraction phase
x1 <- excluded_2$Exclusion.Criteria

# Other
x1[which(x1 %in% str_subset(x1, "Article type"))]   <- "Article type"
x1[which(x1 %in% str_subset(x1, "Other"))]          <- "Other (unable to find, non-existing)"

dotchart(sort(table(x1)),col=colors, pt.cex = 3, cex =1.1, cex.main=1, main =paste0(length(x1), ' out of ',npapers ,' articles (', label_percent() (length(x1)/npapers), ') excluded during extraction'))
dev.copy(png, file=paste0(outPath, 'ExcludedExtraction.png'), width=25, height=12, unit='cm', res=300)
dev.off()


# CONTRIBUTORS #####################

contrib     <- tab[,c(1,2)]
contrib     <- contrib[!duplicated(contrib$SW.ID), ]
Contr.Table <- sort(table(contrib$SearchID))
Contr.Table <- as.data.frame(Contr.Table)
write.csv(Contr.Table, file=paste0(outPath, 'Contributors.csv'), row.names=F)

# --- authors 
Authors <- read.csv('../Contributors_order.csv')
Authors <- as.matrix(Authors)
Authors <- paste(Authors, sep="")
Authors <- toString(Authors)

# YEAR PUBLISHED ###################

year <- data2$Year
year <- table(year)
plot(year, type='l', xlab='Publication year', ylab='Number of retained papers')
dev.copy(png, file=paste0(outPath, 'YearNumbers.png'), width=18, height=13, unit='cm', res=300)
dev.off()

data2$Decade <- rep(NA, nrow(data2))
data2$Decade[which(data2$Year < 1990)] <- "1990s"
data2$Decade[which(data2$Year <2000 & data2$Year >= 1990)] <- "2000s"
data2$Decade[which(data2$Year <2010 & data2$Year >= 2000)] <- "2010s"
data2$Decade[which(data2$Year <2020 & data2$Year >= 2010)] <- "2020s"
data2$Decade[which(data2$Year <2030 & data2$Year >= 2020)] <- "2030s"

Year.tab   <- data.frame(year = data2$Year, SW.ID = data2$SW.ID)
Decade.tab <- data.frame(decade = data2$Decade, SW.ID = data2$SW.ID)

barplot(prop.table(table(data2$Decade)), ylab='Frequency (papers)', xlab='year')

# TASK #############################

# split strings
x  <- tstrsplit(data2$WP6.task , split="_")
x1 <- unlist(x)

# identify tasks by subsetting string 6.

frq <- NA
frq[1] <- length(str_subset(x1, ".2"))
frq[2] <- length(str_subset(x1, ".3"))
frq[3] <- length(str_subset(x1, ".4"))
frq[4] <- length(str_subset(x1, ".5"))
frq[5] <- length(str_subset(x1, "6.6"))

task <- NA
task[1] <- "Task 6.2 - Climatic and ecological effects"
task[2] <- "Task 6.3 - Social and economic effects"    
task[3] <- "Task 6.4 - Targets and limits for indicators"    
task[4] <- "Task 6.5 - Management strategies" 
task[5] <- "Task 6.6 - Fisheries in the socio-ecological system" 

WP6task <- c(rep(task[1], frq[1]), rep(task[2], frq[2]), rep(task[3], frq[3]), rep(task[4], frq[4]), rep(task[5], frq[5])) 

WP6task.table <- data.frame(Var1 = task, 
                            Freq = frq)

WP6task.table <- WP6task.table[order(-WP6task.table$Freq),]

Task.tab  <- data.frame(task = x1, 
                    SW.ID = rep(data2$SW.ID, length(x))
)
Task.tab <- subset(Task.tab, !is.na(task))

# identify tasks by subsetting string 6.

Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.2'))]      <- task[1]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.3'))]      <- task[2]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.4'))]      <- task[3]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.5'))]      <- task[4]
Task.tab$task[which(Task.tab$task %in% str_subset(x1,'.6'))]      <- task[5]

# Generate the layout
packing <- circleProgressiveLayout(table(WP6task), sizetype='area')
packing$radius <- 0.95*packing$radius
WP6.task <- cbind(table(WP6task), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = WP6.task, aes(x, y,  size = 1.3, label = WP6task), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.75)
dev.copy(png, file=paste0(outPath, 'WP6_Task.png'), width=25, height=15, unit='cm', res=300)
dev.off()


# barchart
x <- table(WP6task)
names (x) <- c('Task 6.2', 'Task 6.3', 'Task 6.4', 'Task 6.5', 'Task 6.6')
x <- sort(x)
barTask <- barplot(x, xlim=c(0,400), horiz = T, xlab='Number of unique papers', main='Number of unique papers per WP6 tasks')
text(barTask, x=x+20 ,   paste( x, sep="") ,cex=1) 
dev.copy(png, file=paste0(outPath, 'WP6_Task_barplot.png'), width=15, height=15, unit='cm', res=300)
dev.off()


# TASK by UNIQUE paper #############################

# split strings
x  <- tstrsplit(data2$WP6.task , split="_")
x1 <- as.data.frame(x)

task <- NA
task[1] <- "6.2"
task[2] <- "6.3"    
task[3] <- "6.4"    
task[4] <- "6.5" 
task[5] <- "6.6" 

# change names 
for (i in 1:ncol(x1)){
  x1[which(x1[,i] %in% str_subset(x1[,i],'.2')),i]      <- task[1]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.3')),i]      <- task[2]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.4')),i]      <- task[3]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.5')),i]      <- task[4]
  x1[which(x1[,i] %in% str_subset(x1[,i],'.6')),i]      <- task[5]
}

x2 <- apply(x1, 1, FUN=function(x) toString(na.omit(x)))
x2 <- subset(x2, x2!="")

# corrections
x2[which(x2 =="6.5, 6.2")]   <- "6.2, 6.5"
x2[which(x2 =="6.2, 6.3, Task, 6.4, 6.5")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.2, 6.3, 6.5, management strategies")]   <- "6.2, 6.3, 6.5"
x2[which(x2 =="6.4, 6.5, 6.2, 6.3")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.4, 6.3, 6.5, 6.2")]   <- "6.2, 6.3, 6.4, 6.5"
x2[which(x2 =="6.4, 6.3")]             <- "6.3, 6.4"
x2[which(x2 =="6.3, 6.2")]             <- "6.2, 6.3"
x2[which(x2 =="6.4, 6.5, 6.2")]         <- "6.2, 6.4, 6.5"
x2[which(x2 =="6.4, 6.5, 6.3")]         <- "6.3, 6.4, 6.5"

# Generate the layout
packing <- circleProgressiveLayout(table(x2), sizetype='area')
packing$radius <- 0.95*packing$radius
WP6task_paper <- cbind(table(x2), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = WP6task_paper, aes(x, y,  size = 1.3, label = x2), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.75)
dev.copy(png, file=paste0(outPath, 'WP6_Task_paper.png'), width=25, height=15, unit='cm', res=300)
dev.off()

## REGION ######################

# BE CAREFULL unique SW6 and area (use data2)

#Region.table <- as.data.frame(table(data2$Region))
#Region.table <- Region.table[order(-Region.table$Freq),]
#problem <- subset(data, data$Region %in% c("NE Atlantic", "CS-Western Waters; Mediterranenan - non CS"))

# Break into broad categories (Europe, North America, South America, Asia, Africa, Australia)

x1  <- data2$Continent


Region.table <- as.data.frame(table(x1))
Region.table <- Region.table[order(-Region.table$Freq),]

Region.tab <- data.frame(region = x1, 
                         SW.ID = data2$SW.ID)
Region.tab <- subset(Region.tab, !is.na(region))

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'Area.png'), width=20, height=15, unit='cm', res=300)
dev.off()

## POPULATION STRUCTURE ################
# split strings
x  <- tstrsplit(data2$Population.str , split="_")
x1 <- unlist(x)

x1[which(x1 %in% str_subset(x1,'iomass'))]      <- "Biomass aggregated"
x1[which(x1 %in% str_subset(x1,'stage') | x1 %in% str_subset(x1,'Stage'))]          <- "Stage structured"
x1[which(x1 %in% str_subset(x1,'size')| x1 %in% str_subset(x1,'Size'))]             <- "Size structured"
x1[which(x1 %in% str_subset(x1,'length') | x1 %in% str_subset(x1,'Length') | x1 %in% str_subset(x1,'lenght'))]        <- "Size structured"
x1[which(x1 %in% str_subset(x1,'sex') | x1 %in% str_subset(x1,'Sex'))]              <- "Sex structured"
x1[which(x1 %in% str_subset(x1,'abundance') | x1 %in% str_subset(x1,'Abundance'))]  <- "Abundance aggregated"
x1[which(x1 %in% str_subset(x1,'ndividual') )]                                      <- "Individual based"
x1[which(x1 =='age structured' | x1 =='age' | x1 %in% str_subset(x1,'Age') | x1 == 'age structure')]  <- "Age structured"
x1[which(x1 %in% str_subset(x1,'age structured for') | x1 == 'age structured (rest of species)')]   <- "Age structured"

x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"

x1[which(x1 %in% str_subset(x1, "Not clear"))]       <- "Not specified"
x1[which(x1 %in% str_subset(x1, "not clear"))]       <- "Not specified"
x1[which(x1 %in% str_subset(x1, "CPUE"))]            <- "Not specified"
x1[which(x1 %in% str_subset(x1, "dynamics model"))]  <- "Not specified"
x1[which(x1 %in% str_subset(x1, "Not specified"))]   <- "Not specified"
x1[which(x1 %in% str_subset(x1, "not specified"))]   <- "Not specified"
x1[which(x1 %in% str_subset(x1, "No specified"))]    <- "Not specified"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]    <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "not relevant"))]    <- "Not relevant"
x1[which(x1 =="structured")]    <- "Not specified"

Population.tab <- data.frame(popul = x1, 
                             SW.ID = rep(data2$SW.ID, length(x)))
Population.tab <- subset(Population.tab, !is.na(popul))

x1 <- subset(x1, !is.na(x1))
Population.table <- as.data.frame(table(x1))
Population.table <- Population.table[order(-Population.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'Popul_str.png'), width=20, height=15, unit='cm', res=300)
dev.off()


## MODEL name ##########################
# split strings
x  <- tstrsplit(data2$Model.name, split="_")
x1 <- unlist(x)

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"

# Not specified
x1[which(x1 == "not specified")]   <- "Not specified"

# No model
x1[which(x1 == "none"|x1 == "Not relevant" |x1 == "NOT RELEVANT" |x1 == "None"  )]   <- "No model"
x1[which(x1 == "none"|x1 %in% str_subset(x1,"no model") |x1 == "NOT RELEVANT" |x1 == "None"  )]       <- "No model"

# Other corrections
x1[which(x1 == "Ecospace" | x1 == "ecospace")]    <- "ECOSPACE"
x1[which(x1 == "ISIS-Fish" | x1 == "ISIS-fish")]  <- "ISIS-FISH"
x1[which(x1 %in% str_subset(x1, "ATLANTIS")| x1=='Atlantis')]     <- "ATLANTIS"
x1[which(x1 %in% str_subset(x1, "OSMOSE"))]       <- "OSMOSE"
x1[which(x1 %in% str_subset(x1, "Bioeconomic model"))]     <- "Other"
x1[which(x1 %in% str_subset(x1, "othe"))]         <- "Other"
x1[which(x1 %in% str_subset(x1, "SS-DBEM"))]      <- "SS-DBEM"
x1[which(x1 %in% str_subset(x1, "Fishrent"))]     <- "FISHRENT"
x1[which(x1 == "ELFSim")]                         <- "ELFSIM"
x1[which(x1 == "ROMs")]                           <- "ROMS"

Model.tab <- data.frame(model = x1, 
                             SW.ID = rep(data2$SW.ID, length(x)))
Model.tab <- subset(Model.tab, !is.na(model))

x1 <- subset(x1, !is.na(x1))
Model.table <- as.data.frame(table(x1))
Model.table <- Model.table[order(-Model.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'Model.png'), width=30, height=25, unit='cm', res=300)
dev.off()

## BIOLOGICAL COMPONENT #################
# split strings
x  <- tstrsplit(data2$Biological.component , split="_")
x1 <- unlist(x)

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1== "Mixed stock")]    <- "Other"
x1[which(x1== "Multi-stock")]    <- "Other"

# Not relevant
x1[which(x1 %in% str_subset(x1, "not relevant"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Not specified"

# single stock
x1[which(x1== "single stock")]   <- "Single stock"

Bio.tab <- data.frame(bio = x1, 
                        SW.ID = rep(data2$SW.ID, length(x)))
Bio.tab <- subset(Bio.tab, !is.na(bio))

x1 <- subset(x1, !is.na(x1))
Bio.table <- as.data.frame(table(x1))
Bio.table <- Bio.table[order(-Bio.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'Bio.png'), width=20, height=15, unit='cm', res=300)
dev.off()

## INDICATORS TYPE #####################

# split strings
x1  <- data$Indicators.type

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Not specified"

# Not relevant
x1[which(x1=="NOT RELEVANT")]   <- "Not relevant"
x1[which(x1 == "biological")]   <- "Biological"
x1[which(x1 == "Economic ")]    <- "Economic"

Indicator.tab <- data.frame(indicator = x1, 
                        SW.ID = rep(data$SW.ID))
Indicator.tab <- subset(Indicator.tab, !is.na(indicator))

x1 <- subset(x1, !is.na(x1))
Indicator.table <- as.data.frame(table(x1))
Indicator.table <- Indicator.table[order(-Indicator.table$Freq),]


# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'IndicType.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## INDICATORS NAME by TYPE #############

# probably by category present the 10 most frequenty used
Ind.econ <- subset(data, Indicators.type=='Economic')
Ind.biol <- subset(data, Indicators.type=='Biological')
Ind.soci <- subset(data, Indicators.type=='Social')
Ind.ecos <- subset(data, Indicators.type=='Ecosystem based')
Ind.othe <- subset(data, Indicators.type=='Other')

########## Biological 10 most frequent
x  <- tstrsplit(Ind.biol$Indicators.name , split="_")
x1 <- unlist(x)
x1 <- subset(x1, !is.na(x1))
x1 <- x1[order(x1)]

# corrections (biomass, abundance, catch)
x1[which(x1=="biomass" | x1=="stock biomass" | x1=="biomass level" |x1=="Total biomass" | x1=="total biomass" | x1%in% str_subset(x1,'Biomass of target') | x1=='Biomass level' | x1=='mean biomass')]     <- "Biomass"
x1[which(x1=="biomasses" | x1=="Stock biomass" | x1 == "Population biomass" | x1=='biomasss' | x1=='biomass of stocks' | x1%in% str_subset(x1,'Biomass of all') | x1=='Biomass ' | x1=='B' | x1=='Biomass')]               <- "Biomass"
x1[which(x1 %in% str_subset(x1, "F/F")| x1 %in% str_subset(x1, "F rel"))]       <- "F/Fmsy"
x1[which(x1 %in% str_subset(x1, "SPR")| x1 %in% str_subset(x1, "per recrui"))]   <- "SPR"

x1[which(x1 %in% str_subset(x1, "abundance") | x1 %in% str_subset(x1, "Abundance") |  x1 %in% str_subset(x1, "adundance") | x1 %in% str_subset(x1,"stock size") | x1 %in% str_subset(x1,"Stock size") | x1 %in% str_subset(x1,"opulation") ) ]       <- "Abundance"
x1[which(x1 %in% str_subset(x1, "MSY")| x1 %in% str_subset(x1, "sustainable yield"))]       <- "MSY"
x1[which(x1=="Yearly catch" | x1=="yield" | x1=="yields" | x1=="Total catch" | x1=="total catch" | x1=="catches" | x1=="catch level" )]             <- "Catch"
x1[which(x1=="Landings" | x1=="landings" | x1=="total catches" | x1=="total landings" | x1=="Yield" | x1=="Catches" | x1=="Catch by species")]          <- "Catch"
x1[which(x1 %in% str_subset(x1, "harvest") | x1 %in% str_subset(x1, "Harvest") | x1== "catch")]   <- "Catch"

x1[which(x1 %in% str_subset(x1, "spawning stock") | x1=='spawning SSB' | x1 %in% str_subset(x1, "SSB"))]   <- "SSB"
x1[which(x1 %in% str_subset(x1, "Spawning stock"))]   <- "SSB"
x1[which(x1 == "Spawning SSB" | x1 == "spawning SSB")]                   <- "SSB"
x1[which(x1 %in% str_subset(x1, "fishing mortality") | x1 %in% str_subset(x1, "Fishing mortality") | x1=="F")]        <- "Fishing mortality"

x1[which(x1=="recruitment" | x1=="recruits" | x1=="Recruits" | x1=="recruitment regimes" | x1 %in% str_subset(x1, "recrutis"))]              <- "Recruitment"
x1[which(x1 %in% str_subset(x1, "ycatch") | x1 %in% str_subset(x1, "by-catch") | x1=="By-catch")]      <- "Bycatch"
x1[which(x1 %in% str_subset(x1, "discard") | x1 %in% str_subset(x1, "Discard"))]                        <- "Discards"
x1[which(x1 %in% str_subset(x1, "relative biomass") | x1 %in% str_subset(x1, "SSB vs") | x1 %in% str_subset(x1, "SSB/") | x1 %in% str_subset(x1, "relative to status") | x1 %in% str_subset(x1, "relative to target") | x1 %in% str_subset(x1, "iomass/car"))]   <- "Relative biomass"
x1[which(x1 %in% str_subset(x1, "relative biomass") | x1 %in% str_subset(x1, "SSB vs") | x1 %in% str_subset(x1, "SSB/") )]   <- "Relative biomass"
x1[which(x1 %in% str_subset(x1, "relative abundance"))] <- "Relative abundance"

x1[which(x1 %in% str_subset(x1, "Multiple") | x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Mutiple"))]   <- "Multiple"
x1[which(x1 %in% str_subset(x1, "mortality") | x1 %in% str_subset(x1, "Mortality") | x1=='Fishing morality')] <- "Mortality"
x1[which(x1 == "Multispecies Maximum Sustainable Yield")]                    <- "MMSY"
x1[which(x1 %in% str_subset(x1, "exploitation") | x1 %in% str_subset(x1, "Exploitation")  | x1 %in% str_subset(x1, "explotation"))]   <- "Expoitation rate"

x1[which(x1 %in% str_subset(x1, "ffort"))]                     <- "Effort"
x1[which(x1 %in% str_subset(x1, "overfis"))]                   <- "Overfishing"
x1[which(x1 %in% str_subset(x1, "Depletion") | x1 %in% str_subset(x1, "depletion"))] <- 'Depletion level'
         
Ind.biol.table <- as.data.frame(table(x1))
Ind.biol.table <- Ind.biol.table[order(-Ind.biol.table$Freq),]

x2 <- as.vector(Ind.biol.table$x1[1:10])
x2 <- subset(x1, x1 %in% x2)
x1 <- x2

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, '10BioInds.png'), width=25, height=20, unit='cm', res=300)
dev.off()

########## Economic 10 most frequent
x  <- tstrsplit(Ind.econ$Indicators.name , split="_")
x1 <- unlist(x)
x1 <- subset(x1, !is.na(x1))

x1 <- x1[order(x1)]

x1[which(x1 =="Yield" | x1 == "yield" | x1 == "catch" | x1=="Catch" | x1=="total catch" | x1=="Total catch" | x1=="total annual catch" | x1== "Annual catch" | x1== " catch")]   <- "Yield"
x1[which(x1 %in% str_subset(x1, "harvest") | x1 %in% str_subset(x1, "Harvest") | x1== "landings"  | x1== "Landings" | x1== "catches" | x1== "Catches" | x1== "catch levels")]                      <- 'Yield'
x1[which(x1 %in% str_subset(x1, "Cost") | x1 %in% str_subset(x1, "costs") | x1 %in% str_subset(x1, "cost") | x1 %in% str_subset(x1, "Costs"))]                 <- "Costs"
x1[which(x1 %in% str_subset(x1, "revenue") | x1 %in% str_subset(x1, "Revenue") | x1 %in% str_subset(x1, "Revenues") | x1 %in% str_subset(x1, "revenues"))]     <- "Revenues"
x1[which(x1 %in% str_subset(x1, "NPV") | x1 %in% str_subset(x1, "net present value") | x1 %in% str_subset(x1, "Net Present Value") )]                          <- "NPV"
x1[which(x1 %in% str_subset(x1, "rent") | x1 %in% str_subset(x1, "Rent"))]       <- "Rent"
x1[which(x1 %in% str_subset(x1, "vessel") | x1 %in% str_subset(x1, "boat"))]     <- "Number of vessels"

x1[which(x1 %in% str_subset(x1, "Landed value") | x1 %in% str_subset(x1, "Catch value") | x1 %in% str_subset(x1, "catch value"))]         <- "Catch value"
x1[which(x1=="value of catch" | x1=="value of landings"  | x1=="Landing value" | x1=="landed value")]                                     <- "Catch value"
x1[which(x1 %in% str_subset(x1, "Income") | x1 %in% str_subset(x1, "income"))]                                                            <- "Income"
x1[which(x1 %in% str_subset(x1, "gross value") | x1 %in% str_subset(x1, "Gross Value") | x1 %in% str_subset(x1, "Gross value") | x1=="GAV" | x1=="value added")]     <- "GVA"
x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") | x1=="Mutiple")]      <- "Multiple"
x1[which(x1 %in% str_subset(x1, "MEY") )]     <- "MEY"
x1[which(x1 %in% str_subset(x1, "rice") )]     <- "Prices"
x1[which(x1 %in% str_subset(x1, "ffort") )]  <- "Fishing effort"
x1[which(x1 %in% str_subset(x1, "profit") | x1 %in% str_subset(x1, "Profit"))]     <- "Profit"

x1[which(x1 %in% str_subset(x1, "consumer surplus") )]         <- "Consumer surplus"
x1[which(x1 %in% str_subset(x1, "profitability") )]            <- "Profitability"

Ind.econ.table <- as.data.frame(table(x1))
Ind.econ.table <- Ind.econ.table[order(-Ind.econ.table$Freq),]

x2 <- as.vector(Ind.econ.table$x1[1:10])
x2 <- subset(x1, x1 %in% x2)
x1 <- x2

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, '10EcoInds.png'), width=25, height=20, unit='cm', res=300)
dev.off()

########## Social 10 most frequent
x  <- tstrsplit(Ind.soci$Indicators.name , split="_")
x1 <- unlist(x)
x1 <- subset(x1, !is.na(x1))
x1 <- x1[order(x1)]

# corrections
x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") )]    <- "Multiple"
x1[which(x1 %in% str_subset(x1, "catch") | x1 %in% str_subset(x1, "Catch") )]          <- "Catch"
x1[which(x1 %in% str_subset(x1, "mployment") | x1 %in% str_subset(x1, "mployement") | x1=="unemployment"  | x1=="labour" | x1=="Labour" | x1=="Jobs" |x1 %in% str_subset(x1, "jobs"))]    <- "Employment"
x1[which(x1 %in% str_subset(x1, "wage") | x1 %in% str_subset(x1, "salaries") | x1 =="crew remuneration")]    <- "Salaries"
x1[which(x1 %in% str_subset(x1, "FTE") )]    <- "FTE"
x1[which(x1 %in% str_subset(x1, "social benefit") | x1 %in% str_subset(x1, "Social benefit") | x1=='?benefits')]    <- "Social benefits"
x1[which(x1 %in% str_subset(x1, "number of vessels") | x1=='capacity')]    <- "Fleet size"
x1[which(x1 %in% str_subset(x1, "ncome") )]    <- "Income"
x1[which(x1 %in% str_subset(x1, "elfare") | x1 %in% str_subset(x1, "ellbeing") )]    <- "Community welfare"

Ind.soci.table <- as.data.frame(table(x1))
Ind.soci.table <- Ind.soci.table[order(-Ind.soci.table$Freq),]

x2 <- as.vector(Ind.soci.table$x1[1:9])
x2 <- subset(x1, x1 %in% x2)
x1 <- x2

# Generate the layout

packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, '10SociInds.png'), width=25, height=20, unit='cm', res=300)
dev.off() 

######### Ecosystem based 10 most frequent
x  <- tstrsplit(Ind.ecos$Indicators.name , split="_")
x1 <- unlist(x)
x1 <- subset(x1, !is.na(x1))
x1 <- x1[order(x1)]

# corrections
x1[which(x1 %in% str_subset(x1, "multiple") | x1 %in% str_subset(x1, "Multiple") )]    <- "Multiple"
x1[which(x1 %in% str_subset(x1, "eystonene") )]          <- "Keystoneness"
x1[which(x1 %in% str_subset(x1, "rophic level of community") | x1 %in% str_subset(x1, "rophic level of the community") | x1 =="trophic level of the system" | x1 =="trophic level" | x1 =="Trophic level" | x1 =="mean trophic level")]    <- "Trophic level of community"
x1[which(x1 %in% str_subset(x1, "rophic level of catch") | x1 %in% str_subset(x1, "rophic level of the catch") | x1 =="catch trophic level")]    <- "Trophic level of catch"
x1[which(x1 %in% str_subset(x1, "Finn") | x1 %in% str_subset(x1, "finn") )]    <- "Finn's cycling index"
x1[which(x1 %in% str_subset(x1, "ishing in balance") )]    <- "Fishing in Balance (FIB) index"
x1[which(x1 %in% str_subset(x1, "ishing mortality") )]    <- "Fishing mortality"
x1[which(x1 %in% str_subset(x1, "Kempton") )]             <- "Kemptons Q"
x1[which(x1 %in% str_subset(x1, "arge fish index") | x1 %in% str_subset(x1, "arge fish indicator") )]    <- "Large fish indicator"
x1[which(x1 %in% str_subset(x1, "arine trophic index") )]             <- "Marine trophic index"

x1[which(x1=='Mixed Trophic Impact')]    <- "Mixed trophic impact"
x1[which(x1=='not specified')]           <- "Not specified"
x1[which(x1 %in% str_subset(x1, "Pelagic/demersal") | x1 %in% str_subset(x1, "demersal fish biomass to pelagic") | x1 %in% str_subset(x1, "total demersal to total pelagic") | x1 %in% str_subset(x1, "pelagic vs benthic")  | x1 %in% str_subset(x1, "Pelagic:Demersal"))]    <- "Pelagic over demersal biomass"
x1[which(x1 %in% str_subset(x1, "biomass to primary") | x1 %in% str_subset(x1, "biomass to total primary")  | x1 %in% str_subset(x1, "biomass to primary production" ) | x1 %in% str_subset(x1, "biomass relative to primary" ))]    <- "Total biomass to primary production"
x1[which(x1 %in% str_subset(x1, "hannon") | x1 =="species diversity" | x1 =="species biodiversity") ]    <- "Shannon diversity"
x1[which(x1 %in% str_subset(x1, "lope of the community") )]    <- "Slope of community size spectrum"
x1[which(x1== "total biomass" |x1== "total biomass" |x1== "total system biomass"| x1== "Total system biomass" | x1== "total population biomass" | x1== "Biomass" )]    <- "Total biomass"
x1[which(x1== "total catch" |x1== "total catch" |x1== "total system catch"| x1== "Total system catch" )]    <- "Total catch"


Ind.ecos.table <- as.data.frame(table(x1))
Ind.ecos.table <- Ind.ecos.table[order(-Ind.ecos.table$Freq),]

x2 <- as.vector(Ind.ecos.table$x1[1:10])
x2 <- subset(x1, x1 %in% x2)
x1 <- x2

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, '10EcosInds.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## MANAGEMENT MEASURES CONSIDERED #############

# split strings
x  <- tstrsplit(data2$Management.meas , split="_")
x1 <- unlist(x)

# Corrections
x1[which(x1 %in% str_subset(x1, "not relevant"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1=="No management measures")]            <- "Not relevant"

x1[which(x1 %in% str_subset(x1, "nput"))]            <- "Input control"
x1[which(x1 %in% str_subset(x1, "ffort"))]           <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing press"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing rate"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "Fishing Seas"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "harvest time"))]   <- "Input control"
x1[which(x1 %in% str_subset(x1, "utput"))]      <- "Output control"
x1[which(x1 %in% str_subset(x1, "ITQ"))]        <- "Output control"
x1[which(x1 %in% str_subset(x1, "patial"))]     <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "losure"))]     <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "no take"))]    <- "Spatial managment"
x1[which(x1 %in% str_subset(x1, "echnical"))]   <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "electivity"))] <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "gear"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "Gear"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "length"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "esh size"))]       <- "Technical measures"
x1[which(x1 %in% str_subset(x1, "nhance"))]     <- "Enhancement study"
x1[which(x1 %in% str_subset(x1, "nutrient"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "nitrogen"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "trophication"))]     <- "Water nutrient control"
x1[which(x1 %in% str_subset(x1, "Water quality"))]     <- "Water nutrient control"

# Other
x1[which(x1 %in% str_subset(x1, "other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Other"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "conservation level"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "SQ"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Sex"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "Promotion"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "morphology"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "efficiency"))]   <- "Other"
x1[which(x1 %in% str_subset(x1, "not specified"))]  <- "Other"

Man.tab <- data.frame(man = x1, 
                      SW.ID = rep(data2$SW.ID, length(x)))
Man.tab <- subset(Man.tab, !is.na(man))

x1 <- subset(x1, !is.na(x1))
Man.table <- as.data.frame(table(x1))
Man.table <- Man.table[order(-Man.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)


# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'Management.png'), width=20, height=15, unit='cm', res=300)
dev.off()


## USED IN MANAGEMENT ################# 

x  <- tstrsplit(data2$Used.in.man, split="_")
x1 <- unlist(x)

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 =="no")]   <- "No"

UsedMan.tab <- data.frame(Useman = x1, 
                      SW.ID = rep(data2$SW.ID, length(x)))
UsedMan.tab <- subset(UsedMan.tab, !is.na(Useman))

x1 <- subset(x1, !is.na(x1))
Manag.table <- as.data.frame(table(x1))
Manag.table <- Manag.table[order(-Manag.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'UsedinMan.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## IMPLEMENTED IN SOFT ################ 

x1  <- data2$Implemented.in.soft

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 =="no")]   <- "No"
x1[which(x1 =="yes")]  <- "Yes"
x1[which(x1 =="yes ")]  <- "Yes"

Soft.table <- as.data.frame(table(x1))
Soft.table <- Soft.table[order(-Soft.table$Freq),]

## ENVIRONMENTAL VARS AND PROCESS ################# (YES/NO)

x1  <- data2$Environmental.vars

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "Yes"))]        <- "Yes"
x1[which(x1 =="no")]    <- "No"
x1[which(x1 =="yes")]   <- "Yes"
x1[which(x1 =="NOT RELEVANT")]  <- "Not relevant"
x1[which(x1 =="not relevant")]  <- "Not relevant"

Env.table <- as.data.frame(table(x1))
Env.table <- Env.table[order(-Env.table$Freq),] 


# Which process
x2  <- data2$Process[which(x1=='Yes')]
x3  <- data2$SW.ID[which(x1=='Yes')]
x2  <- tstrsplit(x2, split="_")
x1 <- unlist(x2)

x1[which(x1 %in% str_subset(x1, "ot clear"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "various") | x1 %in% str_subset(x1, "ultiple") | x1 %in% str_subset(x1, "many"))]        <- "Multiple"
x1[which(x1 %in% str_subset(x1, "rowth"))]       <- "Growth"
x1[which(x1 %in% str_subset(x1, "ortality"))]    <- "Mortality"
x1[which(x1 %in% str_subset(x1, "ecruitment") |x1 %in% str_subset(x1, "recru"))]  <- "Recruitment"
x1[which(x1 %in% str_subset(x1, "atchability"))] <- "Catchability"
x1[which(x1 %in% str_subset(x1, "urviva"))]      <- "Survival"
x1[which(x1 %in% str_subset(x1, "istribution"))]      <- "Distribution"
x1[which(x1 %in% str_subset(x1, "eproduct"))]      <- "Reproduction"
x1[which(x1 %in% str_subset(x1, "roduct"))]      <- "Productivity"
x1[which(x1 %in% str_subset(x1, "predat"))]      <- "Predation"
x1[which(x1 %in% str_subset(x1, "ovement")| x1 %in% str_subset(x1, "igration") | x1=='Dispersal')]      <- "Migration"
x1[which(x1 %in% str_subset(x1, "abitat"))]      <- "Habitat"
x1[which(x1 %in% str_subset(x1, "ishing"))]      <- "Fishing effort"
x1[which(x1 =="consumption/biomass ratio")]      <- "consumption/biomass rate (Q/B)" 
x1[which(x1 %in% str_subset(x1, "dvection"))]      <- "Advection"
x1[which(x1 %in% str_subset(x1, "capacity"))]      <- "Carrying capacity"
x1[which(x1 %in% str_subset(x1, "biomass"))]      <- "Biomass"
x1[which(x1 %in% str_subset(x1, "tolerance"))]      <- "Tolerance"
x1[which(x1 %in% str_subset(x1, "feeding"))]      <- "Feeding"

# In many cases this was misundestood and env variables were inserted instead
x1[which(x1 %in% str_subset(x1, "emperature")| x1 %in% str_subset(x1, "alinity") | x1 %in% str_subset(x1, "itrogen")| x1 %in% str_subset(x1, "phorus") | x1=='rainfall' | x1 %in% str_subset(x1, "xygen"))]      <- "Not clear"

Process.tab <- data.frame(Process = x1, 
                          SW.ID = rep(x3, length(x2)))
Process.tab <- subset(Process.tab, !is.na(Process))

x1 <- subset(x1, !is.na(x1))
Process.table <- as.data.frame(table(x1))
Process.table <- Process.table[order(-Process.table$Freq),] 

x2 <- as.vector(Process.table$x1[1:15])
x2 <- subset(x1, x1 %in% x2)
x1 <- x2

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'EnvProcess.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## SPATIAL CONTEXT ####################

x  <- tstrsplit(data2$Spatial.cont, split="_")
x1 <- unlist(x)

# Corrections
x1[which(x1 %in% str_subset(x1, "ot clear")| x1 %in% str_subset(x1,"ot specified") | x1 %in% str_subset(x1,"unclear")| x1 =="Yes")]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "other")| x1 %in% str_subset(x1, "Other") )]   <- "Other"
x1[which(x1 %in% str_subset(x1, "ot relevant"))]   <- "Not relevant"
x1[which(x1 =="NOT RELEVANT")]   <- "Not relevant"
x1[which(x1 =="No")]             <- "No spatial context"

Spatial.tab <- data.frame(Spatial= x1, 
                          SW.ID = rep(data2$SW.ID, length(x)))
Spatial.tab <- subset(Spatial.tab, !is.na(Spatial))

x1 <- subset(x1, !is.na(x1))
Spat.table <- as.data.frame(table(x1))
Spat.table <- Spat.table[order(-Spat.table$Freq),]

# Generate the layout
packing <- circleProgressiveLayout(table(x1), sizetype='area')
packing$radius <- 0.95*packing$radius
species <- cbind(table(x1), packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = species, aes(x, y, size=1.5, label = x1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_fixed(ratio=0.6)
dev.copy(png, file=paste0(outPath, 'SpatialCont.png'), width=25, height=20, unit='cm', res=300)
dev.off()

## SCALES temporal ####################
x                    <- tstrsplit(data2$Scale...Temporal, split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "Equilibrium"))]    <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"

ScaleTemp.tab <- data.frame(TemporalScale = x1, 
                        SW.ID = rep(data2$SW.ID, length(x)))
ScaleTemp.tab <- subset(ScaleTemp.tab, !is.na(TemporalScale))

x1                   <- subset(x1, !is.na(x1))
Scale.Temporal.table <- as.data.frame(table(x1))
Scale.Temporal.table <- Scale.Temporal.table[order(-Scale.Temporal.table$Freq),]
Scale.Temporal.table$prop <- round((Scale.Temporal.table$Freq/sum(Scale.Temporal.table$Freq))*100,1)
colnames(Scale.Temporal.table) <- c('Temporal scale', 'Number of papers', '% of studies')

p <- tableGrob(Scale.Temporal.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'ScaleTemporal.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Scale.Temporal.table, file=paste0(outPath, 'ScaleTemporal.csv'), row.names=F)

## SCALE spatial #####################
x                    <- tstrsplit(data2$Scale...Spatial..m., split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"

ScaleSpat.tab <- data.frame(SpatialScale = x1, 
                            SW.ID = rep(data2$SW.ID, length(x)))
ScaleSpat.tab <- subset(ScaleSpat.tab, !is.na(SpatialScale))

x1                  <- subset(x1, !is.na(x1))
Scale.Spatial.table <- as.data.frame(table(x1))
Scale.Spatial.table <- Scale.Spatial.table[order(-Scale.Spatial.table$Freq),]
Scale.Spatial.table$prop <- round((Scale.Spatial.table$Freq/sum(Scale.Spatial.table$Freq))*100,1)
colnames(Scale.Spatial.table) <- c('Spatial scale', 'Number of papers', '% of studies')

p <- tableGrob(Scale.Spatial.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'ScaleSpatial.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Scale.Spatial.table, file=paste0(outPath, 'ScaleSpatial.csv'), row.names=F)

## RESOLUTION spatial. ##################
x                    <- tstrsplit(data2$Resolution...Spatial..m., split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "ot specified"))]   <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "in degrees"))]     <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"
x1[which(x1=="?")]                                  <- "Not clear"
x1[which(x1 == "Not provided")]                     <- "Not clear"
x1[which(x1 == "not clear")]                        <- "Not clear"

ResolSpat.tab <- data.frame(SpatialResolution = x1, 
                            SW.ID = rep(data2$SW.ID, length(x)))
ResolSpat.tab <- subset(ResolSpat.tab, !is.na(SpatialResolution))

x1                  <- subset(x1, !is.na(x1))
Resolution.Spatial.table <- as.data.frame(table(x1))
Resolution.Spatial.table <- Resolution.Spatial.table[order(-Resolution.Spatial.table$Freq),]
Resolution.Spatial.table$prop <- round((Resolution.Spatial.table$Freq/sum(Resolution.Spatial.table$Freq))*100,1)
colnames(Resolution.Spatial.table) <- c('Spatial resolution', 'Number of papers', '% of studies')

p <- tableGrob(Resolution.Spatial.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'ResolutionSpatial.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Resolution.Spatial.table, file=paste0(outPath, 'ResolutionSpatial.csv'), row.names=F)

## RESOLUTION temporal ##################
x                    <- tstrsplit(data2$Resolution...Temporal, split="_")
x1                   <- unlist(x)

# corrections
x1[which(x1 %in% str_subset(x1, "NOT RELEVANT"))]   <- "Not relevant"
x1[which(x1 %in% str_subset(x1, "snapshot"))]       <- "Not relevant"
x1[which(x1 == "not clear")]                        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "unclear"))]        <- "Not clear"
x1[which(x1 %in% str_subset(x1, "multiple"))]       <- "Not relevant"

ResolTemp.tab <- data.frame(TemporalResolution = x1, 
                            SW.ID = rep(data2$SW.ID, length(x)))
ResolTemp.tab <- subset(ResolTemp.tab, !is.na(TemporalResolution))

x1                  <- subset(x1, !is.na(x1))
Resolution.Temporal.table <- as.data.frame(table(x1))
Resolution.Temporal.table <- Resolution.Temporal.table[order(-Resolution.Temporal.table$Freq),]
Resolution.Temporal.table$prop <- round((Resolution.Temporal.table$Freq/sum(Resolution.Temporal.table$Freq))*100,1)
colnames(Resolution.Temporal.table) <- c('Temporal resolution', 'Number of papers', '% of studies')

p <- tableGrob(Resolution.Temporal.table,rows = NULL)
grid.arrange(p)
dev.copy(png, file=paste0(outPath, 'ResolutionTemporal.png'), width=25, height=20, unit='cm', res=300)
dev.off()

write.csv(Resolution.Temporal.table, file=paste0(outPath, 'ResolutionTemporal.csv'), row.names=F)


# QUALITY METHODS ##################

#---- quality spatial
x <- data2$Quality...Spatial..relative.1.3.

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"

Q.Spat.table <- as.data.frame(table(x))
Q.Spat.table <- Q.Spat.table[order(-Q.Spat.table$Freq),]
Q.Spat.table$prop <- round((Q.Spat.table$Freq/sum(Q.Spat.table$Freq))*100,1)

x1 <- Q.Spat.table[, 3]
names(x1) <-  Q.Spat.table[,1]
dotchart(sort(x1), col=colors, pt.cex = 3, cex =1.1, cex.main=1, main ='Quality spatial')

x2 <- x1[c(3, 1, 4, 2)]

#---- quality temporal
x <- data2$Quality...Temporal

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"

Q.Temp.table <- as.data.frame(table(x))
Q.Temp.table <- Q.Temp.table[order(-Q.Temp.table$Freq),]
Q.Temp.table$prop <- (Q.Temp.table$Freq/sum(Q.Temp.table$Freq))*100

x1 <- Q.Temp.table[, 3]
names(x1) <-  Q.Temp.table[,1]
dotchart(sort(x1), col=colors[2:5], pt.cex = 3, cex =1.1, cex.main=1, main ='Quality temporal')

x3 <- x1[c(3, 1, 4, 2)]


#---- quality method
x <- data2$Quality...Methods

# corrections
x[which(x %in% str_subset(x, "NOT RELEVANT"))]   <- "Not available"
x[which(x %in% str_subset(x, "ot relevant"))]   <- "Not available"

QMethod.tab <- data.frame(qmethod = x, 
                         SW.ID = data2$SW.ID)
QMethod.tab <- subset(QMethod.tab, !is.na(qmethod))

Q.Meth.table <- as.data.frame(table(x))
Q.Meth.table <- Q.Meth.table[order(-Q.Meth.table$Freq),]
Q.Meth.table$prop <- (Q.Meth.table$Freq/sum(Q.Meth.table$Freq))*100

x1 <- Q.Meth.table[, 3]
names(x1) <-  Q.Meth.table[,1]
dotchart(sort(x1), col=colors[2:5], pt.cex = 3, cex =1.1, cex.main=1, main ='Quality Method')

x4 <- x1[c(1, 2, 4, 3)]

# Make barchart
par(mfrow=c(1,3), oma=c(1, 3, 1, 1))
barplot(x2, horiz =T, main='Quality Spatial', las=2)
barplot(x3, horiz =T, main='Quality Temporal', las=2)
mtext(text = "% of unique papers",  side = 1, line = 2.8, at=25, cex=0.9)
barplot(x4, horiz =T, main='Quality Method',las=2)
dev.copy(png, file=paste0(outPath, 'Quality_barplot.png'), width=20, height=7, unit='cm', res=300)
dev.off()

####################################################
#  Make Relational plots
####################################################

# First merge tabs by SW.ID

ModelIndicator  <- merge(Model.tab, Indicator.tab, all.x=T, all.y=T)
ModelDecade     <- merge(Decade.tab, Model.tab, all.x=T, all.y=T)
RegionIndicator <- merge(Indicator.tab, Region.tab, all.x=T, all.y=T)
RegionModel     <- merge(Model.tab, Region.tab, all.x=T, all.y=T)
RegionYear      <- merge(Year.tab, Region.tab, all.x=T, all.y=T)
RegionDecade    <- merge(Decade.tab, Region.tab, all.x=T, all.y=T)
TempResScale    <- merge(ResolTemp.tab, ScaleTemp.tab, all.x=T, all.y=T)
SpatResScale    <- merge(ResolSpat.tab, ScaleSpat.tab, all.x=T, all.y=T)
ModelScale      <- merge(ScaleSpat.tab, Model.tab, all.x=T, all.y=T)
UmanRegion      <- merge(Region.tab, UsedMan.tab, all.x=T, all.y=T)
ManRegion       <- merge(Region.tab, Man.tab, all.x=T, all.y=T)
ManIndicator    <- merge(Man.tab, Indicator.tab, all.x=T, all.y=T)
ManModel        <- merge(Man.tab, Model.tab, all.x=T, all.y=T)
TaskRegion      <- merge(Task.tab, Region.tab, all.x=T, all.y=T)
TaskModel       <- merge(Task.tab, Model.tab, all.x=T, all.y=T)
ProcessModel    <- merge(Process.tab, Model.tab, all.x=T, all.y=T)
QmethodRegion   <- merge(QMethod.tab, Region.tab, all.x=T, all.y=T)
UmanModel       <- merge(Model.tab, UsedMan.tab, all.x=T, all.y=T)
SpatialModel    <- merge(Model.tab, Spatial.tab, all.x=T, all.y=T)
  
# ------- Year published with Region 

df  <- as.data.frame(table(RegionYear[,2:3]))

ggplot(df, aes(x=year, y=Freq, group=region) )+
      geom_line(aes(color=region))+
      geom_point(aes(color=region))+
      theme_light()+
      ylab('Number of retained papers')+
      xlab('Publication year')+
      scale_x_discrete(breaks=seq(1981, 2022, 5))
dev.copy(png, file=paste0(outPath, 'YearRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  
  
# ------- Indicator type by region

data.new <- as.data.table(RegionIndicator)
data.new <- subset(data.new, !is.na(indicator))
data.new <- subset(data.new, indicator!='Not specified' & indicator!='Not relevant')
colnames(data.new) <- c('SW.ID', 'Indicator', 'Region')
data.new2 <- data.new %>%
  make_long(Region, Indicator)


ggplot(data.new2, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'IndicatorRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# ------- Model by region

data.new <- as.data.table(RegionModel)
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Model', 'Region')
data.new3 <- data.new %>%
  make_long(Region, Model)

ggplot(data.new3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ModelRegion.png'), width=18, height=18, unit='cm', res=300)
dev.off()  

# ------- Used in Management by region

data.new <- as.data.table(UmanRegion)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, Useman!='Not clear')
colnames(data.new) <- c('SW.ID', 'Region', 'Used_in_Management')
data.new5 <- data.new %>%
  make_long(Region, Used_in_Management)

ggplot(data.new5, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'UsemanRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# -------Management by region

data.new <- as.data.table(ManRegion)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
colnames(data.new) <- c('SW.ID', 'Region', 'Management')
data.new6 <- data.new %>%
  make_long(Region, Management)

ggplot(data.new6, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ManRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  


# ------- Model by spatial scale

data.new <- as.data.table(ModelScale)
data.new <- subset(data.new, !is.na(SpatialScale))
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'SpatialScale',  'Model')
data.new$order <- rep(NA, nrow(data.new))
data.new$order[which(data.new$SpatialScale=='>100,000')]       <- 1
data.new$order[which(data.new$SpatialScale=='50,000-100,000')] <- 2
data.new$order[which(data.new$SpatialScale=='10,000-50,000')]  <- 3
data.new$order[which(data.new$SpatialScale=='5,000-10,000')]   <- 4
data.new$order[which(data.new$SpatialScale=='1,000-5,000')]    <- 5
data.new$order[which(data.new$SpatialScale=='500-1,000')]      <- 6
data.new$order[which(data.new$SpatialScale=='Not relevant')]   <- 7
data.new <- data.new[order(data.new$order),]
data.new4 <- data.new %>%
  make_long(SpatialScale, Model)

ggplot(data.new4, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ModelScale.png'), width=18, height=18, unit='cm', res=300)
dev.off()  

# -------Management by Indicator

data.new <- as.data.table(ManIndicator)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, indicator!='Not specified')
data.new <- subset(data.new, indicator!='Not relevant')
colnames(data.new) <- c('SW.ID', 'Management', 'Indicator')
data.new7 <- data.new %>%
  make_long(Management, Indicator)

ggplot(data.new7, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ManIndicator.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# -------Management by Model

data.new <- as.data.table(ManModel)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Management', 'Model')
data.new8 <- data.new %>%
  make_long(Management, Model)

ggplot(data.new8, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ManModel.png'), width=18, height=18, unit='cm', res=300)
dev.off()  


# -------Process by Model
data.new <- as.data.table(ProcessModel)
data.new <- subset(data.new, !is.na(Process))
data.new <- subset(data.new, Process!='Not clear')
data.new <- subset(data.new, Process %in% Process.table$x1[1:15])
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Process', 'Model')
data.new13 <- data.new %>%
  make_long(Process, Model)

ggplot(data.new13, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ProcessModel.png'), width=18, height=15, unit='cm', res=300)
dev.off()  

# -------Spatially explicit, Process and  Model
df1 <-  merge(ProcessModel, SpatialModel, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(model))
data.new <- subset(data.new, !is.na(Process))
data.new <- subset(data.new, Process!='Not clear')
data.new <- subset(data.new, Process %in% Process.table$x1[1:15])
data.new <- subset(data.new, Spatial!='Other' & Spatial!='Not relevant'  & Spatial!='No spatial context' & Spatial!='Not clear' & Spatial!='Not specified')
colnames(data.new) <- c('SW.ID', 'Model', 'Process', 'Spatial_context')
data.new20 <- data.new %>%
  make_long(Spatial_context, Model, Process)

ggplot(data.new20, aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'SpatialContProcessModel.png'), width=18, height=15, unit='cm', res=300)
dev.off()  

# ----- Task region
data.new <- as.data.table(TaskRegion)
data.new <- subset(data.new, task != "Task 6.6 - Fisheries in the socio-ecological system")
data.new <- subset(data.new, task != "Task")
data.new <- subset(data.new, task != "management strategies")
#data.new <- subset(data.new, region!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Task', 'Region')
data.new10 <- data.new %>%
  make_long(Region, Task)

ggplot(data.new10, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'TaskRegion.png'), width=18, height=10, unit='cm', res=300)
dev.off()  

# ----- Task model
data.new <- as.data.table(TaskModel)
data.new <- subset(data.new, task != "Task 6.6 - Fisheries in the socio-ecological system")
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Task', 'Model')
data.new9 <- data.new %>%
  make_long(Model, Task)

ggplot(data.new9, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'ModelTask.png'), width=18, height=18, unit='cm', res=300)
dev.off()  

# ------  Region, Management, Indicator

df1 <-  merge(RegionIndicator, ManRegion, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, indicator!='Not relevant')
data.new <- subset(data.new, indicator!='Not specified')
#data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Region', 'Indicator', 'Management')
data.new15 <- data.new %>%
  make_long(Management, Region, Indicator)

ggplot(data.new15, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'Management_Region_Indicator.png'), width=22, height=18, unit='cm', res=300)
dev.off()  

# ------  Model, Management, Indicator

df1 <-  merge(ManModel, ManIndicator, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(man))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, indicator!='Not relevant')
data.new <- subset(data.new, indicator!='Not specified')
data.new <- subset(data.new, model!='Other' & model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID','Management', 'Model', 'Indicator')
data.new15 <- data.new %>%
  make_long(Model, Management, Indicator)

ggplot(data.new15, aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'Model_Management_Indicator.png'), width=22, height=18, unit='cm', res=300)
dev.off()  

# ------  Used Man, Region, Model

df1 <-  merge(UmanRegion, RegionModel, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, Useman!='Not clear' & Useman!='ICES')
data.new <- subset(data.new, region!='Not relevant')
data.new <- subset(data.new, model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Region', 'Used_in_Management', 'Model')
data.new16 <- data.new %>%
  make_long(Used_in_Management, Region, Model)

ggplot(data.new16, aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'Region_Usedman_Model1.png'), width=22, height=19, unit='cm', res=300)
dev.off()  

# ------  Used Man, Model (without No Used man category)

df1 <-  merge(UmanModel, ManModel, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, Useman!='Not clear' &  Useman!='ICES')
data.new <- subset(data.new, Useman!='No')
data.new <- subset(data.new, Useman!='Only at a case study level')
data.new <- subset(data.new, model!='Not relevant'  & model!='Not specified' & model!='No model')
colnames(data.new) <- c('SW.ID', 'Model', 'Used_in_Management', 'Management')
data.new19 <- data.new %>%
  make_long(Used_in_Management, Model, Management)

ggplot(data.new19, aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'Usedman_Model_Man4.png'), width=22, height=15, unit='cm', res=300)
dev.off()  


# ------  Used Man, Model (without No Used man category)

df1 <-  merge(UmanModel, ManModel, all.x=T, all.y=T)

data.new <- as.data.table(df1)
data.new <- subset(data.new, !is.na(Useman))
data.new <- subset(data.new, man!='Not relevant')
data.new <- subset(data.new, Useman!='No' & Useman!='ICES')
data.new <- subset(data.new, Useman!='Only at a case study level')
data.new <- subset(data.new, model!='Not relevant'  & model!='Not specified' & model!='No model')
data.new$model[which(data.new$model %in% c('ATLANTIS', 'BEMTOOL', 'GBFSM', 'ISIS-FISH', 'OSMOSE'))] <-'Other'
#data.new$model[which(data.new$model %in% c('ATLANTIS', 'BEMTOOL', 'GBFSM', 'ISIS-FISH', 'OSMOSE', 'ALADYM', 'MEFISTO', 'FISHRENT', 'ELFSIM'))] <-'Other'
colnames(data.new) <- c('SW.ID', 'Model', 'Used_in_Management', 'Management')
data.new19 <- data.new %>%
  make_long(Used_in_Management, Model, Management)

ggplot(data.new19, aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  #guides(fill = guide_legend(title = "Title") + 
  theme(legend.position = "none")+
  xlab("")
dev.copy(png, file=paste0(outPath, 'Usedman_Model_Man2.png'), width=22, height=15, unit='cm', res=300)
dev.off()  

# ----- Spatial and temporal resolution with scale

### Spatial scale vs resolution

x  <- as.data.frame(table(SpatResScale[,c(2:3)]))
x  <- x[order(-x$Freq),] 

# with ggplot
ggplot(x, aes(fill=SpatialResolution, y=Freq, x=reorder(SpatialScale, +Freq) )) + 
  geom_bar(position="stack", stat="identity")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('Number of retained papers')+
  xlab('Spatial scale')+
  ggtitle ('Spatial scale and resolution of retained papers')+
  scale_fill_viridis_d()
dev.copy(png, file=paste0(outPath, "Spatialscale&resolution.png") , width = 18, height = 13, unit='cm', res=100)
dev.off()

### Temporal scale vs resolution
x  <- as.data.frame(table(TempResScale[,c(2:3)]))
x  <- x[order(-x$Freq),] 

# with ggplot
ggplot(x, aes(fill=TemporalResolution, y=Freq, x=reorder(TemporalScale, +Freq) )) + 
  geom_bar(position="stack", stat="identity")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('Number of retained papers')+
  xlab('Temporal scale')+
  ggtitle ('Temporal scale and resolution of retained papers')+
  scale_fill_viridis_d()
dev.copy(png, file=paste0(outPath, "Temporalscale&resolution.png") , width = 18, height = 13, unit='cm', res=100)
dev.off()

# with simple plot
#x1 <- dcast(x, SpatialResolution~SpatialScale)
#rownames(x1) <-  x1[,1]
#x1 <- x1[,-1]
#x1 <- as.matrix(x1)

# with simple plot
#tiff(paste0(outPath, "Temporalscale&resolution6.tiff"), width = 1000, height = 1000, res=100)
#layout(mat = (matrix(nrow=1, ncol=2, data=c(1,2))), widths = c(6,4), heights = c(1,1))
# plot 1: small scales
#par(mar=c(10,5,8,0))
#b <- barplot(x1)
#b <- barplot(x1, ylim=c(0,450), axes=F, names.arg=rep("", 10), width=1, xlim=c(0,13), col=viridis(15))
#axis(2, at=seq(0,450,100), cex.axis=1.2, las=1, pos=0)
#axis(1, at=b, colnames(x1), las=3)
#axis(2, at=250, "Number of papers retained", cex.axis=1.5, tick=F, line=1.5)
#abline(v=9.7, lty=3, col="dimgrey")
#dev.copy(png, file=paste0(outPath, "Temporalscale&resolution.png") , width = 20, height = 20, unit='cm', res=100)
#dev.off()



### TREEMAPS #######################################


# -------- model with region

x <- table(RegionModel$region, RegionModel$model)
x <- as.data.frame(x)

x1 <- subset(x, Var2!='Other')
x1 <- subset(x1, Var1!='Not relevant')
x1 <- subset(x1, Var2!='Not specified')
treemap(x1, index=c("Var1","Var2"), vSize="Freq", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels= 100,              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)
dev.copy(png, file=paste0(outPath, "Treemap_model_region.png") , width = 18, height = 18, unit='cm', res=100)
dev.off()

# -------- model with scale

x <- table(ModelScale$SpatialScale, ModelScale$model)
x <- as.data.frame(x)

x1 <- subset(x, Var2!='Other')
x1 <- subset(x1, Var1!='Not relevant')
x1 <- subset(x1, Var2!='Not specified')
treemap(x1, index=c("Var1","Var2"), vSize="Freq", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels= 100,              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)
dev.copy(png, file=paste0(outPath, "Treemap_model_scale.png") , width = 18, height = 18, unit='cm', res=100)
dev.off()

### HEATMAPS #######################################

# -------Region Model

x <- table(RegionModel$region, RegionModel$model)
x <- as.data.frame(x)
x <- subset(x, Var1!='Arctic')   # not model in this case

x1 <- subset(x, Var2!='Other')
x1 <- subset(x1, Var1!='Not relevant')
x1 <- subset(x1, Var2!='Not specified')

# find proportions by region
x2 <- dcast(x1, Var1~Var2)
SUMS <- rowSums(x2[,2:ncol(x2)])
x3   <- x2
x3[, 2:ncol(x2)] <- x3[, 2:ncol(x2)] /SUMS
rowSums(x3[,2:ncol(x3)])
x4 <- melt(x3)

ggplot(x1, aes(Var1, Var2, fill= Freq)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
#  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") 
dev.copy(png, file=paste0(outPath, "Heatmap_model_region.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

ggplot(x4, aes(Var1, variable, fill= value)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") 
dev.copy(png, file=paste0(outPath, "Heatmap_model_region_prop.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()


# -------- Management Region

x <- table(ManRegion$region, ManRegion$man)
x <- as.data.frame(x)
x1 <- subset(x, Var2!='Not relevant')   # no management

# find proportions by region
x2 <- dcast(x1, Var1~Var2)
SUMS <- rowSums(x2[,2:ncol(x2)])
x3   <- x2
x3[, 2:ncol(x2)] <- x3[, 2:ncol(x2)] /SUMS
rowSums(x3[,2:ncol(x3)])
x4 <- melt(x3)

ggplot(x1, aes(Var1, Var2, fill= Freq)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") 
dev.copy(png, file=paste0(outPath, "Heatmap_Man_region.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

ggplot(x4, aes(Var1, variable, fill= value)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") 
dev.copy(png, file=paste0(outPath, "Heatmap_Man_region_prop.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()


# ------- Model with scale 

x <- table(ModelScale$model, ModelScale$SpatialScale)
x <- as.data.frame(x)

x1 <- subset(x, Var1!='Other')   # not model in this case
x1 <- subset(x1, Var1!='Not specified')   # not model in this case
x1 <- subset(x1, Var1!='No model')   # not model in this case
x1 <- subset(x1, Var2!='not clear')
x1 <- subset(x1, Var2!='Not relevant')

x1$order <- rep(NA, nrow(x1))
x1$order[which(x1$Var2=='>100,000')]       <- 1
x1$order[which(x1$Var2=='50,000-100,000')] <- 2
x1$order[which(x1$Var2=='10,000-50,000')]  <- 3
x1$order[which(x1$Var2=='5,000-10,000')]   <- 4
x1$order[which(x1$Var2=='1,000-5,000')]    <- 5
x1$order[which(x1$Var2=='500-1,000')]      <- 6
x1 <- x1[order(x1$order),]

ggplot(x1, aes(x=reorder(Var2, +order), y=Var1, fill= Freq)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.copy(png, file=paste0(outPath, "Heatmap_model_scale.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

# find proportions by region
x2 <- dcast(x1[,1:3], Var1~Var2)
SUMS <- rowSums(x2[,2:ncol(x2)])
x3   <- x2
x3[, 2:ncol(x2)] <- x3[, 2:ncol(x2)] /SUMS
rowSums(x3[,2:ncol(x3)])
x4 <- melt(x3)
x4$order <- rep(NA, nrow(x4))
x4$order[which(x4$variable=='>100,000')]       <- 1
x4$order[which(x4$variable=='50,000-100,000')] <- 2
x4$order[which(x4$variable=='10,000-50,000')]  <- 3
x4$order[which(x4$variable=='5,000-10,000')]   <- 4
x4$order[which(x4$variable=='1,000-5,000')]    <- 5
x4$order[which(x4$variable=='500-1,000')]      <- 6
x4 <- x4[order(x4$order),]

ggplot(x4, aes(y=Var1, x=reorder(variable, +order), fill= value)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.copy(png, file=paste0(outPath, "Heatmap_model_scale_prop.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

# find proportions by model
x2 <- dcast(x1[,1:3], Var2~Var1)
SUMS <- colSums(x2[,2:ncol(x2)])
x3   <- x2
for (i in 2:ncol(x2)){
  x3[, i] <- x3[,i]/SUMS[i-1]
}
#x3[, 2:ncol(x2)] <- x3[, 2:ncol(x2)] /SUMS
colSums(x3[,2:ncol(x3)])
x4 <- melt(x3)
x4$order <- rep(NA, nrow(x4))
x4$order[which(x4$Var2=='>100,000')]       <- 1
x4$order[which(x4$Var2=='50,000-100,000')] <- 2
x4$order[which(x4$Var2=='10,000-50,000')]  <- 3
x4$order[which(x4$Var2=='5,000-10,000')]   <- 4
x4$order[which(x4$Var2=='1,000-5,000')]    <- 5
x4$order[which(x4$Var2=='500-1,000')]      <- 6
x4 <- x4[order(x4$order),]

ggplot(x4, aes(y=variable, x=reorder(Var2, +order), fill= value)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.copy(png, file=paste0(outPath, "Heatmap_model_scale_prop1.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

# -------- Quality method vs region

x <- table(QmethodRegion$region, QmethodRegion$qmethod)
x <- as.data.frame(x)
x1 <- subset(x, Var2!='Not relevant')  
x1 <- subset(x, Var2!='Not available') 

# find proportions by region
x2 <- dcast(x1, Var1~Var2)
SUMS <- rowSums(x2[,2:ncol(x2)])
x3   <- x2
x3[, 2:ncol(x2)] <- x3[, 2:ncol(x2)] /SUMS
rowSums(x3[,2:ncol(x3)])
x4 <- melt(x3)

ggplot(x1, aes(Var1, Var2, fill= Freq)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") 
dev.copy(png, file=paste0(outPath, "Heatmap_Qmethod_region.png") , width = 18, height = 14, unit='cm', res=100)
dev.off()

ggplot(x4, aes(Var1, variable, fill= value)) + 
  geom_tile()+
  xlab("")+
  ylab("")+
  #  scale_fill_distiller()
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.copy(png, file=paste0(outPath, "Heatmap_Qmethod_region_prop.png") , width = 14, height = 12, unit='cm', res=100)
dev.off()


#######  MAP of World #############################

library(rworldmap)
#get coarse resolution world from rworldmap
sPDF <- getMap()  
#mapCountries using the 'continent' attribute  
mapCountryData(sPDF, nameColumnToPlot='REGION', colourPalette=viridis(7))

################

library(sf)
library(tidyverse)
library(ggplot2)

# load shape files
# download.file("http://naciscdn.org/naturalearth/packages/natural_earth_vector.zip",
#               "world maps.zip")
 
# unzip("world maps.zip",
#       exdir = "Raw maps from zip")

# I dropped the part of downloading the shapefile here. See solution 1 for that.

world = read_sf(dsn   = "Raw maps from zip\\110m_cultural",
                layer = "ne_110m_admin_0_countries")

# Next we just do some tidy magic and group the data by CONTINENT and get the respective coordinates in a long list
continents = world %>%
  group_by(CONTINENT) %>%
  summarise(.)

# Here I create some dummy survey results
results = data.frame(CONTINENT      = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                     kpi         = c(20, 30, 50, 50, 60, 70),
                     continent_long = c(15, 80, 20, -100, 150, -60),
                     continent_lat  = c(15, 35, 50, 40, -25, -15),
                     stringsAsFactors = F)

# Now let's join the continent data with the results
world_for_plot = continents %>%
  left_join(., results, by = c("CONTINENT")) %>%
  filter(!is.na(kpi))

### Now we can plot the results.

# Let's create the plot first with data and let's care about the labels later
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent"),
  plot.title = element_text(hjust = 0.5)
)

# This is the actual results plot with different colours based on the results
raw_plot = ggplot(data = world_for_plot) +
  geom_sf(aes(fill = kpi),
          colour=NA) +
  coord_sf() +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  plain

# Now we can add the labels
final_plot = raw_plot +
  geom_sf_text(aes(label=CONTINENT))

# We could also use our own label positions
final_plot = raw_plot +
  geom_text(aes(label = CONTINENT,
                x = continent_long,
                y = continent_lat,
                group = CONTINENT))