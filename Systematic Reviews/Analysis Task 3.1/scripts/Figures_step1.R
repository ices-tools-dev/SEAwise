
#####################################################################################################################
#####################################################################################################################
##
### This file produces figures for the report about the first steps
##
##   By Marie Savina
##    March 2022
##
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

library(networkD3)
library(dplyr)
library(viridis)


setwd("C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/scripts/")
outPath <- "C:/Users/msavinar/Documents/SeaWise/Task 3.1/data extraction/data extraction/figures/"

colset <- viridis(8)

links <- data.frame(
source <- c("Screening 2050",	"Screening 2050",	"Screening 2050",	"Reading 802",	"Reading 802",	"Superscreening 562",	"Superscreening 562"),
target <-	c("Reading 802",	"Rejection 1534",	"Superscreening 562",	"Final database 516",	"Rejection 1534",	"Reading 802",	"Rejection 1534"),
value	<- c(633,	855,	562,	516,	286,	169,	393)
)
colnames(links) <- c("source", "target", "value")

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

my_color <- 'd3.scaleOrdinal() .domain(["Screening",	"Reading",	"Superscreening", "Final database",	"Rejection"	]) .range(["#440154FF", "#3B528BFF", "#21908CFF", "#FDE725FF", "#FDE725FF"])'


sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
              Target = "IDtarget", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30, colourScale = my_color)

# save the widget
#library(htmlwidgets)
# saveWidget(p, file=paste0(outPath, "dataextraction.html"))


