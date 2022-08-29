################################################
#-----------------------------------------------
# Plot the fate of all records returned from searches
#
#  info:
#  This section is relatively stand-alone.  It creates sankey diagrams illustrating the fate of all records from search to extraction
#-----------------------------------------------
#===
# Data cleanin
#====
## Combine exclusion and inclusion reasons to integrated fate column
FatePapers$Extraction.Code <- NA
FatePapers[is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Code"] <- FatePapers[is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.WP4.task"]
FatePapers[!is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Code"] <- FatePapers[!is.na(FatePapers$Extraction.Exclusion.Code), "Extraction.Exclusion.Code"]

## Create long versions of both screening and full-text fates for each record
screenFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Screening.Code")],
                         splitCols = "Screening.Code",
                         sep = " _ ",
                         direction = "long")

ExtractFatelong <- cSplit(indt = FatePapers[, c("SW.ID", "Extraction.Code")],
                          splitCols = "Extraction.Code",
                          sep = " _ ",
                          direction = "long")
## Merge screening and extraction fates
fatelong <- merge(screenFatelong, ExtractFatelong, by = "SW.ID", all = TRUE)


## Rename some levels for consistency across screening and extraction phases
fatelong$Screening.Code[grepl("4.2", fatelong$Screening.Code)] <- "4.2"
fatelong$Screening.Code[grepl("4.3", fatelong$Screening.Code)] <- "4.3"
fatelong$Screening.Code[grepl("4.4", fatelong$Screening.Code)] <- "4.4"
fatelong$Screening.Code[grepl("4.5", fatelong$Screening.Code)] <- "4.5"
fatelong$Screening.Code[grepl("INCLUDE on title", fatelong$Screening.Code)] <- "4.general"

fatelong$Extraction.Code[grepl("None", fatelong$Extraction.Code)] <- "4.general"

## Determine pathways to different fates through screening and extraction
paths <- unique(fatelong[, c("Screening.Code", "Extraction.Code")])
paths <- paths[order(paths$Screening.Code, paths$Extraction.Code)]

s <- data.frame(Screening.Code = unique(paths$Screening.Code),
                Extraction.Code = rep("z", times = length(unique(paths$Screening.Code))),
                Label = paste0("S", 0:(length(unique(paths$Screening.Code))-1)))

ex <- data.frame(Screening.Code = paths[!grepl("EXCLUDE", paths$Screening.Code), "Screening.Code"],
                 Extraction.Code = paths[!grepl("EXCLUDE", paths$Screening.Code), "Extraction.Code"],
                 Label = paste0("E", 0:(nrow(paths[!grepl("EXCLUDE", paths$Screening.Code), "Extraction.Code"])-1)))

## Create dataframe of Nodes (manually enter first node)
nodes <- rbind(s, ex)
nodes <- rbind(data.frame(Screening.Code = "SearchResults",
                          Extraction.Code = "SearchResults",
                          Label = "A1"),
               nodes)
## Add Sankey attributes to nodes

nodes$colour <- c("#210384", rep("#037184", times = 5), rep("#86C64E", times = 7), rep(NA, times = nrow(nodes)-13))
nodes[is.na(nodes$colour) & grepl("4.", nodes$Extraction.Code), "colour"] <- "#00B292"
nodes[is.na(nodes$colour) & grepl("EXCLUDE", nodes$Extraction.Code), "colour"] <- "#C6E83E"
# nodes[is.na(nodes$colour) & grepl("SearchResults", nodes$Extraction.Code), "colour"] <- "#210384"

## Calculate values for links
links <- data.frame(source = c(rep(0, )))