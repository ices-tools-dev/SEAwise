#####################################################################################################################
#####################################################################################################################
#
#     Script to read and merge, and analyse data extraction files from SEAwise task 4.1
#     Step 1. Read and merge data extraction files.
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
library(openxlsx)


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
#  Note: I manually switched rows 1 and 2 in the Binch-file, otherwise the file is not read correctly.
#  Note: I manually restored some of the column-names in the file for Thorpe.
#-----------------------------------------------
readers                                <- c("Altuna-Etxabe", "Anastasopoulou", "Astarloa", "Basurko", "Binch", "Bluemel", "Brown",
                                            "Carbonara", "Dinesen", "Festjens", "garcia", "Girardin", "Halouani", "Lefkaditou_Chatzispyrou", "MacMillan", "Papadopoulou", "Potier",
                                            "Romagnoni", "Seghers", "Smith", "Spedicato", "Thorpe", "Thuenen","Tsagarakis", "Uhlmann_Reid", "VanHoey", "vdReijden")

## Create a table from the first reader
people                                 <- readers[1]
tab                                    <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1)
colnames(tab)
tab$Reader                             <- people

## Add all other readers
for(people in readers[2:length(readers)]) {
  print(people)
  tab1                                 <- read.xlsx(file=paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, header = TRUE, sheetIndex = 1)
  tab1$Reader                          <- people
  tab2                                 <- tab1[,colnames(tab1) %in% colnames(tab)] # remove empty / additional columns
  tab2                                 <- subset(tab2, !is.na(tab2$SW.ID)==T) # remove empty rows
  tab                                  <- rbind(tab,tab2)
} # end people-loop

## In case package 'xlsx' doesn't work: same but with another package
if(!"xlsx" %in% .packages()){
  ## Create a table from the first reader
  people                                 <- readers[1]
  tab                                    <- read.xlsx(xlsxFile = paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, colNames = TRUE, sheet = 1)
  colnames(tab)
  tab$Reader                             <- people

  ## Add all other readers
  for(people in readers[2:length(readers)]) {
    print(people)
    tab1                                 <- read.xlsx(xlsxFile = paste0(datPath, "DataExtractionForm_WP4_", people,".xlsx"), startRow = 2, colNames = TRUE, sheet = 1)
    tab1$Reader                          <- people
    tab2                                 <- tab1[,colnames(tab1) %in% colnames(tab)] # remove empty / additional columns
    tab2                                 <- subset(tab2, !is.na(tab2$SW.ID)==T) # remove empty rows
    tab                                  <- rbind(tab,tab2)
  } # end people-loop

  ## Correct some column names so they are the same as if read with 'xlsx' package
  names(tab)
  names(tab)[c(20:23)]                   <- c("Scale...Spatial..m.","Scale...Temporal","Resolution...Spatial..m.","Resolution...Temporal")
  names(tab)[c(27:29)]                   <- c("Quality...Spatial..relative.1.3.","Quality...Temporal","Quality...Methods")
  names(tab)[38]                         <- "Species.taxonomic.group.s."

}

# Save as rds-file
saveRDS(tab, paste0(outPath, "tab.rds"))
