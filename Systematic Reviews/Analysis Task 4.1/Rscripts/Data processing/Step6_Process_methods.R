#####################################################################################################################-
#####################################################################################################################-
#
#     Step 6. Process methods
#
#     By Esther Beukhof
#     Dec 2022
#
#####################################################################################################################-
#####################################################################################################################-
rm(list = ls())

#-----------------------------------------------#
# Load libraries and set Paths.
#-----------------------------------------------#
library(data.table)
library(splitstackshape)
library(stringr)

datPath                               <- "Systematic Reviews/Analysis Task 4.1/Routput/"

#-----------------------------------------------#
# Read in data
#
#  info:
#  This section depends on the processed data file produced in step 5.
#-----------------------------------------------#
data                                  <- readRDS(file=paste0(datPath, "data_correctTaxa_PressVar_RespVar.rds"))
data_allScreened                      <- readRDS(file=paste0(datPath, "data_allScreened_correctTaxa_PressVar_RespVar.rds"))

# Remove some columns to improve viewing the data frame
data_allScreened$SearchID <- data_allScreened$Open.Access <- NULL

# data                                  <- data_allScreened #in case you'd like to run with the entire dataset (incl. all columns)


#-----------------------------------------------#
# Check Sampling Methods ----
#-----------------------------------------------#

# Create dataset where one row represents unique combination of SW.ID and Sampling method
datSampl           <- data
datSampl$samplID   <- paste(datSampl$SW.ID, datSampl$Sampling.Method.used.for.data.collection)
datSampl           <- datSampl[!duplicated(datSampl$samplID),]

# Check how often methods occur
as.data.frame(sort(table(datSampl$Sampling.Method.used.for.data.collection, useNA = "ifany"), decreasing = TRUE)) 
#mostly fisheries dependent data or fisheries independent surveys
#no NAs
#quite some cases where 'Other' has been chosen: 83 papers


#-----------------------------------------------#
# Check Description of Other Sampling Method ----
#-----------------------------------------------#

# Create dataset where one row represents unique combination of SW.ID, Sampling method and Description of Other Sampling method
datDescr           <- data
datDescr$samplID   <- paste(datDescr$SW.ID, datDescr$Sampling.Method.used.for.data.collection, datDescr$Description.Other.Sampling.Method)
datDescr           <- datDescr[!duplicated(datDescr$samplID),]


sort(unique(datDescr$Description.Other.Sampling.Method))

