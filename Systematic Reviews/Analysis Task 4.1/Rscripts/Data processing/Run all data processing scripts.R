#####################################################################################################################
#####################################################################################################################
#
#     Run all processing scripts at once
#
#     By Esther D. Beukhof
#     June 2024
#
#####################################################################################################################
#####################################################################################################################
rm(list = ls())

#-----------------------------------------------
# Load libraries and set Paths
#-----------------------------------------------

# Set paths
scriptPath                               <- "Systematic Reviews/Analysis Task 4.1/Rscripts/Data processing/"

# Create local environment for sourcing of scripts - this avoids that all produced objects end up in the global environment
localEnv                          <- new.env()


#-----------------------------------------------
# IMPORTANT NOTE 
#-----------------------------------------------
# Always run the scripts on the cropped dataset BEFORE running it on the full dataset.
# This is due to some dependencies in script 3.


#-----------------------------------------------
# Run first processing script
#-----------------------------------------------

source(paste0(scriptPath,"Step1_Load_merge_data_extraction_files.R"), localEnv)


#-----------------------------------------------#
# Run remaining scripts on cropped dataset (fewer columns)
#-----------------------------------------------#

# Choose dataset to be produced
dataset                           <- "cropped"

# Run scripts
source(paste0(scriptPath,"Step2_Initial_data_processing.R"), localEnv)
source(paste0(scriptPath,"Step3_Species_studied.R"), localEnv)
source(paste0(scriptPath,"Step4_Categorize_pressure_variables.R"), localEnv)
source(paste0(scriptPath,"Step5_Process_response_variables.R"), localEnv)
source(paste0(scriptPath,"Step6_Process_methods.R"), localEnv)


#-----------------------------------------------#
# Run remaining scripts on full dataset
#-----------------------------------------------#

# Choose dataset to be produced
dataset                           <- "full"

# Run scripts
source(paste0(scriptPath,"Step2_Initial_data_processing.R"), localEnv)
source(paste0(scriptPath,"Step3_Species_studied.R"), localEnv)
source(paste0(scriptPath,"Step4_Categorize_pressure_variables.R"), localEnv)
source(paste0(scriptPath,"Step5_Process_response_variables.R"), localEnv)
source(paste0(scriptPath,"Step6_Process_methods.R"), localEnv)


#-----------------------------------------------
# Run last processing script
#-----------------------------------------------

source(paste0(scriptPath,"Step7_Check_scale_resolution.R"), localEnv)
