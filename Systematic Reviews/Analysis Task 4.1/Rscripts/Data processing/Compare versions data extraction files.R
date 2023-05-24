library(openxlsx)

BinchSP <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_Binch_nowOnSP.xlsx", sheet=1, startRow = 2)
BinchGH <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_Binch_nowOnGH.xlsx", sheet=1, startRow = 2)

identical(BinchSP,BinchGH)

identical(BinchGH$Pressure_variable, BinchSP$Pressure_variable)
identical(BinchGH$Response.variable_paper, BinchSP$Response.variable_paper)
identical(BinchGH$Response.variable_category, BinchSP$Response.variable_category)
identical(BinchGH$Magnitude.of.relationship, BinchSP$Magnitude.of.relationship)

PressVar <- data.frame(GH=BinchGH$Pressure_variable, SP=BinchSP$Pressure_variable)



VdRSP <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_vdReijden_nowOnSP.xlsx", sheet=1, startRow = 2)
VdRSP <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_vdReijden_nowOnSP2.xlsx", sheet=1, startRow = 2)
VdRGH <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_vdReijden_nowOnGH.xlsx", sheet=1, startRow = 2)

identical(VdRSP,VdRGH)

identical(VdRGH$Pressure_variable, VdRSP$Pressure_variable)
identical(VdRGH$Response.variable_paper, VdRSP$Response.variable_paper)
identical(VdRGH$Response.variable_category, VdRSP$Response.variable_category)
identical(VdRGH$Magnitude.of.relationship, VdRSP$Magnitude.of.relationship)

PressVar <- data.frame(GH=VdRGH$Pressure_variable, SP=VdRSP$Pressure_variable)



CarSP <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_Carbonara_nowOnSP.xlsx", sheet=1, startRow = 2)
CarSP <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_Carbonara_nowOnSP2.xlsx", sheet=1, startRow = 2)
CarGH <- read.xlsx("C:/Users/estb/Desktop/DataExtractionForm_WP4_Carbonara_nowOnGH.xlsx", sheet=1, startRow = 2)

identical(CarSP,CarGH)

identical(CarGH$Pressure_variable, CarSP$Pressure_variable)
identical(CarGH$Response.variable_paper, CarSP$Response.variable_paper)
identical(CarGH$Response.variable_category, CarSP$Response.variable_category)
identical(CarGH$Magnitude.of.relationship, CarSP$Magnitude.of.relationship)

PressVar <- data.frame(GH=VdRGH$Pressure_variable, SP=VdRSP$Pressure_variable)
