#
##
### This script has "Helper" lines of code to calculate percnetages and counts of records to support the figures in the "ScopingExploratoryAnalyses.R" file.
###  Each chunk needs the corresponding chunk run in "ScopingExploratoryAnalyses.R" before being used here.
##
#

#===
# Percentages of records over time ----
#===
#### Numbers of socio-economic studies
# 2012
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])

# 2013
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])

# 2021
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])

#### Proportion of socio-economic studies rise before and after 2013
# 2012
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100

# 2013
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100

# 2021
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("Livelihood"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("ExternalHumanDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("Livelihood", "HumanWellBeing"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100


#### Numbers of Fishing vs ecosystem studies
# 2012
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])

# 2013
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])

# 2021
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])
sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])

#### Proportion of Fishing vs ecosystem studies rise before and after 2013
# 2012
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2012 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2012, "Occurrence"]))*100

# 2013
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2013 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2013, "Occurrence"]))*100

# 2021
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("RetainedSpecies"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("ExternalEcologicalDrivers"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100
(sum(tempdf_L[tempdf_L$Year == 2021 & tempdf_L$FAO_Group %in% c("RetainedSpecies", "GeneralEcosystem"), "Occurrence"])/
    sum(tempdf_L[tempdf_L$Year == 2021, "Occurrence"]))*100

#====

#===
# Records by region over time ----
#===
tempdf_L

#====