#
##
### This script explores the raw scoping input from various stakeholders from the first round of SEAwise workshops
### Analyses by Elliot J. Brown 
##
#

#===
# Dependencies ----
#===

#====

#===
# Data ----
rd <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/coding_all_09052022.csv", header = T)
#====

#===
# Data Cleaning ----
#===

#====

#===
# Coding and matching categories ----
#===

#====

#===
# Temp figures for task 6.1 ----
#===
measures <- c("share of sea by different activities", "management", "governance", "spatial management", "tools", "spatially explicit", "delayed advice", "regulation", "Landing obligation", "regulation incoherence", "implementation", "collaboration", "quota exchange", "LTMS", "nature restoration", "control and enforcement", "management framework", "energy islands", "balanced harvesting", "market tools", "temporal closures ", "catch limits", "co-management", "displacement ", "NOT landing obligation", "selective gear", "spatial / temporal restrictions ", "coastal development", "management strategies", "fisher's autonomy", "fishing new stocks", "multispecies management", "short and long term temporal fisheries management", "spatio-temporal restrictions", "species catch options", "selfmanagment", "national quota allocation", "multispecies MEY", "compliance", "no-take zones", "ecosystem based management", "regulation measures", "Fmsy", "Multiannual Plans", "ecosystem based fisheries management", "management strategy", "adaptive management", "multispecies management ", "management objectoves", "spawning closures", "bag limit", "multi-stakeholder approach", "documentation of spawning closures", "culling", "effective control", "fleet overcapacity", "third country fleets", "temporal closures", "effort displacement", "protection measures", "closures", "fishing closure", "spill-over effects", "gear regulations", "other spatial and temporal measures", "management tools" , "decisive power back to fishers", "protection (recovery)", "spatial activity managing", "market price dynamics", "technical interactions", "alternative fishing grounds", "EBFM", "management measures", "landing obligation", "ecosystem approach for management ", "TAC", "NOT minimum mesh size", "labour standards", "access", "infrastructure", "OECM (other effective conservation measures)", "marine spatial planning")
measures <- measures[order(measures)]

#====