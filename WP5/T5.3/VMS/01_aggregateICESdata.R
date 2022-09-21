#- SEAwise script task 5.3 to read in ICES 2021/2022 datacall files and convert to aggregated products
#  This script reads in data as submitted under the ICES datacall https://ices-library.figshare.com/articles/report/ICES_Data_call_2022_-_Data_submission_of_VMS_Log_book_data/19249019

#library(devtools)
#install_github("nielshintzen/vmstools/vmstools/")
#install.packages(c("sfdSAR", "icesVMS"), repos = 'https://ices-tools-prod.r-universe.dev')

library(vmstools) #required for C-square to ICESrectangle conversion
library(icesVMS)
library(sfdSAR)
library(dplyr)
library(icesVocab)
library(doBy)
library(plyr)

#- Preparations to get gear width information for countries submitting 2009-2020 data or for countries that do not hold gear width information
# metier level 6 -> benthis metiers (updated 2022)
metier_lookup <-
  get_metier_lookup() %>%
  tibble() %>%
  select(leMetLevel6, benthisMetiers)

# benthis metiers -> gear width model and coefficients
gear_widths_models <-
  get_benthis_parameters() %>%
  tibble() %>%
  select(-id)

# metier level 6 -> gear width model and coefficients
metier6_gear_model_lookup <-
  metier_lookup %>%
  filter(!is.na(benthisMetiers)) %>%
  left_join(
    gear_widths_models, by = c(benthisMetiers = "benthisMet")
  ) %>%
  rename(
    a = "firstFactor", b = "secondFactor"
  )

# Settings to convert vessel length into category data
vlen_ices 			<- getCodeList("VesselLengthClass") ### Get DATSU Vocabulary list for selected dataset
# Filter values that aren't deprecated, overlapped  or not accepted by data call requirements
vlen_icesc 			<-  vlen_ices%>%slice(2, 4, 6, 7, 8, 10, 11, 12, 13 )%>%select(Key)


#- Example to get gear widths from test dataset
#data(test_vms)
#test_vms <- merge(test_vms,metier6_gear_model_lookup,by.x="LE_MET_level6",by.y="leMetLevel6",all.x=T) 
#test_vms$AverageGearWidth <-
#  predict_gear_width(test_vms$gearModel, test_vms$gearCoefficient, test_vms)

#- Running script for each of the countries that submitted raw data
readSettings 		<- data.frame(country=c("BEL","DNK","EST","IRL","NLD"),
								  yearRange=c("0920","0920","0921","0921","0921"))

#- Set path to table 1 data as submitted to ICES
dataPath 			<- "W:/IMARES/Data/VMS_/BASEFILES/restricted/SEAwise/Data"
outPath 			<- "W:/IMARES/Data/VMS_/BASEFILES/restricted/SEAwise/Data"

#- Read in the VMS table (table 1). Set column names according to 2021 and 2022 calls

for(i in 1:nrow(readSettings)){
	VMS <- read.csv(file.path(dataPath,paste0(readSettings$country[i],"_table1_2009-2021.csv")),header=F,stringsAsFactors=F)
	if(VMS[1,1]=="RecordType") VMS <- VMS[-1,]
	if(readSettings$yearRange[i]=="0921"){
		colnames(VMS) 	<- c("RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
						      "C-square","MetierL4", "MetierL5",  "MetierL6",  "VesselLengthRange",
	    				   	  "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
	    				      "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth")
		VMS$contactModel 			<- merge(VMS,metier6_gear_model_lookup,by.x="MetierL6",by.y="leMetLevel6",all.x=T)$contactModel
		idx 		      			<- which(is.na(VMS$AverageGearWidth))
		if(length(idx)>0)
			VMS$AverageGearWidth[idx] 	<- merge(VMS[idx,],metier6_gear_model_lookup,by.x="MetierL6",by.y="leMetLevel6",all.x=T)$gearWidth*1000
	}
	if(readSettings$yearRange[i]=="0920"){
		colnames(VMS) 	<- c("RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
	    				     "C-square","MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6","VMSenabled",
	    				     "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
	    				     "kWFishingHour", "TotWeight", "TotValue")
		VMS$AverageGearWidth  <- merge(VMS,metier6_gear_model_lookup,by.x="MetierL6",by.y="leMetLevel6",all.x=T)$gearWidth*1000
		VMS$VesselLengthRange <- VMS$AverageVesselLength%>%cut(    breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
	                                             right = FALSE    ,include.lowest = TRUE,
	                                             labels =  vlen_icesc$Key)
		VMS$contactModel <- merge(VMS,metier6_gear_model_lookup,by.x="MetierL6",by.y="leMetLevel6",all.x=T)$contactModel

	}

	Country				<- VMS$CountryCode[1]  

	#- Force columns to be numeric
	unchar		<-c("Year","Month","NoDistinctVessels","AverageFishingSpeed","AverageVesselLength","AveragekW","kWFishingHour","FishingHour","TotWeight","TotValue","AverageGearWidth")
	VMS[,unchar]<-lapply(unchar, function(x) as.numeric(as.character(VMS[,x])))

	#- Checks
	if(range(VMS$Year)[1] >2009 | range(VMS$Year)[2] < 2022)
		warning(paste0("Submitting a time-period (",range(VMS$Year)[1],"-",range(VMS$Year)[2],") shorter than requested (2009-2022)"))

	missingValsBy 		<- apply(VMS[,c("Year","Month","C-square","MetierL4","MetierL6","VesselLengthRange")],2,function(x){length(which(is.na(x) | is.infinite(x)))})
	missingValsVar 		<- apply(VMS[,c("FishingHour","kWFishingHour","AverageFishingSpeed","AverageGearWidth")],2,function(x){length(which(is.na(x) | is.infinite(x)))})
	if(any(missingValsBy>0)){
		print(missingValsBy)
		warning("Missing records for variable to aggregate by:")
	}
	if(any(missingValsVar>0)){
		print(missingValsVar)
		warning(paste0("Missing records for variable to aggregate by: ", names(which(missingValsVar>0))))
	}

	#---------------------------------------
	#- Aggregate for different data products
	#---------------------------------------

	#- Task 4.2 (Evaluate the effects of fishing on bycatch of potentially endangered and threatened species)
	#  Aggregation by: year, month, c-square level (0.05), gear and vessel length category 
	#  Variable to aggregate: Fishing hour, kWFishingHour

	aggBy 	<- c("Year","Month","C-square","MetierL4","VesselLengthRange")
	aggVar 	<- c("FishingHour","kWFishingHour")

	prodT42 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 

	#- Task 4.3 (Evaluate the effects of fishing on benthic habitats)
	#  Aggregation by: year, month, c-square level (0.05), gear * target species composition (metier level 6)
	#  Variable to aggregate: Swept Area 

	VMS$SweptArea <- predict_surface_contact(VMS$contactModel,VMS$FishingHour,VMS$AverageGearWidth/1000,VMS$AverageFishingSpeed)
	aggBy 	<- c("Year","Month","C-square","MetierL6")
	aggVar 	<- c("SweptArea")

	prodT43 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 

	#- Task 4.5 (Effects of fisheries related litter and ghost nets)
	#  Aggregation by: year, month, ICES rectangle, gear and vessel length category
	#  Variable to aggregate: Fishing hour 

	VMS$Rect<- ICESrectangle(CSquare2LonLat(VMS[,"C-square"],degrees=0.05))
	aggBy 	<- c("Year","Month","Rect","MetierL4","VesselLengthRange")
	aggVar 	<- c("FishingHour")

	prodT45 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 

	#- Task 5.3 (Existing and novel fleet models will be used to link species distribution with fleet behaviour)
	#  Aggregation by: year, month, C-square,Vessel length category, gear * target species composition (metier level 6)
	#  Variable to aggregate: Fishing hour, kWFishingHour 

	aggBy 	<- c("Year","Month","C-square","MetierL6","VesselLengthRange")
	aggVar 	<- c("FishingHour","kWFishingHour")

	prodT53 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 

	#- Task 5.5 (Νo-Τake Zones or other types of areas restricted to certain fishing)
	#  Aggregation by: year, month, C-square,Vessel length category, gear * target species composition (metier level 6)
	#  Variable to aggregate: Fishing hour, TotWeight, TotValue

	aggBy 	<- c("Year","Month","C-square","MetierL6","VesselLengthRange")
	aggVar 	<- c("FishingHour","kWFishingHour","TotWeight","TotValue")

	prodT55 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 


	#- Save outputs to file, send RData file to seawise@wur.nl
	save(prodT42,prodT43,prodT45,prodT53,prodT55,file=file.path(outPath,paste0("aggregatedProducts",Country,".RData")))
}


#- Aggregate data over all countries, including those that submitted aggregated data
prodT42s <- prodT43s <- prodT45s <- prodT53s <- prodT55s <- list()
for(iCtr in c("BE","DE","DK","GB","IE","NL","EE")){
	print(iCtr)
	load(file=file.path(outPath,paste0("aggregatedProducts",iCtr,".RData")))
	prodT42s[[iCtr]] <- prodT42
	prodT43s[[iCtr]] <- prodT43
	prodT45s[[iCtr]] <- prodT45
	prodT53s[[iCtr]] <- prodT53
	prodT55s[[iCtr]] <- prodT55
	rm(prodT42,prodT43,prodT45,prodT53,prodT55)
}
load(file.path(outPath,"aggregatedProducts43DE.RData")
prodT43s[["DE"]] <- prodT43
	
prodT42c 	<- do.call(rbind,prodT42s)
aggBy 		<- c("Year","Month","C-square","MetierL4","VesselLengthRange")
aggVar 		<- c("FishingHour","kWFishingHour")
orderBy(~country,data=cbind(country=c("BE","DE","DK","GB","IE","NL","EE"),do.call(rbind,lapply(prodT42s,function(x){apply(x[,aggVar],2,range)}))))
prodT42all 	<- aggregate(prodT42c[,aggVar],by=as.list(prodT42c[,aggBy]),FUN=sum,na.rm=T) 

prodT43c 	<- do.call(rbind,prodT43s)
colnames(prodT43c)[ncol(prodT43c)] <- "SweptArea"
aggBy 		<- c("Year","Month","C-square","MetierL6")
aggVar 		<- c("SweptArea")
orderBy(~country,data=cbind(country=c("BE","DE","DK","GB","IE","NL","EE"),do.call(rbind,lapply(prodT43s,function(x){range(x$x)}))))
prodT43all 	<- aggregate(prodT43c[,aggVar],by=as.list(prodT43c[,aggBy]),FUN=sum,na.rm=T) 
colnames(prodT43all)[ncol(prodT43all)] <- aggVar

prodT45c 	<- do.call(rbind,prodT45s)
colnames(prodT45c)[ncol(prodT45c)] <- "FishingHour"
aggBy 		<- c("Year","Month","Rect","MetierL4","VesselLengthRange")
aggVar 		<- c("FishingHour")
orderBy(~country,data=cbind(country=c("BE","DE","DK","GB","IE","NL","EE"),do.call(rbind,lapply(prodT45s,function(x){range(x$x)}))))
prodT45all 	<- aggregate(prodT45c[,aggVar],by=as.list(prodT45c[,aggBy]),FUN=sum,na.rm=T) 
colnames(prodT45all)[ncol(prodT45all)] <- aggVar

prodT53c 	<- do.call(rbind,prodT53s)
aggBy 		<- c("Year","Month","C-square","MetierL6","VesselLengthRange")
aggVar 		<- c("FishingHour","kWFishingHour")
orderBy(~country,data=cbind(country=c("BE","DE","DK","GB","IE","NL","EE"),do.call(rbind,lapply(prodT53s,function(x){apply(x[,aggVar],2,range)}))))
prodT53all 	<- aggregate(prodT53c[,aggVar],by=as.list(prodT53c[,aggBy]),FUN=sum,na.rm=T) 

prodT55c 	<- do.call(rbind.fill,prodT55s)
aggBy 		<- c("Year","Month","C-square","MetierL6","VesselLengthRange")
aggVar 		<- c("FishingHour","kWFishingHour","TotWeight","TotValue")
orderBy(~country,data=cbind(country=c("BE","DE","DK","GB","IE","NL","EE"),do.call(rbind,lapply(prodT55s,function(x){apply(x[,aggVar[1:3]],2,range)}))))
prodT55all 	<- aggregate(prodT55c[,aggVar],by=as.list(prodT55c[,aggBy]),FUN=sum,na.rm=T) 
prodT55all  <- prodT55all[,c(aggBy,aggVar[1:3])]

save(prodT42all,file=file.path(outPath,"prodT42all.RData"))
save(prodT43all,file=file.path(outPath,"prodT43all.RData"))
save(prodT45all,file=file.path(outPath,"prodT45all.RData"))
save(prodT53all,file=file.path(outPath,"prodT53all.RData"))
save(prodT55all,file=file.path(outPath,"prodT55all.RData"))



