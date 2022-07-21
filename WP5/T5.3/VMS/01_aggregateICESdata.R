#- SEAwise script task 5.3 to read in ICES 2021/2022 datacall files and convert to aggregated products
#  This script reads in data as submitted under the ICES datacall https://ices-library.figshare.com/articles/report/ICES_Data_call_2022_-_Data_submission_of_VMS_Log_book_data/19249019

#library(devtools)
#install_github("nielshintzen/vmstools/vmstools/")

library(vmstools) #required for C-square to ICESrectangle conversion

#- Set path to table 1 data as submitted to ICES
dataPath 			<- "W:/IMARES/Data/VMS_/BASEFILES/restricted/SEAwise/Data"

#- Read in the VMS table (table 1). Set column names according to 2021 and 2022 calls
VMS <- read.csv(file.path(dataPath,"NLD_table1_2009-2021.csv"),header=F,stringsAsFactors=F)
if(any(VMS[,3]==2021))
	colnames(VMS) 	<- c("RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
					      "C-square","MetierL4", "MetierL5",  "MetierL6",  "VesselLengthRange",
    				   	  "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
    				      "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth")
if(!any(VMS[,3]==2021))
	colnames(VMS) 	<- c("RecordType", "CountryCode", "Year", "Month", "NoDistinctVessels", "AnonymizedVesselID",
    				     "C-square","MetierL4", "MetierL5", "LowerMeshSize", "UpperMeshSize", "MetierL6",  "VesselLengthRange",
    				     "AverageFishingSpeed", "FishingHour", "AverageVesselLength", "AveragekW",
    				     "kWFishingHour", "TotWeight", "TotValue" , "AverageGearWidth")

Country				<- VMS$CountryCode[1]    				     

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

VMS$SweptArea <- VMS$AverageFishingSpeed * 1.852 * VMS$AverageGearWidth/1000 * VMS$FishingHour
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

prodT53 <- aggregate(VMS[,aggVar],by=as.list(VMS[,aggBy]),FUN=sum,na.rm=T) 


#- Save outputs to file, send RData file to seawise@wur.nl
save(prodT42,prodT43,prodT45,prodT53,prodT55,file=file.path(outPath,paste0("aggregatedProducts",Country,".RData")))