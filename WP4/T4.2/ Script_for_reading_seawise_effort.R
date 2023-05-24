##################################################################
##Script to read FDI data and create maps per gear and quarter
#Author: Amaia Astarloa
#Date:18/02/2022
# Jo Bluemel updated to create maps per gear and quarter for the seawise effort data
# Date -2/12/2022
###################################################################


#Load libraries
library(rgdal)
library(sf)
library(raster)
library(SDMTools)
library(fields)
library(maptools)
library(tidyverse)

map<- rgdal::readOGR(dsn="C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/Amaia_Scripts/New script/UIA_World_Countries_Boundaries-shp", layer="World_Countries__Generalized_")
plot(map)


#1.Set directory-------
setwd("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data")
load("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data/prodT42all.Rdata")


#2. Read data------

summary(prodT42all)

df_all <- prodT42all
str(df_all)

#'data.frame':	10116518 obs. of  9 variables:
#  $ Year             : num  2016 2017 2017 2016 2016 ...
#$ Month            : num  2 10 1 2 3 3 4 4 5 5 ...
#$ Csquare          : chr  "1400:390:140:3" "1400:390:140:4" "1400:390:350:1" "1400:390:350:1" ...
#$ MetierL4         : chr  "MIS" "MIS" "MIS" "MIS" ...
#$ VesselLengthRange: chr  "MIS" "MIS" "MIS" "MIS" ...
#$ FishingHour      : num  1.999 0.104 0.59 0.25 1.45 ...
#$ kWFishingHour    : num  263.9 13.8 77.8 33 191.5 ...
#$ SI_LATI          : num  49.5 49.5 49.5 49.5 49.5 ...
#$ SI_LONG          : num  0.025 0.075 0.025 0.025 0.025 ...

df_all$X <- df_all$SI_LONG
df_all$Y <- df_all$SI_LATI


##4. Check spatial scale
min(df_all$X); max(df_all$X)
min(df_all$Y); max(df_all$Y)

quilt.plot(df_all$X,df_all$Y,df_all$kWFishingHour)
plot(map,add=T,col="grey")



#5. Check temporal scale---
unique(sort(df_all$Year))
#[1] 2016 2017 2012 2020 2018 2021 2015 2019 2009 2010 2014 2011 2013
unique(sort(df_all$Month))
#[1]  2 10  1  3  4  5  6  7  8  9 11 12

# convert month to quarter
df_all$quarter <- ifelse(df_all$Month <=3, "1", 
                       ifelse((df_all$Month>3 ) & (df_all$Month<=6), "2", 
                              ifelse((df_all$Month>6 ) & (df_all$Month<=9), "3", 
                                     ifelse(df_all$Month >9, "4", ""))))

unique(sort(df_all$quarter))
#[1] "1" "4" "2" "3"

#6. Check gear types---
unique(df_all$MetierL4)

#[1] "MIS" #"DRB" #"FPO" #"GNS" #"GTR" #"LHP" #"LLS" #"OTB" #"OTM" #"OTT" #"SSC" #"TBB" "GN"  #"HMD" #"LLD" #"PS" 
#[17] #"PTB" #"PTM" #"SDN" #"LTL"



#abbreviations of the métiers
#https://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier
#https://fish-commercial-names.ec.europa.eu/fish-names/fishing-gears_en


#Gillnets and similar nets
## GNS: set gillnet
## GTR: trammel net
## GN: undefined gillnet, probably GNS

# NO GTN: combined trammel and gillnets
# NO GNC: encircling gillnets
# NO GND: drift nets


#Seines
## SDN: Danish seines
## SSC: Scottish seines

# NO SB: beach seines
# NO SV: boat seine
# NO SPR : pair seines


#Trawls 
## OTB: bottom otter trawls
## PTB: bottom pair trawls
## OTT: otter twin trawls
## TBB: beam trawls

# pelagic trawls
## PTM: pelagic pair trawls - pelagic
## OTM: midwater otter trawls - pelagic


#Hooks and lines
## LLS: set longline
## LLD: longlines
## LTL: troll lines
## LHP: hand lines and pole lines (hand operated)

# NO LHM: hand lines and pole lines (mechanised)


#Dredges
## DRB: boat dredges
## HMD: mechanized dredge

# NO DRH: hand dredges used on board a vesell

#Pots and traps
## FPO: pots

# NO FYK: Fyke nets
# NO FPN: Stationary uncovered pound nets

#Surrouding nets and lift nets
## PS: Purse seines

# NO LNS: Shore operated stationary lift nets
# NO LNB: Boat operated lift nets
# NO LA: Lampara nets


#7. Save the object as an intermediate step---
saveRDS(df_all, "seawise_effort.RDS")


#8. Obtain annual raster------

#Read the object if needed
df_all<-readRDS( "seawise_effort.RDS")
min(df_all$X); max(df_all$X)
min(df_all$Y); max(df_all$Y)


#For plotting
library(fields)
library(SDMTools)
library(maptools)
library(marmap)
library(raster)
library(sp)


#Create a reference grid 
xmin<--14.97501; xmax<-24.97501 
ymin<-43.02501; ymax<-61.97501;
cellsize<-0.05

LowerLeftCornerLon<- -14.97501
LowerLeftCornerLat<-43.02501

DimLon<-ceiling(abs(xmin -xmax)/cellsize)
DimLat<-ceiling(abs(ymax - ymin)/cellsize)

gt <- GridTopology(cellcentre.offset=c(LowerLeftCornerLon,LowerLeftCornerLat), 
                   cellsize=c(cellsize,cellsize), cells.dim=c(DimLon,DimLat))
ref<-SpatialGrid(gt)
ref$id<-1:(ref@grid@cells.dim[1]*ref@grid@cells.dim[2])
class(ref)

grid<-raster.from.asc(asc.from.sp(ref))
plot(grid)

crs(grid) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 

#Bathymetry
tt<-raster("C:/use/PREDOC/Scripts/bat_med/GEBCO/gebco.asc")
bat.res<-resample(tt, grid) #takes time
bat.res
bat<-asc.from.raster(bat.res)
image.plot(bat)

xbat<-seq(attr(bat,"xll"),
          (attr(bat,"xll")+(nrow(bat)*attr(bat,"cellsize"))-attr(bat,"cellsize")),
          attr(bat,"cellsize"))
ybat<-seq(attr(bat,"yll"),
          (attr(bat,"yll")+(ncol(bat)*attr(bat,"cellsize"))-attr(bat,"cellsize")),
          attr(bat,"cellsize"))



#Example for gillnets------


#Gillnets and similar nets

gillnet<-df_all[df_all$MetierL4=="GNS"|df_all$MetierL4=="GTR"|df_all$MetierL4=="GN",]
colnames(gillnet)

q<-sort(unique(gillnet$quarter))
yy<-sort(unique(gillnet$Year))


for(i in 1:length(q)){
  #i=1
  gillq<-gillnet[gillnet$quarter==i,]
  for (j in 1:length(yy)){
  #j=1
  sub<-gillq[gillq$Year==yy[j],]
  agg <- aggregate(kWFishingHour ~ X + Y + Csquare, data = sub, mean)
  xy <- agg[,c(1,2)]
  xyz <- agg[c("X","Y","kWFishingHour")]
  spdf <- SpatialPointsDataFrame(coords = xy, data = xyz, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #spplot(spdf, "kWFishingHour")
  
  ras <- rasterize(spdf, grid, spdf$kWFishingHour, fun='last')
  plot(ras)
  print(dim(ras)) #check dimension
  print(res(ras)==res(grid)) #to check that resolution is the same
  name<-paste0("r",j)
  assign(name,ras)
  }
  rs <- stack(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13) #put together all the rasters
  zz<-calc(rs, fun=mean, na.rm=T)  #median?
  name<-paste0("mean.gillnets.q",i) #annual mean per quarter
  assign(name,zz)
  
}

#Checking
par(mfrow=c(2,2))
plot(log(mean.gillnets.q1))
plot(log(mean.gillnets.q2))
plot(log(mean.gillnets.q3))
plot(log(mean.gillnets.q4))


#Customize maps

q<-unique(gillnet$quarter)

for (i in 1:length(q))   { 
  
  kk<-get(paste0("mean.gillnets.q",i))
  png(paste0("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data/Gillnets_q",i,".png"),units="mm", width=180,height=150, res=300)
  plot(log(kk+1), xlim=c(-20,30), ylim=c(30,70),
       main=paste("Gillnets quarter",i),
     xlab="Longitude", ylab="Latitude",cex=0.9,legend.args = list(text = 'Kw \nFishing \nHour', side=3, font=2, cex=0.8, line=1))
  plot(map,add=T,col="grey")
  #contour(xbat,ybat,bat,levels=c(-200,-1000,-2000),add=T,col="darkgrey")
  box()
  dev.off()
}


###Example for pelagic trawlers---------

trawlers<-df_all[df_all$MetierL4=="PTM"|df_all$MetierL4=="OTM",]

colnames(trawlers)
q<-unique(trawlers$quarter)
yy<-unique(trawlers$Year)


for(i in 1:length(q)){
  #i=1
  trawlq<-trawlers[trawlers$quarter==i,]
  
  for (j in 1:length(yy)){
    #j=1
    sub<-trawlq[trawlq$Year==yy[j],]
    agg <- aggregate(kWFishingHour ~ X + Y + Csquare, data = sub, mean)
    xy <- agg[,c(1,2)]
    xyz <- agg[c("X","Y","kWFishingHour")]
    spdf <- SpatialPointsDataFrame(coords = xy, data = xyz, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #spplot(spdf, "kWFishingHour")
    ras <- rasterize(spdf, grid, spdf$kWFishingHour, fun='last')
    #plot(ras)
    print(dim(ras)) #echeck dimension
    print(res(ras)==res(grid)) #to check that resolution is teh same
    name<-paste0("r",j)
    assign(name,ras)
  }
  rs <- stack(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13) #put together all the rasters
  zz<-calc(rs, fun=mean, na.rm=T)  #median?
  name<-paste0("mean.trawlers.q",i) #annual mean per quarter
  assign(name,zz)
  
}

#Checking
par(mfrow=c(2,2))
plot(log(mean.trawlers.q1))
plot(log(mean.trawlers.q2))
plot(log(mean.trawlers.q3))
plot(log(mean.trawlers.q4))


#Customize maps
q<-unique(trawlers$quarter)

for (i in 1:length(q))   { 
  
  kk<-get(paste0("mean.trawlers.q",i))
  png(paste0("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data/trawlers_q",i,".png"),units="mm", width=180,height=150, res=300)
  plot(log(kk+1), xlim=c(-20,30), ylim=c(30,70),
       main=paste("Pelagic trawlers quarter",i),
       xlab="Longitude", ylab="Latitude",cex=0.9,legend.args = list(text = 'Kw \nFishing \nHour', side=3, font=2, cex=0.8, line=1))
  plot(map,add=T,col="grey")
  box()
  dev.off()
}


#Example for otter trawls------

otter<-df_all[df_all$MetierL4=="OTB"|df_all$MetierL4=="OTT"|df_all$MetierL4=="PTB",]

colnames(otter)
q<-unique(otter$quarter)
yy<-unique(otter$Year)


for(i in 1:length(q)){
  #i=1
  ottq<-otter[otter$quarter==i,]
  
  for (j in 1:length(yy)){
    #j=1
    sub<-ottq[ottq$Year==yy[j],]
    agg <- aggregate(kWFishingHour ~ X + Y + Csquare, data = sub, mean)
    xy <- agg[,c(1,2)]
    xyz <- agg[c("X","Y","kWFishingHour")]
    spdf <- SpatialPointsDataFrame(coords = xy, data = xyz, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #spplot(spdf, "kWFishingHour")
    ras <- rasterize(spdf, grid, spdf$kWFishingHour, fun='last')
    #plot(ras)
    print(dim(ras)) #check dimension
    print(res(ras)==res(grid)) #to check resolution 
    name<-paste0("r",j)
    assign(name,ras)
  }
  rs <- stack(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13) #put together all the rasters
  zz<-calc(rs, fun=mean, na.rm=T)  #median?
  name<-paste0("mean.otters.q",i) #annual mean per quarter
  assign(name,zz)
  
}

#Checking
par(mfrow=c(2,2))
plot(log(mean.otters.q1))
plot(log(mean.otters.q2))
plot(log(mean.otters.q3))
plot(log(mean.otters.q4))


#Customize maps

q<-unique(otter$quarter)

for (i in 1:length(q))   { 
  
  kk<-get(paste0("mean.otters.q",i))
  png(paste0("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data/otters_q",i,".png"),units="mm", width=180,height=150, res=300)
  plot(log(kk+1), xlim=c(-20,30), ylim=c(30,70),
       main=paste("otters quarter",i),
       xlab="Longitude", ylab="Latitude",cex=0.9,legend.args = list(text = 'Kw \nFishing \nHour', side=3, font=2, cex=0.8, line=1))
  plot(map,add=T,col="grey")
  box()
  dev.off()
}


#Example for beam trawls------

beam<-df_all[df_all$MetierL4=="TBB",]

colnames(beam)
q<-unique(beam$quarter)
yy<-unique(beam$Year)


for(i in 1:length(q)){
  #i=1
  bttq<-beam[beam$quarter==i,]
  
  for (j in 1:length(yy)){
    #j=1
    sub<-bttq[bttq$Year==yy[j],]
    agg <- aggregate(kWFishingHour ~ X + Y + Csquare, data = sub, mean)
    xy <- agg[,c(1,2)]
    xyz <- agg[c("X","Y","kWFishingHour")]
    spdf <- SpatialPointsDataFrame(coords = xy, data = xyz, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #spplot(spdf, "kWFishingHour")
    ras <- rasterize(spdf, grid, spdf$kWFishingHour, fun='last')
    #plot(ras)
    print(dim(ras)) # dimension
    print(res(ras)==res(grid)) #to check resolution is the same
    name<-paste0("r",j)
    assign(name,ras)
  }
  rs <- stack(r1,r2,r3,r4,r5,r6,r7) #put together all the rasters
  zz<-calc(rs, fun=mean, na.rm=T)  #median?
  name<-paste0("mean.beam.q",i) #mean per quarter across all years
  assign(name,zz)
  
}

#Checking
par(mfrow=c(2,2))
plot(log(mean.beam.q1))
plot(log(mean.beam.q2))
plot(log(mean.beam.q3))
plot(log(mean.beam.q4))


#Customize maps

q<-unique(beam$quarter)

for (i in 1:length(q))   { 
  
  kk<-get(paste0("mean.beam.q",i))
  png(paste0("C:/Users/JB27/OneDrive - CEFAS/DOCUMENTS/SEAWISE/4.2/R/seawise_effort data/beam_q",i,".png"),units="mm", width=180,height=150, res=300)
  plot(log(kk+1), xlim=c(-20,30), ylim=c(30,70),
       main=paste("beam quarter",i),
       xlab="Longitude", ylab="Latitude",cex=0.9,legend.args = list(text = 'Kw \nFishing \nHour', side=3, font=2, cex=0.8, line=1))
  plot(map,add=T,col="grey")
  box()
  dev.off()
}
