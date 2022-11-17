##-----------------------------------------------
## Author: Isabel Garc?a-Bar?n
## Modified by Amaia Astarloa for SEAWISE project
## Last update: 07/2022
##-----------------------------------------------

# workspace

#rm(list = ls ())

lapply(c("MuMIn","coda", "mgcv", "mvtnorm", "plyr", "dplyr", "reshape", "ggplot2","plotrix",
         "missMDA","raster","SDMTools","fields","maptools","maps","visreg","Hmisc","writexl",
         "viridis","dichromat","RColorBrewer", "ggsignif","grid", "gridExtra", "Rmisc"),
       library, character.only=TRUE)

map<- rgdal::readOGR(dsn="C:/Use/PREDOC/AZTI/SeaWise/Task4.2/Milestone 4.1/Plots/UIA_World_Countries_Boundaries-shp", layer="World_Countries__Generalized_")
plot(map)

## Set working directory

WorkDir <- "C:/Use/PREDOC/AZTI/SeaWise/task4.2/PSA/Input/"
OutDir <- "C:/Use/PREDOC/AZTI/SeaWise/task4.2/PSA/Output/"

setwd(WorkDir)

#You can also load the workspace
#load("Risk.assessment.trial.Rdata")


##------------------------------------
#         LOAD DATA              -----
##------------------------------------

#Here species data and effort have already same resolution and dimensions (this has been done in a previous step)

#The target species is common dolphin and the effort belong to pelagic trawlers (PTB,OTM) in fishing days
#For quarter 1 (winter) and 3 (summer). 

data<-readRDS("Deldel.trawlers.RDS") 
head(data)

par(mfrow=c(2,2))
plot(rasterFromXYZ(data[,c(1,2,3)]),main="C.dolphin dens Q1");plot(map,add=T,col="grey")
plot(rasterFromXYZ(data[,c(1,2,4)]),main="C.dolphin dens Q3");plot(map, add=T, col="grey")

plot(rasterFromXYZ(data[,c(1,2,5)]),main="Trawlers effort Q1", zlim=c(0,15));plot(map,add=T,col="grey")
plot(rasterFromXYZ(data[,c(1,2,6)]),main="Trawlers effort Q3", zlim=c(0,15));plot(map, add=T, col="grey")


##--------------------------------
#      Risk assessment      -----
##--------------------------------

#Risk is calculated as follows;  R = sqrt(P^2*S^2)

#For that we need P (productivity) and S (susceptibility)

# PRODUCTVITY (P): literature review



# SUSCEPTIBILITY (S)---> S=(a^2*e*ex^2*s^2*ple)^1/8


##--------------------------------
#        SUSCEPTABILITY           -----
##--------------------------------

#SUSCEPTIBILITY (S)---> S=(a^2*e*ex^2*s^2*ple)^1/8

#Here availability and exposure will be calculated
#Remaining values cab ne obtained from literature/expert knowledge

# 1. Availability (a)  --> [% overlap between fishing activity & species range.] --> TO BE CALCULATED

# 2. Encounterability (e) 

#    a) Overlap with fishery year round --> Score 3
#    b) Overlap with fishery beyond assessment period but not year round --> 2
#    c) Overlap limited to the assessment period --> score 1 

# 3. Selectivity (s)

#    a) High potential for capture --> Score 3
#    b) Moderate potential for capture--> 2
#    c) Low potential for capture--> score 1 

# 4. Potential for lethal encounter (ple)

#    a) Interaction with gear likely to result in death --> Score 3
#    b) Interaction with gear likely to result in injury  --> 2
#    c) Interaction with gear unlikely to result in injury or death  --> score 1 


# 5. Exposure (ex)  -->  TO BE CALCULATED (different by cell)


# 1. AVAILABILITY----

#Overlap within species range and fishing activity 

#High (Score 3): >30% overlap between fishing activity & species distribution 
#Medium (Score 2): 10-30% overlap between fishing activity & species distribution 
#Low (Score 3): <10% overlap between fishing activity & species distribution 

##We first delimit species range >> cells with abundance>0

only.pres.q1<-data[data$Dens.q1>0,]
only.pres.q3<-data[data$Dens.q3>0,]

image(rasterFromXYZ(only.pres.q1[,c(1,2,3)]));plot(map, add=T, col="grey")
image(rasterFromXYZ(only.pres.q1[,c(1,2,4)]));plot(map, add=T, col="grey")

interaction.percent.q1<-length(which(only.pres.q1$eff.q1!=0))/nrow(only.pres.q1)*100
interaction.percent.q3<-length(which(only.pres.q3$eff.q3!=0))/nrow(only.pres.q3)*100

#Plot to check
only.pres.q1$Densbi<-ifelse(only.pres.q1$Dens.q1>0,1,0)
only.pres.q1$Effbi<-ifelse(only.pres.q1$eff.q1>0,1,0)
only.pres.q1$sum<-rowSums(only.pres.q1[8:9])
table(only.pres.q1$sum)

plot(rasterFromXYZ(only.pres.q1[,c("x","y","sum")]), legend=F,col=viridis(3),main="Overlap q1");plot(map,add=T,col="grey"); box()
legend(grconvertX(7.5), grconvertY(50),c("1","2"), fill=viridis(2, option = "D"),xpd=TRUE, box.col="white")


#70% in both seasons >> Score 3
data$a.q1<-3
data$a.q3<-3


# 5. EXPOSURE----

exposure<-data[1:7]
head(exposure)

#Three equations

#Equation 1: ex.cell= [(ab cell x fishing activity cell)/abundance study area]  
exposure$ex.cell.q1<- (data$Dens.q1*data$eff.q1)/sum(data$Dens.q1, na.rm=T)
exposure$ex.cell.q3<- (data$Dens.q3*data$eff.q3)/sum(data$Dens.q3, na.rm=T)

table(is.na(exposure$ex.cell.q1)); table(is.na(exposure$ex.cell.q3))

#Equation 2: ex.mean=(sp.mean.cell*activity.mean.cell)/ab.total >> different by cell

# exposure$sp.mean.cell <- rowMeans(data[,grepl("Dens", names(data))], na.rm=T)
# exposure$activity.mean.cell<-rowMeans(data[,grepl("eff", names(data))], na.rm=T)
# 
# exposure$ex.mean.q1<-exposure$sp.mean.cell*exposure$activity.mean.cell/sum(data$Dens.q1, na.rm=T)
# exposure$ex.mean.q3<-exposure$sp.mean.cell*exposure$activity.mean.cell/sum(data$Dens.q3, na.rm=T)

#Equation 2: ex.mean=(sp.mean.cell*activity.mean.cell)/ab.total >> same for all cells (checked qith Patricia)

#Mean or meadian?
#First have a look:
summary(exposure$Dens.q1)
summary(exposure$Dens.q3)

summary(exposure$eff.q1)
summary(exposure$eff.q1)

#Not to get meadian values of 0, 0s will be replaced by NA
exposure$Dens.q1<-ifelse(exposure$Dens.q1==0,NA,exposure$Dens.q1)
exposure$Dens.q3<-ifelse(exposure$Dens.q3==0,NA,exposure$Dens.q3)

summary(exposure$Dens.q1)
summary(exposure$Dens.q3)

#Mean
exposure$sp.mean.cell.q1 <- mean(data$Dens.q1, na.rm=T)
exposure$activity.mean.cell.q1 <- mean(data$eff.q1, na.rm=T)
exposure$ex.mean.q1<-exposure$sp.mean.cell.q1*exposure$activity.mean.cell.q1/sum(data$Dens.q1, na.rm=T)

exposure$sp.mean.cell.q3 <- mean(data$Dens.q3, na.rm=T)
exposure$activity.mean.cell.q3 <- mean(data$eff.q3, na.rm=T)
exposure$ex.mean.q3<-exposure$sp.mean.cell.q3*exposure$activity.mean.cell.q3/sum(data$Dens.q3, na.rm=T)

#Median
exposure$sp.median.cell.q1 <- median(exposure$Dens.q1, na.rm=T)
exposure$activity.median.cell.q1 <- median(exposure$eff.q1, na.rm=T)
exposure$ex.median.q1<-exposure$sp.median.cell.q1*exposure$activity.median.cell.q1/sum(exposure$Dens.q1, na.rm=T)

exposure$sp.median.cell.q3 <- median(exposure$Dens.q3, na.rm=T)
exposure$activity.median.cell.q3 <- median(exposure$eff.q3, na.rm=T)
exposure$ex.median.q3<-exposure$sp.median.cell.q3*exposure$activity.median.cell.q3/sum(exposure$Dens.q3, na.rm=T)


#Equation 3:ex.score=log10(ex.cell/ex.mean)
#decide which one you want mean or median
exposure$ex.score.q1<-log10(exposure$ex.cell.q1/exposure$ex.mean.q1)
exposure$ex.score.q3<-log10(exposure$ex.cell.q3/exposure$ex.mean.q3)

exposure$ex.score.q1<-log10(exposure$ex.cell.q1/exposure$ex.median.q1)
exposure$ex.score.q3<-log10(exposure$ex.cell.q3/exposure$ex.median.q3)

summary(exposure$ex.score.q1)
summary(exposure$ex.score.q3)

#NaN, Inf and -Inf
exposure$ex.score.q1<-ifelse(is.nan(exposure$ex.score.q1), NA, exposure$ex.score.q1)
exposure$ex.score.q1<-ifelse(exposure$ex.score.q1=="-Inf", NA, exposure$ex.score.q1)

exposure$ex.score.q3<-ifelse(is.nan(exposure$ex.score.q3), NA, exposure$ex.score.q3)
exposure$ex.score.q3<-ifelse(exposure$ex.score.q3=="-Inf", NA, exposure$ex.score.q3)

summary(exposure$ex.score.q1)
summary(exposure$ex.score.q3)

#Rescale to 1-3 ---> #How ? From table 1 in brown et al 2015 (in the sharepoint)

#exposure equation:  log10(exposure.cell/mean exposure)
#exposure==0 >> exposure.cell=mean.exposure  >>moderate risk=2
#exposure >1 >> exposure.cell>mean.exposure  >>high risk=3
#exposure <-1 >> exposure.cell<mean.exposure  >>low risk=1


exposure$ex.score.q1.res <- cut(exposure$ex.score.q1,c(min(exposure$ex.score.q1, na.rm=T),-1,1,max(exposure$ex.score.q1, na.rm=T)))
levels(exposure$ex.score.q1.res) <- c(1,2,3)
summary(exposure$ex.score.q1.res)

exposure$ex.score.q3.res <- cut(exposure$ex.score.q3,c(min(exposure$ex.score.q3, na.rm=T),-1,1,max(exposure$ex.score.q3, na.rm=T)))
levels(exposure$ex.score.q3.res) <- c(1,2,3)
summary(exposure$ex.score.q3.res)

data$ex.q1<-exposure$ex.score.q1.res
data$ex.q3<-exposure$ex.score.q3.res

#Checking
par(mfrow=c(2,2))
plot(rasterFromXYZ(exposure[,c("x","y","ex.cell.q1")]));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.mean.q1")]));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q1")]), zlim=c(-2,2),col=viridis(5));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q1.res")]), col=viridis(3));plot(map,add=T,col="grey")

plot(rasterFromXYZ(exposure[,c("x","y","ex.cell.q3")]));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.mean.q3")]));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q3")]), zlim=c(-2,2),col=viridis(5));plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q3.res")]), col=viridis(3));plot(map,add=T,col="grey")

par(mfrow=c(2,2))
plot(rasterFromXYZ(data[,c(1,2,3)]),main="Dens.q1");plot(map,add=T,col="grey"); box()
plot(rasterFromXYZ(data[,c(1,2,5)]),main="Eff.q1");plot(map, add=T, col="grey"); box()
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q1")]), main="Exposure q1");plot(map, add=T, col="grey"); box()
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q1.res")]), legend=F, col=viridis(3),main="Exposure cat q1");plot(map,add=T,col="grey"); box()
legend(grconvertX(21), grconvertY(50),c("1","2","3"), fill=viridis(3, option = "D"),xpd=TRUE, box.col="white")

par(mfrow=c(2,2))
plot(rasterFromXYZ(data[,c(1,2,4)]),main="Dens.q3");plot(map,add=T,col="grey")
plot(rasterFromXYZ(data[,c(1,2,6)]),main="Eff.q3");plot(map, add=T, col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q3")]), col=viridis(5),main="Exposure q3");plot(map,add=T,col="grey")
plot(rasterFromXYZ(exposure[,c("x","y","ex.score.q3.res")]), legend=F,col=viridis(3),main="Exposure cat q3");plot(map,add=T,col="grey"); box()
legend(grconvertX(21), grconvertY(50),c("1","2","3"), fill=viridis(3, option = "D"),xpd=TRUE, box.col="white")


#REMAINING VALUES  -----

data$e <- 3      #encounterability                     >> expert knowledge/literature     
data$s <- 3     #selectivity                           >> expert knowledge/literature   
data$ple <- 3   #potential for lethal encounter:     >> expert knowledge/literature   


##FINAL EQUATION FOR SUSCEPTABILITY------

#S=(a^2*e*ex^2*s^2*ple)^1/8

names(data)
str(data)

#Quarter1
S1<-data[!is.na(data$ex.q1),]
S1$ex.q1<-as.numeric(S1$ex.q1)
str(S1)

S1$S<-S1$a.q1^2*S1$e*S1$ex.q1^2*S1$s^2*S1$ple
S1$S<-S1$S^0.125  #1/8=0.125
summary(S1$S)


#Quarter3
S3<-data[!is.na(data$ex.q3),]
S3$ex.q3<-as.numeric(S3$ex.q3)
str(S3)

S3$S<-(S3$a.q3^2*S3$e*S3$ex.q3^2*S3$s^2*S3$ple)
S3$S<-S3$S^0.125  #1/8=0.125
summary(S3$S)

#Minimum and maximum values that can be achieved in Susceptability. 
#Important to understand for further steps

min<-c(1^2*1*1^2*1^2*1)
min^0.125

max<-c(3^2*3*3^2*3^2*3)
max^0.125

##--------------------------------
#        PRODUCTIVITY        -----
##--------------------------------

#Attributes from Brown et al 2015 (available in the Sharepoint)

#average sexual maturity 8.66 years     >>> risk 2
#oldest reproductive female 9.5 years   >>> risk 1
#calf survival  (proportion) 0.798      >>> risk 2
#inter calving interval 3.05 years      >>> risk 2

x<-c(2,1,2,2)
P<-mean(x)

S1$P<-P
S3$P<-P

#Minimum value that can be achived here is also 1 and maximum value 3

##--------------------------------
#           RISK             -----
##--------------------------------

#R<-sqrt(P^2 + S^2)
#Minimum and maximum values that can be achieved here: 1-4.24

sqrt(1^2 + 1^2)
sqrt(3^2 + 3^2)

#So, according to the methodology (Brown, 2015):
#Low risk: values <2.64
#Medium risk: between 2.64 and 3.18
#High risk<: values >3.18  


# Calculate RISK  --> R = sqrt(P^2+S^2)  

#Quarter1
Q1<-S1
Q1$R <- sqrt(S1$P^2 + S1$S^2)
summary(Q1$R)

#Categorize
Q1$R1.score<-NA
Q1$R1.score<-ifelse(Q1$R>3.18, 3,Q1$R1.score)
Q1$R1.score<-ifelse(Q1$R<=3.18&  Q1$R>2.64, 2,Q1$R1.score)
Q1$R1.score<-ifelse(Q1$R<2.64,1,Q1$R1.score)
summary(Q1$R1.score)

Q1<-Q1[,c("x","y","R1.score")]
data<-merge(data,Q1, by=c("x","y"), all=T) #to have same dimensions

#Quarter3
Q3<-S3
Q3$R <- sqrt(S3$P^2 + S3$S^2)
summary(Q3$R)

#Categorize
Q3$R3.score<-NA
Q3$R3.score<-ifelse(Q3$R>3.18, 3,Q3$R3.score)
Q3$R3.score<-ifelse(Q3$R<=3.18&Q3$R>2.64, 2,Q3$R3.score)
Q3$R3.score<-ifelse(Q3$R<2.64,1,Q3$R3.score)
summary(Q3$R3.score)

Q3<-Q3[,c("x","y","R3.score")]
data<-merge(data,Q3, by=c("x","y"),all=T)#to have same dimensions


#Plots
par(mfrow=c(1,2))
colors=c("green","yellow","red")
plot(rasterFromXYZ(data[,c("x","y","R1.score")]),main=c("Risk quarter 1"),col = colors[2:3], legend=FALSE)
legend(grconvertX(-22), grconvertY(46),c("Low","Medium","High"), fill=colors,xpd=TRUE, box.col="white")
plot(map,add=T,col="grey")      
box()

plot(rasterFromXYZ(data[,c("x","y","R3.score")]),main=c("Risk quarter 3"),col = colors[2:3], legend=FALSE)
legend(grconvertX(-22), grconvertY(46),c("Low","Medium","High"), fill=colors,xpd=TRUE, box.col="white")
plot(map,add=T,col="grey")      
box()

save.image(paste0(OutDir,"Risk.assessment.trial.Rdata"))
