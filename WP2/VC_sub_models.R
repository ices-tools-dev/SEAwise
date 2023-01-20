wd<-"C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020\\_____________WP2\\TASK 2.2\\Socio-economic sub-models\\Seawise project_ITA"
setwd(wd)
econ_data=readxl::read_excel("AER_all_uploaded_v_fs.xlsx",sheet="all_uploaded_v_fs")
  #readxl::read_excel("Economic dataset.xlsx",sheet="Foglio1")
colnames(econ_data)

EFF=readxl::read_excel("Effort.xlsx",sheet="Foglio1")
colnames(EFF)
Country="ITA"
econ_data = econ_data[econ_data$country_code==Country,]
EFF=EFF[EFF$COUNTRY==Country,]

# Fleet segments definition
econ_data$FS=econ_data$fishing_tech
econ_data$FS=paste( econ_data$fishing_tech,econ_data$vessel_length,sep="_")

colnames(econ_data)
unique(econ_data$variable_name)

EFF$FS=paste(EFF$FISHING_TECH,EFF$VESSEL_LENGTH,sep="_")
EFF_temp=aggregate(EFF$TOTFISHDAYS,by=list(EFF$FS,EFF$YEAR),FUN="sum")
colnames(EFF_temp)=c("FS","YEAR","TOTFISHDAYS")


Merg=merge(econ_data,EFF_temp,by.y=c("FS","YEAR"),by.x=c("FS","year"))

# Energy costs
dir.create("FuelCosts")

Merg_temp=Merg[Merg$variable_name=="Energy costs" ,]

Merg_temp=Merg_temp[as.character(Merg_temp$value)!="NULL",]

FS=unique(Merg_temp$FS)

res=data.frame(coeff=matrix(ncol=1,nrow=length(FS)))
res$FS=""
res$MODEL=1
res$points=0
res$RMSE=NA

Merg_temp$EnCosMod1<-0


Merg_temp1=Merg_temp[1,]

for (i in 1:length(FS)){
  
 # Model 1 
  Merg_temp2=Merg_temp[Merg_temp$FS==FS[i],]  
  
  n=round(nrow(Merg_temp2)*2/3,0)
  hind_indices=sample(1:nrow(Merg_temp2),n)
  fore= which(!seq(1:nrow(Merg_temp2)) %in% hind_indices)
  
  res$coeff[i]=coefficients(lm(value~TOTFISHDAYS+0,data=Merg_temp2[hind_indices,]))
  res$points[i]=nrow(Merg_temp2)
  
  Merg_temp2$EnCosMod1=res$coeff[i]*Merg_temp2$TOTFISHDAYS
  
  
  
  res$RMSE[i]=sqrt(sum((as.numeric(Merg_temp2[fore,]$value)-Merg_temp2[fore,]$EnCosMod1)^2,na.rm=TRUE)/(length(Merg_temp2[fore,]$value)+1)) 
  
  Merg_temp1=rbind(Merg_temp1,Merg_temp2)
  
}


Merg_temp2=Merg_temp1[-1,]

write.table(res,"FuelCosts/EnCost_model1.csv",sep=";",row.names=F)

# Model 2

res=data.frame(coeff=matrix(ncol=1,nrow=length(FS)))
res$FS=""
res$MODEL=2
res$points=0
res$RMSE=NA

Merg_temp<-Merg_temp2
Merg_temp$EnCosMod2<-0


Merg_temp1=Merg_temp[1,]

for (i in 1:length(FS)){
  
  # Model 2 
  Merg_temp2=Merg_temp[Merg_temp$FS==FS[i],]  
  
  # estimation of fuel price
  Merg_temp3=Merg[Merg$FS==FS[i] & Merg$variable_name=="Energy consumption",]
  Merg_temp3=Merg_temp3[as.character(Merg_temp3$value)!="NULL",]
  
  FP=as.numeric(Merg_temp2[,15])/as.numeric(Merg_temp3[,15])
  
  Merg_temp3=Merg_temp2
  Merg_temp3$FP=FP
  Merg_temp3$FP_FD=Merg_temp3$FP*Merg_temp3$TOTFISHDAYS
  
  n=round(nrow(Merg_temp2)*2/3,0)
  hind_indices=sample(1:nrow(Merg_temp2),n)
  fore= which(!seq(1:nrow(Merg_temp2)) %in% hind_indices)
  
  res$coeff[i]=coefficients(lm(value~FP_FD+0,data=Merg_temp3[hind_indices,]))
  res$points[i]=nrow(Merg_temp2)
  
  Merg_temp2$EnCosMod2=res$coeff[i]*Merg_temp3$FP*Merg_temp3$TOTFISHDAYS
  
  res$RMSE[i]=sqrt(sum((as.numeric(Merg_temp2[fore,]$value)-Merg_temp2[fore,]$EnCosMod2)^2,na.rm=TRUE)/(length(Merg_temp2[fore,]$value)+1)) 
  
  Merg_temp1=rbind(Merg_temp1,Merg_temp2)
  
}


Merg_temp1=Merg_temp1[-1,]
write.table(res,"FuelCosts/EnCost_model2.csv",sep=";",row.names=F)


# Plots


for (r in 1:length(FS)){
  jpeg(paste("FuelCosts/graph",r,".jpg",sep=""),units="cm",width = 35,height=20,res=400)
  Merg_tempp1=Merg_temp1[Merg_temp1$FS==FS[i],]
  plot(Merg_tempp1$year,as.numeric(Merg_tempp1$value), ylab="Fuel costs (euro)",xlab="Year",main=FS[i],ylim=c(0,1.5*max(as.numeric(Merg_tempp1$value))),pch=19,cex=1.5,col="dark grey",cex.main=1.5,cex.axis=1.5,cex.lab=1.3)  
  lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod1),col="green",lwd=3)
  lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod2),col="blue",lwd=3)
  legend("topleft",c("Observed","Model1","Model2"),col=c("dark grey","green","blue"),pch=c(19,NA,NA),lty=c(NA,"solid","solid"),lwd=c(NA,4,4))
 dev.off() 
  }



# Other variable costs depending of effort

Merg_temp=Merg[Merg$variable_name=="Other variable costs" ,]

Merg_temp=Merg_temp[as.character(Merg_temp$value)!="NULL",]

FS=unique(Merg_temp$FS)

res=data.frame(coeff=matrix(ncol=1,nrow=length(FS)))
res$FS=""
res$MODEL=1
res$points=0
res$RMSE=NA


Merg_temp$EnCosMod1<-0

Merg_temp1=Merg_temp[1,]

for (i in 1:length(FS)){
  
  # Model 1 
  Merg_temp2=Merg_temp[Merg_temp$FS==FS[i],] 
  
  n=round(nrow(Merg_temp2)*2/3,0)
  hind_indices=sample(1:nrow(Merg_temp2),n)
  fore= which(!seq(1:nrow(Merg_temp2)) %in% hind_indices)
  
  res$coeff[i]=coefficients(lm(value~TOTFISHDAYS+0,data=Merg_temp2[hind_indices,]))
  res$points[i]=nrow(Merg_temp2)
  Merg_temp2$EnCosMod1=res$coeff[i]*Merg_temp2$TOTFISHDAYS
  
  res$RMSE[i]=sqrt(sum((as.numeric(Merg_temp2[fore,]$value)-Merg_temp2[fore,]$EnCosMod1)^2,na.rm=TRUE)/(length(Merg_temp2[fore,]$value)+1)) 
  
  Merg_temp1=rbind(Merg_temp1,Merg_temp2)
  
}


Merg_temp2=Merg_temp1[-1,]
dir.create("OtherVarCosts_Eff")
write.table(res,"OtherVarCosts_Eff/OtherVarCost_model1.csv",sep=";",row.names=F)


# Plots


for (r in 1:length(FS)){
  jpeg(paste("OtherVarCosts_Eff/graph",r,".jpg",sep=""),units="cm",width = 35,height=20,res=400)
  Merg_tempp1=Merg_temp1[Merg_temp1$FS==FS[i],]
  plot(Merg_tempp1$year,as.numeric(Merg_tempp1$value), ylab="Fuel costs (???)",xlab="Year",main=FS[i],ylim=c(0,1.2*max(as.numeric(Merg_tempp1$value))),pch=19,cex=1.5,col="dark grey",cex.main=1.5,cex.axis=1.5,cex.lab=1.3)  
  lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod1),col="green",lwd=3)
  #lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod2),col="blue",lwd=3)
  legend("topleft",c("Observed","Model1"),col=c("dark grey","green"),pch=c(19,NA),lty=c(NA,"solid"),lwd=c(NA,4))
  dev.off() 
}


land_data=readxl::read_excel("Landing.xlsx",sheet="Foglio1")
colnames(land_data)

#library(dplyr)

# Fleet segments definition

land_data$FS=""
land_data$FS=paste( land_data$FISHING_TECH,land_data$VESSEL_LENGTH,sep="_")


revenues=aggregate(land_data$Euro,by=list(land_data$FS,land_data$YEAR),FUN="sum")
colnames(revenues)=c("FS","YEAR","REVENUES")


# Other variable costs depending of revenues
dir.create("OtherVarCosts_Rev")

Merg_temp=Merg[Merg$variable_name=="Other variable costs" ,]

Merg_temp=Merg_temp[as.character(Merg_temp$value)!="NULL",]

Merg_temp=merge(Merg_temp,revenues,by.y=c("FS","YEAR"),by.x=c("FS","year"))


FS=unique(Merg_temp$FS)

res=data.frame(coeff=matrix(ncol=1,nrow=length(FS)))
res$FS=""
res$MODEL=1
res$points=0
res$RMSE=NA


Merg_temp$EnCosMod1<-0

Merg_temp1=Merg_temp[1,]

for (i in 1:length(FS)){
  
  # Model 1 
  Merg_temp2=Merg_temp[Merg_temp$FS==FS[i],]  
  
  n=round(nrow(Merg_temp2)*2/3,0)
  hind_indices=sample(1:nrow(Merg_temp2),n)
  fore= which(!seq(1:nrow(Merg_temp2)) %in% hind_indices)
  
  res$coeff[i]=coefficients(lm(value~REVENUES+0,data=Merg_temp2[hind_indices,]))
  res$points[i]=nrow(Merg_temp2)
  Merg_temp2$EnCosMod1=res$coeff[i]*Merg_temp2$REVENUES
  
  res$RMSE[i]=sqrt(sum((as.numeric(Merg_temp2[fore,]$value)-Merg_temp2[fore,]$EnCosMod1)^2,na.rm=TRUE)/(length(Merg_temp2[fore,]$value)+1)) 
  
  Merg_temp1=rbind(Merg_temp1,Merg_temp2)
  
}


Merg_temp2=Merg_temp1[-1,]

write.table(res,"OtherVarCosts_Rev/OtherVarCost_model1.csv",sep=";",row.names=F)



# Plots
dir.create("OtherVarCosts_Rev")

for (r in 1:length(FS)){
  jpeg(paste("OtherVarCosts_Rev/graph",r,".jpg",sep=""),units="cm",width = 35,height=20,res=400)
  Merg_tempp1=Merg_temp1[Merg_temp1$FS==FS[i],]
  plot(Merg_tempp1$year,as.numeric(Merg_tempp1$value), ylab="Revenues (???)",xlab="Year",main=FS[i],ylim=c(0,1.2*max(as.numeric(Merg_tempp1$value))),pch=19,cex=1.5,col="dark grey",cex.main=1.5,cex.axis=1.5,cex.lab=1.3)  
  lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod1),col="green",lwd=3)
  #lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod2),col="blue",lwd=3)
  legend("topleft",c("Observed","Model1"),col=c("dark grey","green"),pch=c(19,NA),lty=c(NA,"solid"),lwd=c(NA,4))
  dev.off() 
}


# Disaggregation of variable costs at metier level
# Fuel as an example

library(SECFISH)
dir.create("Disaggregation")

# In SECFISH the disaggregation is carried out in 2 steps:
# on individual data: the coefficients for disaggregation are derived.
# the coefficients are used to disaggregate fuel costs on the basis of effort data (by metier).
# Here an example of disaggregation is reported, using as coefficients the results on several Italian fleet segments as reported in Bitetto et al. 2022 (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0264334 ) 

key_table=read.csv("FS_MET_Italy_SECFISH.csv",sep=";",header=TRUE)

Merg_temp=Merg[Merg$variable_name=="Energy costs" ,]
Merg_temp=Merg_temp[as.character(Merg_temp$value)!="NULL",]

DF=data.frame(Fleet_segment=Merg_temp$FS,	fishing_tech=Merg_temp$fishing_tech,	vessel_length=Merg_temp$vessel_length,	year=Merg_temp$year,	variable_name="fuel_costs",	value=Merg_temp$value)

# includes the average number of hours in a fishing day
avg_Hs=18
DF2=data.frame(gear=EFF$GEAR_TYPE, vessel_length=EFF$VESSEL_LENGTH, year=EFF$YEAR, Fleet_segment= EFF$FS, Metier=EFF$METIER,  Effort=EFF$TOTFISHDAYS*avg_Hs, Lab_expl_var=0)


# selection of the FS present in the key_table
DF=DF[DF$Fleet_segment %in% unique(key_table$Fleet_segment),]
DF2=DF2[DF2$Fleet_segment %in% unique(key_table$Fleet_segment),]

source("Disaggregation_of_the_costs_SECFISH.r")

Disaggr(DF,key_table,DF2,path=paste(getwd(),"/Disaggregation",sep=""))

