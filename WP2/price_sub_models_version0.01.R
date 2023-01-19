wd<-"C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020\\_____________WP2\\TASK 2.2\\Socio-economic sub-models\\Seawise project_ITA"
setwd(wd)

#This is the Catch table of FDI datacall, but the script can be adapted to read any dataset with landings and revenues.

land_data=readxl::read_excel("Landing.xlsx",sheet="Foglio1")
colnames(land_data)

library(dplyr)

# Fleet segments definition
# Here different fleet segmentations can be defined (e.g. metier-vessel_length; gsa-fishing_technique-metier; etc....)
# This stratification will be used to estimate the price model coefficients.

land_data$FS=""
land_data$FS=paste(land_data$COUNTRY, land_data$SUB_REGION,land_data$FISHING_TECH,land_data$VESSEL_LENGTH,land_data$METIER,sep="_")

# Selection of species and area
species<-c("MUT")
gsa<-c("GSA18")

land_data %>% group_by(SUB_REGION,FS,YEAR,SPECIES) %>% summarise(Landings=sum(Ton),Revenues=sum(Euro))
land_data$price<-0
land_data$price<-land_data$Euro/(land_data$Ton*1000)

land_data2<-land_data[land_data$SPECIES %in% species & land_data$SUB_REGION == gsa,]

# Model 1
# 

strata=unique(land_data2$FS)
i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=1

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
land_data2_temp=land_data2[land_data2$FS==strata[i],]
land_data2_temp$ratio=0
land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[-nrow(land_data2_temp)]
land_data2_temp$ratio2=999
land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]-land_data2_temp$Ton[-nrow(land_data2_temp)])/land_data2_temp$Ton[-1]
if(nrow(land_data2_temp)>3){
epsilon[i,]$epsilon=round(coefficients(lm((ratio-1)~ratio2+0,data=land_data2_temp[-1,])),5)
}
}
write.table(epsilon,"Price_model1.csv",sep=";",row.names=F)

# Model 2 



# Model 3
i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=3

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$FS==strata[i],]
  land_data2_temp$ratio=0
  land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[-nrow(land_data2_temp)]
  land_data2_temp$ratio2=999
  land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
  if (class(try(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[-1,])),silent=TRUE))!="try-error") {
  epsilon[i,]$epsilon=round(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[-1,]),silent=TRUE),3)
  } else {
    epsilon[i,]$epsilon=NA  
  }
}

write.table(epsilon,"Price_model3.csv",sep=";",row.names=F)

# Model 4

i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=4

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$FS==strata[i],]
  land_data2_temp$ratio=0
  land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[nrow(land_data2_temp)]
  #land_data2_temp$ratio2=999
  #land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
  if (class(try(coefficients(nls((ratio)~exp(a*Ton),start=list(a=1),data=land_data2_temp[-1,])),silent=TRUE))!="try-error") {
    epsilon[i,]$epsilon=round(coefficients(nls((ratio)~exp(a*Ton),start=list(a=1),data=land_data2_temp[-1,])),3)
  } else {
    epsilon[i,]$epsilon=NA  
  }
}

write.table(epsilon,"Price_model4.csv",sep=";",row.names=F)

# Model 5

i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=5
# epsilon$avg=0
 
for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$FS==strata[i],]
 
  #land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[nrow(land_data2_temp)]
  #land_data2_temp$ratio2=999
  #land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
 
    epsilon[i,]$epsilon=mean(land_data2_temp$price)  

}

write.table(epsilon,"Price_model5.csv",sep=";",row.names=F)

# Model 6
# In FLBEIA the price is estimated by age. To be checked with AZTI

i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=6

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$FS==strata[i],]
  land_data2_temp$ratio=0
  land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[1]
  land_data2_temp$ratio2=999
  land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[1]/land_data2_temp$Ton[-1])
  
  if (class(try(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[-1,])),silent=TRUE))!="try-error") {
    epsilon[i,]$epsilon=round(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[-1,])),3)
  } else {
    epsilon[i,]$epsilon=NA  
  }
}

write.table(epsilon,"Price_model6.csv",sep=";",row.names=F)

# Binding the model results

results=list.files(pattern="(model)",full.names = TRUE)
data_=read.table(results[1],sep=";",header=T) 

for (r in 2:5){
 data2=read.table(results[r],sep=";",header=T) 
 data_=rbind(data_,data2) 
}

write.table(data_,"coefficients.csv",sep=";",row.names=F)


land_data2_tempp=land_data2[1,]
land_data2_tempp$price_Mod1=0
#land_data2_tempp$price_Mod2=0
land_data2_tempp$price_Mod3=0
land_data2_tempp$price_Mod4=0
land_data2_tempp$price_Mod5=0
land_data2_tempp$price_Mod6=0
r=1
data_$RMSE=0
data_$points=0

dir.create(paste("FishPrice/",species,sep=""))
for (r in 1:length(strata)){
  
  jpeg(past("FishPrice/",species,"/graph",r,".jpg",sep=""),units="cm",width = 35,height=20,res=400)
# Observed
  land_data2_temp=land_data2[land_data2$FS==strata[r],]  
  
  data_[data_$FS==strata[r],]$points = nrow(land_data2_temp)
  
plot(land_data2_temp$YEAR,land_data2_temp$price, ylab="Price/kg (???)",xlab="Year",main=strata[r],ylim=c(0,1.2*max(land_data2_temp$price)),pch=19,cex=1.5,col="dark grey",cex.main=1.5,cex.axis=1.5,cex.lab=1.3)  
  
# Model 1
  
data_temp=data_[data_$MODEL==1 & data_$FS==strata[r],] 
land_data2_temp$price_Mod1=0
land_data2_temp$price_Mod1[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(1+data_temp$epsilon*(land_data2_temp$Ton[-1]-land_data2_temp$Ton[-nrow(land_data2_temp)])/land_data2_temp$Ton[-1])
land_data2_temp$price_Mod1[1]=NA
lines(land_data2_temp$YEAR,land_data2_temp$price_Mod1,col="green",lwd=4)  
data_[data_$MODEL==1 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[-1]-land_data2_temp$price_Mod1[-1])^2,na.rm=TRUE)/(length(land_data2_temp$price[-1])+1)) 


# Model 2



# Model 3

data_temp=data_[data_$MODEL==3 & data_$FS==strata[r],] 
land_data2_temp$price_Mod3=0
land_data2_temp$price_Mod3[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])^data_temp$epsilon
land_data2_temp$price_Mod3[1]=NA
lines(land_data2_temp$YEAR,land_data2_temp$price_Mod3,col="blue",lwd=4)  
data_[data_$MODEL==3 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[-1]-land_data2_temp$price_Mod3[-1])^2,na.rm=TRUE)/(length(land_data2_temp$price[-1])+1)) 


# Model 4
data_temp=data_[data_$MODEL==4 & data_$FS==strata[r],] 
land_data2_temp$price_Mod4=0
land_data2_temp$price_Mod4[-1]=land_data2_temp$price[nrow(land_data2_temp)]*exp(land_data2_temp$Ton[-1]*data_temp$epsilon)
land_data2_temp$price_Mod4[1]=NA
lines(land_data2_temp$YEAR,land_data2_temp$price_Mod4,col="orange",lwd=4)  
data_[data_$MODEL==4 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[-1]-land_data2_temp$price_Mod4[-1])^2,na.rm=TRUE)/(length(land_data2_temp$price[-1])+1)) 



# Model 5

data_temp=data_[data_$MODEL==5 & data_$FS==strata[r],] 
land_data2_temp$price_Mod5=data_temp$epsilon
#land_data2_temp$price_Mod3[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])^data_temp$epsilon
#land_data2_temp$price_Mod3[1]=NA
lines(land_data2_temp$YEAR,land_data2_temp$price_Mod5,col="black",lwd=3,lty="dotted")  
data_[data_$MODEL==5 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[-1]-land_data2_temp$price_Mod5[-1])^2,na.rm=TRUE)/(length(land_data2_temp$price[-1])+1)) 



# Model 6

data_temp=data_[data_$MODEL==6 & data_$FS==strata[r],] 
land_data2_temp$price_Mod6=0
land_data2_temp$price_Mod6[-1]=land_data2_temp$price[1]*(land_data2_temp$Ton[1]/land_data2_temp$Ton[-1])^data_temp$epsilon
land_data2_temp$price_Mod6[1]=NA
lines(land_data2_temp$YEAR,land_data2_temp$price_Mod6,col="turquoise",lwd=4)  
data_[data_$MODEL==6 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[-1]-land_data2_temp$price_Mod6[-1])^2,na.rm=TRUE)/(length(land_data2_temp$price[-1])+1)) 

land_data2_tempp=rbind(land_data2_tempp,land_data2_temp)

legend("topleft",c("Observed","Model1","Model3","Model4", "Model5", "Model6"),col=c("dark grey","green","blue","orange","black","turquoise"),pch=c(19,NA,NA,NA,NA,NA),lty=c(NA,"solid","solid","solid","dotted","solid"),lwd=c(NA,4,4,4,3,4))


dev.off()


}

write.table(land_data2_tempp[-1,],"Price_fittings.csv",sep=";",row.names=F)
write.table(data_,"Fit_indicators.csv",sep=";",row.names=F)


