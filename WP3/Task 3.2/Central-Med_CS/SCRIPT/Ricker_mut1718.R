# SEAwise Task 3.2
# Mediterranean case in the Adriatic and western Ionian Seas (GSAs17-18-19)
# Angela Martiradonna, Isabella Bitetto, Walter Zupa, Maria Teresa Spedicato
# Fondazione COISPA ETS
# Mail to: ang.martiradonna@gmail.com

rm(list=ls())

library(modelr)
library(nlstools)
library(MuMIn)
library(rlist)
library(formula.tools)

data<-read.csv('Dataset.csv',sep=';',header=TRUE, dec=',')

case="Red mullet 17-18"
var=read.csv('MUT1718_10-200_Q3.csv',sep=';',header=TRUE)


ain=356.8583	
bin=1.39E-04


var$d_sst=c(NA,var$sst[2:length(var$sst)]-var$sst[1:(length(var$sst)-1)])
var$d_botT=c(NA,var$botT[2:length(var$botT)]-var$botT[1:(length(var$botT)-1)])
var$d_so=c(NA,var$so[2:length(var$so)]-var$so[1:(length(var$so)-1)])
var$d_botso=c(NA,var$botso[2:length(var$botso)]-var$botso[1:(length(var$botso)-1)])
var$d_nppv=c(NA,var$nppv[2:length(var$nppv)]-var$nppv[1:(length(var$nppv)-1)])


y=data[data$Stock==case,c(1)]
R=data[data$Stock==case,c(2)]
S=data[data$Stock==case,c(3)]
logR<-log(R)
logS<-log(S)

var=var[var$YEAR>=y[1] & var$YEAR<=y[length(y)],]
# botT<-var$d_botT
# sst <-var$d_sst
# so  <-var$d_so
# botso<-var$d_botso
# nppv<-var$d_nppv

botT<-var$botT
sst <-var$sst
so  <-var$so
botso<-var$botso
nppv<-var$nppv


pairs(cbind(logS,botT,sst,so,botso,nppv))
cor(cbind(logS,botT,sst,so,botso,nppv),use="na.or.complete")


data_case<-data[data$Stock==case,c(1,2,3)]
names(data_case)<-c("year","R","S")
data_case<-cbind(data_case,logR,logS,botT,sst,so,botso,nppv)

N=length(y)
k=6
tabella<-c()

case="Red mullet GSAs 17-18"

##################################################################################


mod<-nls(R~ a*S*exp(-b*S), start=list(a=ain,b=bin) )
err<-residuals(mod)
N_nona<-N-sum(length(which(is.na(err))))
SSE=sum(err^2,na.rm=T)
ME=sum(abs(err),na.rm=T)/N_nona
MSE=SSE/N_nona+ME^2

tabella<-rbind(tabella, c(
  paste("Ricker"),
  paste("R=a*S/(1+b*S)") ,
  coeffs(mod),"","", "",AIC(mod) , SSE, ME, MSE) )

modR<-list(classic=mod)


plot(y,R,lwd=1,pch=19,xlab="Year",ylab="Recruits",ylim=c(min(R)*0.9,max(R)*1.1))
lines(y,predict(modR[[1]]),lwd=3,col="black")
title(case)
legend_names<-"Ricker"
legend_color<-"black"
  legend_lty<-1
  
  
  ain<-coeffs(mod)[1]
  bin<-coeffs(mod)[2]
  cin<-0.1
  din<-0.1
  fin<-0.1
  
  
  #botT
  mod<-nls(R~a*S*exp(-b*S-c*botT), start=list(a=ain,b=bin,c=cin),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker botT"),
    as.character(formula(mod)),coeffs(mod),"","", AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, botT=mod)
  lines(y,predict(mod),lwd=3,col=3)
  legend_color<-c(legend_color,3)
  legend_names<-c(legend_names, "Ricker botT")
  legend_lty<-c(legend_lty,1)
  
  
  #sst
  mod<-nls(R~a*S*exp(-b*S-c*sst), start=list(a=ain,b=bin,c=cin),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker sst"),
    as.character(formula(mod)),coeffs(mod),"", "",AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, sst=mod)
  lines(y,predict(mod),lwd=3,col=4)
  legend_color<-c(legend_color,4)
  legend_names<-c(legend_names, "Ricker sst")
  legend_lty<-c(legend_lty,1)
  
  
  # # #so
  # mod<-nls(R~a*S*exp(-b*S-c*so), start=list(a=ain,b=bin,c=cin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker so"),
  #   as.character(formula(mod)),coeffs(mod),"", "",AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, so=mod)
  # lines(y,predict(mod),lwd=3,col=5)
  # legend_color<-c(legend_color,5)
  # legend_names<-c(legend_names, "Ricker so")
  # legend_lty<-c(legend_lty,1)
  
  # 
  # # #botso
  # mod<-nls(R~a*S/(1+b*S)*exp(-c*botso), start=list(a=ain,b=bin,c=cin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botso"),
  #   as.character(formula(mod)),coeffs(mod),"","", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botso=mod)
  # lines(y,predict(mod),lwd=3,col=6)
  # legend_color<-c(legend_color,6)
  # legend_names<-c(legend_names, "Ricker botso")
  # legend_lty<-c(legend_lty,1)
  
  
  #nppv
  mod<-nls(R~a*S*exp(-b*S-c*nppv), start=list(a=ain,b=bin,c=cin),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker nppv"),
    as.character(formula(mod)),coeffs(mod),"", "",AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, nppv=mod)
  lines(y,predict(mod),lwd=3,col=7)
  legend_color<-c(legend_color,7)
  legend_names<-c(legend_names, "Ricker nppv")
  legend_lty<-c(legend_lty,1)
  
  ##################################################################################################
  
  #botT-sst
  mod<-nls(R~ a*S*exp(-b*S-c*botT-d*sst), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker botT-sst"),
    as.character(formula(mod)),coeffs(mod),"",AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, botTsst=mod)
  lines(y,predict(mod),lwd=3,col=11,lty=2)
  legend_color<-c(legend_color,11)
  legend_names<-c(legend_names, "Ricker botT-sst")
  legend_lty<-c(legend_lty,2)
  
  
  # # #botT-so
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*so), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-so"),
  #   as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTso=mod)
  # lines(y,predict(mod),lwd=3,col=3,lty=2)
  # legend_color<-c(legend_color,3)
  # legend_names<-c(legend_names, "Ricker botT-so")
  # legend_lty<-c(legend_lty,2)
  
  
  # # #botT-botso
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*botso), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-botso"),
  #   as.character(formula(mod)),coeffs(mod),"",AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTbotso=mod)
  # lines(y,predict(mod),lwd=3,col=4,lty=2)
  # legend_color<-c(legend_color,4)
  # legend_names<-c(legend_names, "Ricker botT-botso")
  # legend_lty<-c(legend_lty,2)
  
  
  #botT-nppv
  mod<-nls(R~ a*S*exp(-b*S-c*botT-d*nppv), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker botT-nppv"),
    as.character(formula(mod)),coeffs(mod),"",AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, botTnppv=mod)
  lines(y,predict(mod),lwd=3,col=5,lty=2)
  legend_color<-c(legend_color,5)
  legend_names<-c(legend_names, "Ricker botT-nppv")
  legend_lty<-c(legend_lty,2)
  
  
  # # #sst-so
  # mod<-nls(R~ a*S*exp(-b*S-c*sst-d*so), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker sst-so"),
  #   as.character(formula(mod)),coeffs(mod),"",AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, sstso=mod)
  # lines(y,predict(mod),lwd=3,col=6,lty=2)
  # legend_color<-c(legend_color,6)
  # legend_names<-c(legend_names, "Ricker sst-so")
  # legend_lty<-c(legend_lty,2)
  
  
  # # #sst-botso
  # mod<-nls(R~ a*S*exp(-b*S-c*sst-d*botso), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker sst-botso"),
  #   as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, sstbotso=mod)
  # lines(y,predict(mod),lwd=3,col=7,lty=2)
  # legend_color<-c(legend_color,7)
  # legend_names<-c(legend_names, "Ricker sst-botso")
  # legend_lty<-c(legend_lty,2)
  
  
  #sst-nppv
  mod<-nls(R~ a*S*exp(-b*S-c*sst-d*nppv), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  err<-residuals(mod)
  N_nona<-N-sum(length(which(is.na(err))))
  SSE=sum(err^2,na.rm=T)
  ME=sum(abs(err),na.rm=T)/N_nona
  MSE=SSE/N_nona+ME^2
  tabella<-rbind(tabella, c(
    paste("Ricker sst-nppv"),
    as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  modR<-list.append(modR, sstnppv=mod)
  lines(y,predict(mod),lwd=3,col=8,lty=2)
  legend_color<-c(legend_color,8)
  legend_names<-c(legend_names, "Ricker sst-nppv")
  legend_lty<-c(legend_lty,2)
  
  # # #so-botso
  # mod<-nls(R~a*S*exp(-b*S-c*so-d*botso), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker so-botso"),
  #   as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, sonppv=mod)
  # lines(y,predict(mod),lwd=3,col=9,lty=2)
  # legend_color<-c(legend_color,9)
  # legend_names<-c(legend_names, "Ricker so-botso")
  # legend_lty<-c(legend_lty,2)
  
  
  # # #so-nppv
  # mod<-nls(R~a*S*exp(-b*S-c*so-d*nppv), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker so-nppv"),
  #   as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, sonppv=mod)
  # lines(y,predict(mod),lwd=3,col=9,lty=2)
  # legend_color<-c(legend_color,9)
  # legend_names<-c(legend_names, "Ricker so-nppv")
  # legend_lty<-c(legend_lty,2)
  
  
  # # #botso-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*botso-d*nppv), start=list(a=ain,b=bin,c=cin,d=din),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botso-nppv"),
  #   as.character(formula(mod)),coeffs(mod),"", AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botsonppv=mod)
  # lines(y,predict(mod),lwd=3,col=10,lty=2)
  # legend_color<-c(legend_color,10)
  # legend_names<-c(legend_names, "Ricker botso-nppv")
  # legend_lty<-c(legend_lty,2)
  
  ######################################################################################################
  
  # # #botT-sst-so
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*sst-f*so), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-sst-so"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=3,lty=3)
  # legend_color<-c(legend_color,3)
  # legend_names<-c(legend_names, "Ricker botT-sst-so")
  # legend_lty<-c(legend_lty,3)
  
  
  # # #botT-sst-botso
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*sst-f*botso), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-sst-botso"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=4,lty=3)
  # legend_color<-c(legend_color,4)
  # legend_names<-c(legend_names, "Ricker botT-sst-botso")
  # legend_lty<-c(legend_lty,3)
  
  
  # #botT-sst-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*sst-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-sst-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=5,lty=3)
  # legend_color<-c(legend_color,5)
  # legend_names<-c(legend_names, "Ricker botT-sst-nppv")
  # legend_lty<-c(legend_lty,3)
  

  # # #botT-so-botso
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*so-f*botso), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-so-botso"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=6,lty=3)
  # legend_color<-c(legend_color,6)
  # legend_names<-c(legend_names, "Ricker botT-so-botso")
  # legend_lty<-c(legend_lty,3)


  # # #botT-so-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*so-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-so-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=7,lty=3)
  # legend_color<-c(legend_color,7)
  # legend_names<-c(legend_names, "Ricker botT-so-nppv")
  # legend_lty<-c(legend_lty,3)


  # # #botT-botso-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*botT-d*botso-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker botT-botso-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=8,lty=3)
  # legend_color<-c(legend_color,8)
  # legend_names<-c(legend_names, "Ricker botT-botso-nppv")
  # legend_lty<-c(legend_lty,3)


  # # #sst-so-botso
  # mod<-nls(R~ a*S*exp(-b*S-c*sst-d*so-f*botso), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker sst-so-botso"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=9,lty=3)
  # legend_color<-c(legend_color,9)
  # legend_names<-c(legend_names, "Ricker sst-so-botso")
  # legend_lty<-c(legend_lty,3)


  # # #sst-so-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*sst-d*so-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker sst-so-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=10,lty=3)
  # legend_color<-c(legend_color,10)
  # legend_names<-c(legend_names, "Ricker botT-so-nppv")
  # legend_lty<-c(legend_lty,3)


  # # #sst-botso-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*sst-d*botso-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker sst-botso-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=11,lty=3)
  # legend_color<-c(legend_color,11)
  # legend_names<-c(legend_names, "Ricker botT-botso-nppv")
  # legend_lty<-c(legend_lty,3)


  # # #so-botso-nppv
  # mod<-nls(R~ a*S*exp(-b*S-c*so-d*botso-f*nppv), start=list(a=ain,b=bin,c=cin,d=din,f=fin),data=data_case )
  # err<-residuals(mod)
  # N_nona<-N-sum(length(which(is.na(err))))
  # SSE=sum(err^2,na.rm=T)
  # ME=sum(abs(err),na.rm=T)/N_nona
  # MSE=SSE/N_nona+ME^2
  # tabella<-rbind(tabella, c(
  #   paste("Ricker so-botso-nppv"),
  #   as.character(formula(mod)),coeffs(mod), AIC(mod) , SSE, ME, MSE) )
  # modR<-list.append(modR, botTsst=mod)
  # lines(y,predict(mod),lwd=3,col=12,lty=3)
  # legend_color<-c(legend_color,12)
  # legend_names<-c(legend_names, "Ricker so-botso-nppv")
  # legend_lty<-c(legend_lty,3)
  
  #################################################################################
  
  
  
  legend("topright",legend=legend_names,col=legend_color,lwd=c(2,2),lty=legend_lty)
  
  
  tabella<-as.data.frame(tabella)
  colnames(tabella)<-c("Model","Formula","a","b","c","d","f","AIC","SSE","ME","MSE")
  #tabella=rbind(names,tabella)
  write.table(tabella, file = paste(case,"Ricker.csv"),row.names=F,col.names=T,sep=";")