
read.PKPDdata<-function(file.name)
{ 
  temp.data<-read.csv(file.name,na.strings=".")
  colnames(temp.data)<-toupper(colnames(temp.data))
  return(temp.data)
}



XYplot.orig<-function(orig.data,X.name,Y.name,ID.name)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t)
  ggplot( data=new.data, aes(x=X.t, y=Y.t))+
     geom_point()+geom_line()+labs(x=X.name,y=Y.name) + scale_y_log10()
}   


PK.ID.orig<-function(orig.data,X.name,Y.name,ID.name)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t)
  ggplot( data=new.data, aes(x=X.t,y=Y.t))+
    geom_point()+geom_line()+labs(x=X.name,y=Y.name)+ scale_y_log10()+
    facet_wrap(~ID.t)
}   

PK.TRT.orig<-function(orig.data,X.name,Y.name,ID.name,TRT)
{  
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  trt <- orig.data[,TRT]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t,trt=trt)
  ggplot( data=new.data, aes(x=X.t,y=Y.t,group=trt))+
    geom_point()+geom_line()+labs(x=X.name,y=Y.name)+ scale_y_log10()+
    facet_wrap(~ID.t)
}   


#ncappc(obsFile=dsingle, doseNm="Dose", doseAmtNm = "Dose", idNmObs="ID", timeNmObs="Time", concNmObs = "Conc", 
 #      tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs", "Vz_obs", "Cl_obs", "HL_Lambda_z"), noPlot = "TRUE")

NCA.PPC.SINGLE<-function(orig.data,X.name,Y.name,ID.name,TRT,Time, Conc, ID, DOSE)
{  
  X.t<-orig.data[,c(X.name, Time)]
  Y.t<-orig.data[,c(Y.name, Conc)]
  ID.t<-orig.data[,c(ID.name, ID)]
  trt <- orig.data[,TRT]
  Dose <- orig.data[,c(DOSE, Dose)]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t,trt=trt, Dose=Dose)
  ncappc(obsFile = new.data, doseNm = "Dose",
         #concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",  obsLog = "FALSE", 
         idNmObs = "ID.t", timeNmObs = "X.t",
         concNmObs = "Y.t", AUCTimeRange = c(0,24), # backExtrp = "TRUE", LambdaTimeRange = NULL, LambdaExclude = NULL, 
         doseAmtNm = "Dose",
         adminType = "extravascular", doseType = "ns", Tau = NULL, TI = NULL,
         method = "mixed", timeFormat = "number",  tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                                                              "Vz_obs", "Cl_obs", "HL_Lambda_z"),# figFormat = "png",  
         noPlot = "TRUE", #printOut = "TRUE", studyName = "test"
         )
}


