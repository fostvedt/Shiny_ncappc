

nca.choice <- function(data,pk=NULL,time=NULL,id=NULL,ds=NULL,trt=NULL,grp=NULL,auc=24){                       )
ncappc(obsFile = data,  grNm = grp, gr =NULL,
       flNm = NULL, flag = NULL, doseNm = ds, dose = NULL,
       concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",
       doseNormUnit = NULL, obsLog = "FALSE", idNmObs = id, timeNmObs = time,
       concNmObs = pk, AUCTimeRange = c(0,auc), backExtrp = "TRUE",
       LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = ds,
       adminType = "extravascular", doseType = "ns", Tau = NULL, TI = NULL,
       method = "mixed", timeFormat = "number",  
       tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                  "Vz_obs", "Cl_obs", "HL_Lambda_z"), figFormat = "png",  noPlot = "TRUE",
       printOut = "FALSE", studyName = "test")
return(ncaOutput)
}




nca.est <- function(data){

  # column names for the newEntry() dataframe are 
  # c("ID","Time","Conc","Day","Treatment","Dose","gnam","Group 1")
  # If no ID is given, assume
  if(!is.null(data$Dose) & !is.numeric(data$Dose)){
    data$Dose <- as.numeric(as.character(dataNCA$Dose))
  }
  

  a1 <- !is.null(data$Conc)
  a2 <- !is.null(data$Time)
  a3 <- !is.null(data$Dose)  
  a4 <- !is.null(data$Treatment)
  a5 <- !is.null(data$ID)
  a6 <- !is.null(data$Group1)

  # If conc and time defined 
  if(a1 & a2 & !a3 & !a4 & !a5 & !a6){

    return(ncaOutput)
  }
  
  # If conc, time, and dose defined 
  if(a1 & a2 & a3 & !a4 & !a5 & !a6)
    
  # If ID, time, and conc defined
  if(a1 & a2 & !a3 & !a4 & a5 & !a6)
      
  # If ID, conc, time, and dose defined  
  if(a1 & a2 & a3 & !a4 & a5 & !a6)  
  
  # If ID, conc, time, dose, and treatment defined  
  if(a1 & a2 & a3 & a4 & a5 & !a6)
  
  # If ID, conc, time, dose, and extra defined  
  if(a1 & a2 & a3 & !a4 & a5 & a6){
    
  }
    
  # If ID, conc, time, dose, treatment, and extra defined  
  if(a1 & a2 & a3 & a4 & a5 & a6){
    
  }
  
  else
    return(ncaOutput)
  }
)