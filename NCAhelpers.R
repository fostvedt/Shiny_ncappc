

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

  # creating boolean decisions to detect columns in dataset
  ct <- a1 & a2 & !a3 & !a4 & !a5 & !a6   # If conc and time defined 
  ctd <- a1 & a2 & a3 & !a4 & !a5 & !a6  # If conc, time, and dose defined 
  cti <- a1 & a2 & !a3 & !a4 & a5 & !a6 # If ID, time, and conc defined
  ctid <- a1 & a2 & a3 & !a4 & a5 & !a6 # If ID, conc, time, and dose defined  
  ctidt <- a1 & a2 & a3 & a4 & a5 & !a6 # If ID, conc, time, dose, and treatment defined  
  ctide <- a1 & a2 & a3 & !a4 & a5 & a6 # If ID, conc, time, dose, and extra defined  
  ctidte <- a1 & a2 & a3 & a4 & a5 & a6 # If ID, conc, time, dose, treatment, and extra defined  
  
  
  # If ID, conc, time, dose, treatment, and extra defined  
  ifelse(ct, #If conc and time defined 
          nca.choice(data,pk="Conc",time="Time",auc=24),
    ifelse(ctd, # If conc, time, and dose defined 
          nca.choice(data,pk="Conc",time="Time",ds="Dose",auc=24),
    ifelse(cti, # If ID, time, and conc defined
          nca.choice(data,pk="Conc",time="Time",id="ID",auc=24),
    ifelse(ctid, # If ID, conc, time, and dose defined 
           nca.choice(data,pk="Conc",time="Time",id="ID",ds="Dose",auc=24),
    ifelse(ctidt, # If ID, conc, time, dose, and treatment defined
           nca.choice(data,pk="Conc",time="Time",id="ID",ds="Dose",trt="Treatment",auc=24),
    ifelse(ctide,  # If ID, conc, time, dose, and extra defined  
           nca.choice(data,pk="Conc",time="Time",id="ID",ds="Dose",grp="Group 1",auc=24),
    ifelse(ctidte, # If ID, conc, time, dose, treatment, and extra defined 
           nca.choice(data,pk="Conc",time="Time",id="ID",ds="Dose",trt="Treatment",grp="Group 1",auc=24),
    ncaOutput <- NULL #returning nothing if one of the above is not satisfied
     )))))))

  ncaOutput
)



