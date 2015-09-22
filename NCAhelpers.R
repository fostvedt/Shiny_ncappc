

nca.choice <- function(data,pk=NULL,time=NULL,id=NULL,ds=NULL,trt=NULL,
                  grp=NULL,dtype=NULL,bE="TRUE",auc=c(0,24),route=RouAd,method="mixed",
                  tau=tau,dur=dur){
  
ncappc(obsFile = data,  
       #grNm = grp, gr =NULL,
       #flNm = NULL, flag = NULL, 
       doseNm = ds, dose = NULL,
       concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",
       doseNormUnit = NULL, obsLog = "FALSE", idNmObs = id, timeNmObs = time,
       concNmObs = pk, AUCTimeRange = auc, backExtrp = bE,
       LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = ds,
       adminType = route, doseType = dtype, Tau = tau, TI = dur,
       method = method, timeFormat = "number",  
       tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                  "Vz_obs", "Cl_obs", "HL_Lambda_z"), figFormat = "png",  noPlot = "TRUE",
       printOut = "FALSE", studyName = "test")
return(ncaOutput)
}




nca.est <- function(data,AUCmax,RouAd,method,NSS,tau,dur){

  # column names for the newEntry() dataframe are 
  # c("ID","Time","Conc","Day","Treatment","Dose","gnam","Group 1")
  # If no ID is given, assume
  if(!is.null(data$AMT) & !is.numeric(data$AMT)){
    data$AMT <- as.numeric(as.character(data$AMT))
  }

  if(is.null(AUCmax)) AUCmax <- c(0,24)
  AUCmax <- as.numeric(AUCmax) 
  tau <- as.numeric(tau)
  dur <- as.numeric(dur)
  RouAd  <- as.character(RouAd)
  method <- as.character(method) 
  
  #NSS <- as.character(NSS)
  if(is.null(NSS)) NSS="Non-Steady State"
  dtype <- "ns"
  bE="TRUE"
  if(NSS=="Steady State") {
    dtype <- "ss"
    bE="FALSE"
  }

  a1 <- !is.null(data$Conc)
  a2 <- !is.null(data$Time)
  a3 <- !is.null(data$AMT)  
  a4 <- !is.null(data$Treatment)
  a5 <- !is.null(data$ID)
  a6 <- !is.null(data$Group1)
  a7 <- !is.null(data$DUR)
  

  # creating boolean decisions to detect columns in dataset
  ct <- a1 & a2 & !a3 & !a4 & !a5 & !a6   # If conc and time defined 
  ctd <- a1 & a2 & a3 & !a4 & !a5 & !a6  # If conc, time, and dose defined 
  cti <- a1 & a2 & !a3 & !a4 & a5 & !a6 # If ID, time, and conc defined
  ctid <- a1 & a2 & a3 & !a4 & a5 & !a6 # If ID, conc, time, and dose defined  
  ctidt <- a1 & a2 & a3 & a4 & a5 & !a6 # If ID, conc, time, dose, and treatment defined  
  ctide <- a1 & a2 & a3 & !a4 & a5 & a6 # If ID, conc, time, dose, and extra defined  
  ctidte <- a1 & a2 & a3 & a4 & a5 & a6 # If ID, conc, time, dose, treatment, and extra defined 
  ctidtef <- a1 & a2 & a3 & a4 & a5 & a6 &a7 # If ID, conc, time, dose, treatment, dur and extra defined  
  
  
  
  # If ID, conc, time, dose, treatment, and extra defined  
  ifelse(ct, #If conc and time defined 
         ncaOutput <- nca.choice(data,pk="Conc",time="Time",auc=AUCmax,route=RouAd, 
                                 method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
         
    ifelse(ctd, # If conc, time, and dose defined 
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",ds="AMT",auc=AUCmax,
                           route=RouAd, method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
           
    ifelse(cti, # If ID, time, and conc defined
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",auc=AUCmax,
                           route=RouAd, method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
           
    ifelse(ctid, # If ID, conc, time, and dose defined 
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",ds="AMT",
                           auc=AUCmax,route=RouAd, method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
           
    ifelse(ctidt, # If ID, conc, time, dose, and treatment defined
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",ds="AMT",
                          trt="Treatment",auc=AUCmax,route=RouAd, method=method,
                          dtype=dtype,bE=bE,tau=tau,dur=dur),
           
    ifelse(ctide,  # If ID, conc, time, dose, and extra defined  
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",ds="AMT",
                          grp="Group 1",auc=AUCmax,route=RouAd, method=method,
                          dtype=dtype,bE=bE,tau=tau,dur=dur),
           
    ifelse(ctidte, # If ID, conc, time, dose, treatment, and extra defined 
           ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",ds="AMT",
                          trt="Treatment",grp="Group 1",auc=AUCmax,route=RouAd, 
                          method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
           
           ifelse(ctidtef, # If ID, conc, time, dose, treatment, and extra defined 
                  ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",
                          ds="AMT",trt="Treatment",grp="Group 1",auc=AUCmax,route=RouAd, 
                          method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
                  
    ncaOutput <- NULL #returning nothing if one of the above is not satisfied
     ))))))))

  ncaOutput
}



