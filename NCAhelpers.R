
################
# function to do the NCA estimation. 
# This function is called in the next defined function below. 
################
nca.choice <- function(data,pk=NULL,time=NULL,id=NULL,ds=NULL,trt=NULL,
                  grp=NULL,dtype=NULL,auc=c(0,24),route=RouAd,method="linear-log",
                  tau=24,dur=0,bE=FALSE){
  
ncappc(obsFile = data, doseTime = 0,
       concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",
       doseNormUnit = NULL, obsLog = "FALSE", idNmObs = id, timeNmObs = time,
       concNmObs = pk, AUCTimeRange = auc, backExtrp = "FALSE",
       LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = ds,
       adminType = route, doseType = dtype, Tau = tau, TI = dur,
       method = method, timeFormat = "number",  
       tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                  "Vz_obs", "Cl_obs", "HL_Lambda_z"), 
       figFormat = "png",  noPlot = "TRUE",
       printOut = "FALSE", studyName = "test")
return(list(ncaOutput,ObsStat))
}


################
# function to pass along the data for NCA estimation 
# based on what is provided by the user (e.g. was dose available or ID available, etc.)
# The conditions are defined before the ifelse statement as true/false and
# based on the combination that is true, the "correct" values are passed into the NCA estimation
################

nca.est <- function(data,AUCmax,RouAd,method,NSS,tau,dur=0){

  # column names for the newEntry() dataframe are 
  # c("ID","Time","Conc","Day","Treatment","Dose","gnam","Group 1")
  # If no ID is given, assume
  if(!is.null(data$AMT) & !is.numeric(data$AMT)){
    data$AMT <- as.numeric(as.character(data$AMT))
  }

   # Added these lines since the format of the passed along arguments 
   # doesn't seem to be in the right class. Making sure they are the
   # correct class (e.g. Dose must be numeric)
  if(is.null(AUCmax)) AUCmax <- c(0,24)
  AUCmax <- as.numeric(AUCmax)   # the partial AUC value selected by the user
  tau <- as.numeric(tau)    # dosing frequency. Only needed for steady-state
  dur <- as.numeric(dur)    # Duration of infusion
  RouAd  <- as.character(RouAd)   # eg "extravascular"
  method <- as.character(method)  # e.g. "mixed"
  
  # This is making sure the "ns" or "ss" is in the right format
  if(is.null(NSS)) NSS="Non-Steady State"
  dtype <- "ns"
  bE="TRUE"
  if(NSS=="Steady State") {
    dtype <- "ss"
    bE="FALSE"
  }

  
  # logical arguments to determine which arguments to pass into nca.choice()
  # !is.null() means the variable is in the data frame
  a1 <- !is.null(data$Conc)
  a2 <- !is.null(data$Time)
  a3 <- !is.null(data$AMT)  
  a4 <- !is.null(data$Treatment)
  a5 <- !is.null(data$ID)
  a6 <- !is.null(data$Group1)
  
  #  a7 <- !is.null(data$DUR) # TBD
  

  # creating boolean decisions to detect columns in dataset
  ct <- a1 & a2 & !a3 & !a4 & !a5 & !a6 # If conc and time defined 
  ctd <- a1 & a2 & a3 & !a4 & !a5 & !a6 # If conc, time, and dose defined 
  cti <- a1 & a2 & !a3 & !a4 & a5 & !a6 # If ID, time, and conc defined
  ctid <- a1 & a2 & a3 & !a4 & a5 & !a6 # If ID, conc, time, and dose defined  
  ctidt <- a1 & a2 & a3 & a4 & a5 & !a6 # If ID, conc, time, dose, and treatment defined  
  ctide <- a1 & a2 & a3 & !a4 & a5 & a6 # If ID, conc, time, dose, and extra defined  
  ctidte <- a1 & a2 & a3 & a4 & a5 & a6 # If ID, conc, time, dose, treatment, and extra defined 
  #ctidtef <- a1 & a2 & a3 & a4 & a5 & a6 &a7 # If ID, conc, time, dose, treatment, dur and extra defined  
  
  
  
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
           
   #        ifelse(ctidtef, # If ID, conc, time, dose, treatment, and extra defined 
    #              ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",
     #                     ds="AMT",trt="Treatment",grp="Group 1",auc=AUCmax,route=RouAd, 
      #                    method=method,dtype=dtype,bE=bE,tau=tau,dur=dur),
                  
    ncaOutput <- NULL #returning nothing if one of the above is not satisfied
     )))))))

  ncaOutput
}



