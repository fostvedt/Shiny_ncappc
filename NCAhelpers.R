
################
# function to do the NCA estimation. 
# This function is called in the next defined function below. 
################
nca.choice <- function(data,pk=NULL,time=NULL,id=NULL,ds=NULL,
                       grp=NULL,dtype=NULL,auc=c(0,24),route=RouAd,method="linear-log",
                       tau=24,dur=0,bE=FALSE,dunits=NULL,cunits=NULL,tunits=NULL,
                       gr1=NULL,gr2=NULL,gr3=NULL){
  
  ncappc(obsFile = data, doseTime = 0, str1Nm=gr1, str1=NULL,
         str2Nm=gr2, str2=NULL, str3Nm=gr3, str3=NULL,
         concUnit = cunits, timeUnit = tunits, doseUnit = dunits,
         doseNormUnit = NULL, obsLog = "FALSE", idNmObs = id, timeNmObs = time,
         concNmObs = pk, AUCTimeRange = auc, backExtrp = "FALSE",
         LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = ds,
         adminType = route, doseType = dtype, Tau = tau, TI = dur,
         method = method, timeFormat = "number",  
         tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                    "Vz_obs", "Cl_obs", "HL_Lambda_z"), 
         figFormat = "png",  noPlot = "TRUE",
         printOut = "FALSE", studyName = "test")
  
  # Work around for not being able to pass a NULL object into the ncappc function
  if("None Specified" %in% ncaOutput$Level1){ ncaOutput$Level1 <- NULL ; ObsStat$Level1 <- NULL}
  if("None Specified" %in% ncaOutput$Level2){ ncaOutput$Level2 <- NULL ; ObsStat$Level2 <- NULL}
  if("None Specified" %in% ncaOutput$Level3){ ncaOutput$Level3 <- NULL ; ObsStat$Level3 <- NULL}
  
  return(list(ncaOutput,ObsStat))
}


################
# function to pass along the data for NCA estimation 
# based on what is provided by the user (e.g. was dose available or ID available, etc.)
# The conditions are defined before the ifelse statement as true/false and
# based on the combination that is true, the "correct" values are passed into the NCA estimation
################

nca.est <- function(data,AUCmax,RouAd,method,NSS,tau,dur=0,cunits="ng/ml",dunits="mg",tunits="hrs"){
  
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
  
  if(method=="Linear Up - Log Down") method = "linear-log"
  if(method=="Linear-Log Trapezoid") method = "linear"
  
  # This is making sure the "ns" or "ss" is in the right format
  if(is.null(NSS)) NSS="Non-Steady State"
  dtype <- "ns"
  bE="TRUE"
  if(NSS=="Steady State") {
    dtype <- "ss"
    bE="FALSE"
  }
  
  tunits = ifelse(tunits=="minutes", "min",ifelse(tunits=="hours","hr","d"))
  
  # logical arguments to determine which arguments to pass into nca.choice()
  # !is.null() means the variable is in the data frame
  a1 <- !is.null(data$Conc)
  a2 <- !is.null(data$Time)
  a3 <- !is.null(data$AMT)  
  a4 <- !is.null(data$ID)
  
  # creating boolean decisions to detect columns in dataset
  ct <- a1 & a2 & !a3 & !a4  # If conc and time defined 
  ctd <- a1 & a2 & a3 & !a4  # If conc, time, and dose defined 
  cti <- a1 & a2 & !a3 & a4 # If ID, time, and conc defined
  ctid <- a1 & a2 & a3 & a4 # If ID, conc, time, and dose defined  
  
  
  
  gr1 = "Level1"; gr2="Level2";gr3="Level3"
  
  if(is.null(data$Level1)) data$Level1 = "None Specified"
  if(is.null(data$Level2)) data$Level2 = "None Specified"
  if(is.null(data$Level3)) data$Level3 = "None Specified"
  
  # If ID, conc, time, dose, treatment, and extra defined  
  ifelse(ct, #If conc and time defined 
         ncaOutput <- nca.choice(data,pk="Conc",time="Time",auc=AUCmax,route=RouAd, 
                                 method=method,dtype=dtype,bE=bE,tau=tau,dur=dur,
                                 cunits=cunits,tunits=tunits,gr1=gr1,gr2=gr2,gr3=gr3),
         
         ifelse(ctd, # If conc, time, and dose defined 
                ncaOutput <- nca.choice(data,pk="Conc",time="Time",ds="AMT",auc=AUCmax,
                                        route=RouAd, method=method,dtype=dtype,bE=bE,tau=tau,dur=dur,
                                        cunits=cunits,tunits=tunits,gr1=gr1,gr2=gr2,gr3=gr3),
                
                ifelse(cti, # If ID, time, and conc defined
                       ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",auc=AUCmax,
                                               route=RouAd, method=method,dtype=dtype,bE=bE,tau=tau,dur=dur,
                                               cunits=cunits,tunits=tunits,gr1=gr1,gr2=gr2,gr3=gr3),
                       
                       ifelse(ctid, # If ID, conc, time, and dose defined 
                              ncaOutput <- nca.choice(data,pk="Conc",time="Time",id="ID",ds="AMT",
                                                      auc=AUCmax,route=RouAd, method=method,dtype=dtype,
                                                      bE=bE,tau=tau,dur=dur,dunits=dunits,cunits=cunits,tunits=tunits,
                                                      gr1=gr1,gr2=gr2,gr3=gr3),
                              
                              ncaOutput <- NULL #returning nothing if one of the above is not satisfied
                       ))))
  ncaOutput
}



