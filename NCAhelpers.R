

nca.est <- function(data){
  nam <- names(data)
  if(!is.null(data$Dose) & !is.numeric(data$Dose)){
    data$Dose <- as.numeric(as.character(dataNCA$Dose))
  }
  
  
  

ncappc(obsFile = data,  grNm = "Group 1", grp =NULL,
       flNm = NULL, flag = NULL, doseNm = "Dose", dose = NULL,
       concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",
       doseNormUnit = NULL, obsLog = "FALSE", idNmObs = "ID", timeNmObs = "Time",
       concNmObs = "Conc", AUCTimeRange = c(0,input$AUCmax), backExtrp = "TRUE",
       LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = "DOSE",
       adminType = "extravascular", doseType = "ns", Tau = NULL, TI = NULL,
       method = "mixed", timeFormat = "number",  
       tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                  "Vz_obs", "Cl_obs", "HL_Lambda_z"), figFormat = "png",  noPlot = "TRUE",
       printOut = "FALSE", studyName = "test")

return(ncaOutput)
)