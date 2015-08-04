library(grid)

output$NCA = renderDataTable({
    input$NCAestimate

    ncappc(obsFile = selectedData(),  grNm = "DAY", grp =NULL,
           flNm = NULL, flag = NULL, doseNm = "Dose", dose = NULL,
           concUnit = "[ng].[mL]", timeUnit = "[hr]", doseUnit = "[mg]",
           doseNormUnit = NULL, obsLog = "FALSE", idNmObs = "ID", timeNmObs = "Time",
           concNmObs = "Conc", AUCTimeRange = c(0,24), backExtrp = "TRUE",
           LambdaTimeRange = NULL, LambdaExclude = NULL, doseAmtNm = "Dose",
           adminType = "extravascular", doseType = "ns", Tau = NULL, TI = NULL,
           method = "mixed", timeFormat = "number",  
           tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs",
                      "Vz_obs", "Cl_obs", "HL_Lambda_z"), figFormat = "png",  noPlot = "TRUE",
           printOut = "FALSE", studyName = "test")
    names(dat) = c("Team", "Divison", "Record", "Colley", "Score",
                   "Colley Rank", "Score Rank")
    ncaOutput
}, options = list(pageLength = 10))

