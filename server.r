library(shiny)
library(ggplot2)
library(grid)
source("PKhelpers.R")
library(ncappc)

shinyServer(function(input, output) {
  origData<<-NULL
  output$read_Origfile <- renderUI({
    fileInput("origfile",label="Insert PK file",accept=c('.csv','.txt','.sim','.dat'))    
  })

  
  output$choose_Xvar <- renderUI({
    if(is.null(input$origfile))
      return()    
    origData.name<-paste(input$origfile$datapath,input$origfile$name,sep="/") 
    origData<<-read.PKPDdata(input$origfile$datapath)   
    colnames <- colnames(origData)
    selectInput("Xvar", "Choose Time variable", 
                choices  = c(" ",colnames))
  })
  
  output$Route <- renderUI({
    if(is.null(input$origfile) | is.null(origData))
      return()
  radioButtons('route', 'Route of Administration',
               c("extravascular","iv-bolus","iv-infusion")
  )
  })
  
  output$DoseSchedule <- renderUI({
    if(is.null(input$origfile) | is.null(origData))
      return()
    radioButtons('Sched', 'Dosing',
                 c("Single dose", "Multiple Dose")
    )
  })
  
  output$choose_Yvar <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }
    selectInput("Yvar", "Choose Concentration variable", 
                choices  =choice.temp )
  })
  
  output$choose_IDvar <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    selectInput("IDvar", "Choose ID variable", 
                choices  =choice.temp )
  })
  
  output$choose_TRT <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    
    selectInput("TRTvar", "Choose Treatment variable", 
                choices  = choice.temp )
  })
  
  output$choose_DOSE <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    
    selectInput("Dose", "Choose Dose", choices = choice.temp )
  })
  
  output$choose_extra <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    
    selectInput("Group", "Choose Other Grouping", choices = choice.temp,multiple=T )
  })

  
  output$summary <- renderPrint({
    if(is.null(input$origfile) | is.null(origData))
    { return()
    } else
    { 
      summary(origData)
    }
  })
  
  newEntry <- reactive({
    newLine <- c(ID=input$IDvar, 
                 Time = input$Xvar,
                 Conc = input$Yvar,
                 Treatment = input$TRTvar,
                 Dose = input$Dose)
    newLine2 <- newLine[which(newLine!= " ")]
    origData[,newLine2]
  })
  
  output$Data <-  renderPrint({ head(newEntry()) })

  ####################################################
  # code for NCA estimation tab
  ####################################################
  
  output$AUC <- renderUI({
    selectInput("AUCmax", "select AUC interval: (0,Selection)",
                choices=c(8,12,24,48,72,Inf),selected=24)
  })
  
  ####################################################
  # code for NCA estimation
  ####################################################
  
  output$NCA = renderTable({
    #adding a submit button. Will only run once the users clicks the button
    input$NCAest()
    #warnings
#    if(colnames(newData()) return()
    
    
    ncappc(obsFile = newData(),  grNm = "DAY", grp =NULL,
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
    ncaOutput
  })
  
  ####################################################
  # code for graphics using NCA estimates
  ####################################################
  
  output$plot<-renderPlot({
    if(is.null(input$origfile) | is.null(input$Xvar)| is.null(input$Yvar))
      return()
    else if(input$Xvar==" " | input$Yvar==" ") 
           return()
    else if(input$IDvar==" " & input$TRTvar== " ")  
          XYplot.orig(origData,input$Xvar,input$Yvar)
    else if(input$TRTvar==" ")
          PK.ID.orig(origData,input$Xvar,input$Yvar,input$IDvar)  
    else
          PK.TRT.orig(origData,input$Xvar,input$Yvar,input$IDvar,input$TRTvar)  
  }) #closing render plot
  
  output$table<-renderTable({
    if(is.null(input$origfile) | is.null(input$Xvar)| is.null(input$Yvar))
      return(
      NCA.PPC.SINGLE(origData,input$Xvar,input$Yvar,input$IDvar,input$TRTvar, input$Dose) )
  })  
  }) #closing render table
  
#})

