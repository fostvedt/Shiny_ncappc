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
  

  
  output$summary <- renderPrint({
    if(is.null(input$origfile) | is.null(origData))
    { return()
    } else
    { 
      summary(origData)
    }
  })
  
  newEntry <- reactive({
    newLine <- isolate(c(input$choose_Xvar,input$choose_Yvar,input$choose_IDvar,
                         input$choose_TRT,input$choose_DOSE))
  })
  
  output$Data <-  renderPrint({ newEntry() })

  
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

