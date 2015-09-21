library(shiny)
library(DT)
library(devtools)
library(ggplot2)
library(grid)
install_github("cacha0227/ncappc")
source("PKhelpers.R")
source("NCAhelpers.R")
library(ncappc)

shinyServer(function(input, output) {
  
  ####################################################
  # 
  # Code for front page UI
  # 
  ####################################################
  
  
  
  # This is setting up the file upload
  # The idea is that anyone can upload their own file
  # and get NCA estimates along with plots
  origData<<-NULL
  output$read_Origfile <- renderUI({
    fileInput("origfile",label="Insert PK file",accept=c('.csv','.txt','.sim','.dat'))    
  })

  
 
  # Time Variable
  # This UI selects the Time variable. Usually this will
  # be on the x-axis for a PK-conc plot
  output$choose_Xvar <- renderUI({
    if(is.null(input$origfile))
      return()    
    origData.name<-paste(input$origfile$datapath,input$origfile$name,sep="/") 
    origData<<-read.PKPDdata(input$origfile$datapath)   
    colnames <- colnames(origData)
    selectInput("Xvar", "Choose Time variable", 
                choices  = c(" ",colnames))
  })
  
  # Concentration variable
  # This UI selects the Concentration variable. Usually this will
  # be on the y-axis for a PK-conc plot
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
  
  # ID variable
  # This UI selects the ID variable. Usually this will
  # be used for stratification as well as estimation
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
  
  # Treatment variable
  # This UI selects the Treatment variable. Usually this will
  # be used for stratification as well as estimation
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
  
  # Dose variable
  # This UI selects the Dose variable. Usually this will
  # be used for stratification as well as estimation
  # need this to estimate clearance = Dose/AUC
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
  
  output$choose_DAY <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    selectInput("Day", "Choose Day", choices = choice.temp )
  })
  
   
  # Extra Stratification variables
  # e.g. fed/fasted
  # must be selected from the variables uplaoded
  output$choose_extra <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    
    selectizeInput("Group", "Choose Other Grouping", choices = choice.temp,
                multiple=T,options = list(maxItems = 3) )
  })

  output$choose_DUR <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    selectInput("DUR", "Choose Duration", choices = choice.temp )
  })
  
  # This is outputted so that the user can see
  # what data they have available to choose from when 
  #selecting the variables for the estimation
  output$summary <- renderPrint({
    if(is.null(input$origfile) | is.null(origData))
    { return()
    } else
    { 
      summary(origData)
    }
  })
  
  # This line is creating a new data frame using only the columns
  # selected by the user. This will make it easier to 
  # get the NCA estimates as well as visualize the data
  newEntry <- reactive({
    newLine <- c(ID=input$IDvar, 
                 Time = input$Xvar,
                 Conc = input$Yvar,
                 Day = input$Day,
                 Treatment = input$TRTvar,
                 Dose = input$Dose,
                 Duration = input$DUR,
                 Group = input$Group)
    newLine2 <- newLine[which(newLine!= " ")]
    dat <- origData[,newLine2]
    
    gnam <- paste0("Group",1:length(input$Group))
    nam <- c("ID","Time","Conc","Day","Treatment","Dose", "DUR", gnam)
    
    if(length(newLine2)<=1){ return(dat)}
    else{
    colnames(dat) <- nam[which(newLine!= " ")]
    return(dat)
    }
  }) 
  
  # Showing the user what data variables they have selected for use
  # in the NCA
  #output$Data <-  renderPrint({ head(newEntry()) })
  output$Data <-  renderDataTable({ 
    if(is.null(input$origfile) | is.null(origData))
     return()
     #else  head(origData)
    else head(newEntry())
    })
  
  
  ####################################################
  # code for NCA estimation tab
  ####################################################
  
  # This affects the NCA estimation
  # The options are the specific arguments for the
  # ncappc function. 
  output$Route <- renderUI({
    if(is.null(input$origfile))
      return() 
    radioButtons("route", "Route of Administration",
                 c("extravascular","iv-bolus","iv-infusion")
    )
  })
  
  # This affects the NCA. The options in the ncappc function
  # are "ns" non-steady state
  # and "ss" for steady state.
  output$DoseSchedule <- renderUI({
    if(is.null(input$origfile))
      return() 
    radioButtons("Sched", "Dosing",
                 c("Non-Steady State","Steady State")
    )
  })
  
  output$EstMeth <- renderUI({
    if(is.null(input$origfile))
      return() 
    radioButtons("method", "Estimation Method",
                 c("mixed","linear","log-linear"))
  })
  
  
  # The function requires an AUC time interval
  # The default is (0,24)
  # The output is not returned though :/ from ncappc
  
  output$AUC <- renderUI({
    sliderInput("AUCmax", "select AUC interval",
                min=0,max=150,value=c(0,24))
  })
  
  
  output$dosefreq <- renderUI({
    if(is.null(input$origfile) | is.null(origData))
      return() 
    numericInput("dfreq", "Dosing Frequency (hours)",
                value=1,min=0,max=168)
  })
  
  
  
  ####################################################
  # code for NCA estimation
  ####################################################
  
  # This calls nca.est in the NCAhelpers.r file
  # This function does all the data checking
  NCAestimates <- reactive({
    if(is.null(input$origfile) | is.null(origData) | is.null(newEntry()) )
      return()
    else 
    nca.est(newEntry(), input$AUCmax,input$route,input$method, input$Sched,input$dfreq)
  })
    
  output$NCA <- renderDataTable({
    if(is.null(input$origfile) | is.null(origData) | is.null(newEntry()) )
      return()
    else 
      NCAestimates()
  })
  

#  output$NCA = renderPrint({  
#    NCAd()
#})
  
  output$downloadNCA <- downloadHandler(
    filename = function() {paste0("NCAest", '.csv')},
    content  = function(file){
      write.csv(NCAestimates(),file)
    }
  )
  
  
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
      return()
    else if(input$Xvar==" " | input$Yvar==" ") 
      return()
    else if(input$IDvar==" " & input$TRTvar== " ")  
      XYplot.orig(origData,input$Xvar,input$Yvar)
    else if(input$Dose==" ")
      PK.ID.orig(origData,input$Xvar,input$Yvar,input$IDvar)  
    else
      NCA.PPC.SINGLE(origData,input$Xvar,input$Yvar,input$IDvar,input$Dose)   
  }) #closing render table
  
})

