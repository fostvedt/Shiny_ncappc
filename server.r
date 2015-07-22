source("PKhelpers.R")

shinyServer(function(input, output, session) {
  #  output$out6 <- renderPrint((input$in6))
  origData <<- NULL
  output$dosefile <- renderUI({
    fileInput("dfile", label="Upload dosing Schedule", accept=c('.csv','.txt','.dat'))
  })
  
  
  output$missd <- renderUI({
    if(is.null(input$dfile))
      return()    
    origData.name<-paste(input$dfile$datapath,input$dfile$name,sep="/") 
    origData<<-read.csv(input$dfile$datapath) 
    numdose <- nrow(origData)
    selectInput("miss", "Choose Missed Doses", 
                choices  = c(" ",1:numdose), multiple=TRUE,selectize=TRUE)
  })
  
  output$table1 = renderTable({
    if(is.null(origData))
      return()
    origData
  })
  
  
  
  
  
  output$dosered <- renderUI({
    if(is.null(input$dfile))
      return()    
    numdose <- nrow(origData)
    selectInput("dayreduct", "Beginning Dose Reduction", 
                choices  = c(" ",1:numdose))
  })
  
  output$reddose <- renderUI({
    if(is.null(input$dfile))
      return()    
    numericInput("dayreduct", value=NULL,"Reduction Dosage (mg)")
  })
  
  
  vis <- reactive({
    dose <- 500
    schedule <- c(0)
    time <- 24
    DS <- dose.prof(dose,time,schedule)
    PK <- PKsim.data(ka=1,v=600,ke=.05,dose,f=1,DS, t.half=25)
    samp.time <- as.numeric(input$in6) 
    if(input$timescale=="Hours") samp.time <- samp.time/24
    #samp.time <- c(c(.5,1,2,3,4,6,8,9,24,48,72,96,120)/24)
    PKgraph <- plot.PK(PK,samp.time) %>%
      add_axis("x", title = "Day") %>%
      add_axis("y", title = "Concentration")# %>%
  })
  vis %>% bind_shiny("plot1")
  
  output$table2 = renderTable({
    if(input$timescale=="Hours") {
      data.frame(
        Day = as.integer(ceiling(sort(as.numeric(input$in6))/24)),
        Hr.Post.Dose = sort(as.numeric(input$in6))
      )}
    else{
      data.frame(
        Day = as.integer(sort(as.numeric(input$in6))),
        Hr.Post.Dose = sort(as.numeric(input$in6)*24)
      )}
  })
  
  
})