shinyUI(fluidPage(
  h4("Non-Compartmental Analysis using the ncappc library"),
  
  sidebarPanel(width = 3,
    uiOutput("read_Origfile"),
    uiOutput("Route"),
    uiOutput("DoseSchedule"),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_IDvar"),
    uiOutput("choose_TRT"),
    uiOutput("choose_DOSE"),
    br()),
  
  mainPanel(width = 9,
            tabsetPanel(
              tabPanel("Summary", br(), 
                        h4("Summary Statistics and Plot"), 
                        plotOutput("plot"),
                       verbatimTextOutput("Data")
                        ),
              tabPanel("NCA", br(), 
                       h4("NCA Parameter Estimates"), 
                       verbatimTextOutput("summary")
                       )
              ) # close tabsetPanel
  ) #close main Panel
))

