shinyUI(fluidPage(
  h4("Non-Compartmental Analysis using the ncappc library"),
  
  sidebarPanel(
    uiOutput("read_Origfile"),
    br(),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_IDvar"),
    uiOutput("choose_TRT"),
    br()),
  
   
  mainPanel(
    h4("Summary Statistics"),
    plotOutput("plot"),
    verbatimTextOutput("summary")
  )
))

