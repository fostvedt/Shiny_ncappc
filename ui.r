shinyUI(navbarPage("NCA Explorer",
  tabPanel("Input Data",
           
#  h4("Non-Compartmental Analysis using the ncappc library"),
  sidebarPanel(width = 3,
    uiOutput("read_Origfile"),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_IDvar"),
    uiOutput("choose_TRT"),
    uiOutput("choose_DOSE"),
    uiOutput("choose_DAY"),
    uiOutput("choose_extra"),
    br()),
  
  mainPanel(width = 9,
    h4("Summary Statistics and Plot"), 
       dataTableOutput("Data"),
       verbatimTextOutput("summary")
    )
),

tabPanel("Summary",
       uiOutput("AUC"),
       uiOutput("Route"),
       uiOutput("DoseSchedule"),
       actionButton("NCAest", "Estimate NCA"),
       dataTableOutput("NCA"),
       plotOutput("plot")
       )
      ) # close navbarPanel
  ) #close UI


