shinyUI(navbarPage("NCA Explorer",
  tabPanel("Input Data",
           
#  h4("Non-Compartmental Analysis using the ncappc library"),
fluidRow(column(width = 3,
    uiOutput("read_Origfile"),
    uiOutput("choose_IDvar"),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_DOSE")), 
    column(width=3, 
           uiOutput("choose_TRT"),
           uiOutput("choose_extra"),
           uiOutput("choose_DAY")),
    column(width=3, 
    uiOutput("choose_DUR")),
    
    mainPanel(width = 12,
    h4("Raw Data and Summary Statistics"), 
       dataTableOutput("Data"),
       verbatimTextOutput("summary")
    )
)),

tabPanel("Estimate NCA",
       fluidRow(
       column(3,uiOutput("Route"),
         uiOutput("DoseSchedule")),
       column(3,uiOutput("dosefreq"),
              uiOutput("EstMeth")),
       column(3,uiOutput("AUC"))
       ),
       downloadButton("downloadNCA","Download NCA Estimates"),
       dataTableOutput("NCA")
       ),
tabPanel("Plots",
         plotOutput("plot")
)

      ) # close navbarPanel
  ) #close UI


