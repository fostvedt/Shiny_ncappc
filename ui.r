shinyUI(navbarPage("NCA Explorer",
  tabPanel("Input Data",
           
#  h4("Non-Compartmental Analysis using the ncappc library"),
fluidRow(column(width = 3,
    helpText( a("ncappc package documentation",
                            target="_blank",href="https://cran.rstudio.com/web/packages/ncappc/vignettes/ncappc-vignette.html")),
    uiOutput("read_Origfile"),
    uiOutput("choose_IDvar")),
    column(width=3,
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_DOSE")), 
    column(width=3, 
           uiOutput("choose_TRT"),
           uiOutput("choose_extra")),

    mainPanel(width = 12,
    h4("Raw Data and Summary Statistics"), 
       dataTableOutput("Data"),
       verbatimTextOutput("summary")
    )
)),

tabPanel("Estimate NCA",
       fluidRow(
       column(3,uiOutput("Route"),
              uiOutput("choose_DUR")),
       column(3, uiOutput("DoseSchedule"),
              uiOutput("dosefreq")),
       column(3,uiOutput("AUC"),
              uiOutput("EstMeth"))
       ),
       h6("The download feature will not work if run through RStudio. Please open in your browser."),
       downloadButton("downloadNCA","Download NCA Estimates"),
       dataTableOutput("NCA")
       ),
tabPanel("Plots",
         plotOutput("plot")
)

      ) # close navbarPanel
  ) #close UI


