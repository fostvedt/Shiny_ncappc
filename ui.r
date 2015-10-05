shinyUI(navbarPage("NCA Explorer",
  tabPanel("Input Data",
 #want another commit          
  h6("The App performs Non-Compartmental Analysis with the ncappc R library.
     The program is relient on free, open-source software and libraries that are made 
      available without warranty. No strenuous validation of this App has been conducted. 
      The code is available on github at www.github.com/fostvedt/shiny_ncappc."),
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
       h3("Table of NCA estimates"),
       h6("The download feature will not work if run through RStudio. Please open in your browser."),
       downloadButton("downloadNCA","Download NCA Estimates"),
       dataTableOutput("NCAval"),
       h3("Table of Statistics for the NCA estimates"),
       downloadButton("downloadNCAstat","Download NCA Summary Statistics
                      "),
       dataTableOutput("NCAstat")
       ),
tabPanel("Plots",
         plotOutput("plot")
)

      ) # close navbarPanel
  ) #close UI


