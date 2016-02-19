

shinyUI(fluidPage( 
  
  navbarPage("", selected = "NCA",id="nav", inverse="FALSE", title="Non-Compartmental Analysis",
             tabPanel("NCA set-up",
                      tabsetPanel(id= "tabs", 
                                  type="tabs",
                                  tabPanel("Load Data"),
                                  tabPanel("Select Variables"),
                                  tabPanel("NCA Arguments"),
                                  #tabPanel("Output")
                                  br()
                      ),
                      fluidRow(
                        conditionalPanel(condition= "input.tabs=='Load Data'",
                                         column(width = 3,
                                                uiOutput("read_Origfile"),
                                                br() 
                                         )),
                        conditionalPanel(condition= "input.tabs=='Select Variables'",
                                         column(width=3,
                                                uiOutput("choose_IDvar"),
                                                uiOutput("choose_Xvar"),
                                                uiOutput("choose_Yvar"),
                                                uiOutput("choose_DOSE"),
                                                br()),
                                         column(width=3,
                                                uiOutput("choose_TRT"),
                                                uiOutput("choose_extra"),
                                                br() ),
                                         column(width=3,
                                                uiOutput("dose_units"),
                                                uiOutput("conc_units"),
                                                uiOutput("time_units"), 
                                                br() )
                        ),
                        conditionalPanel(condition= "input.tabs=='NCA Arguments'",
                                         column(3,
                                                uiOutput("EstMeth")),
                                         column(3,uiOutput("Route"),
                                                uiOutput("choose_DUR")),
                                         column(3, uiOutput("DoseSchedule"),
                                                uiOutput("dosefreq")),
                                         column(3,uiOutput("AUC")),
                                         br() )
                      )),
             
             navbarMenu("View Data",
                        tabPanel("Raw Data",
                                 h4("Summary Raw Data"), 
                                 verbatimTextOutput("summary")
                        ), 
                        tabPanel("NCA Data",
                                 h4("Selected PK Data for NCA"), 
                                 dataTableOutput("Data"),
                                 br()
                        ),
                        tabPanel("Visuals",
                                 plotOutput("plot"),
                                 br()
                        )),
             
             navbarMenu("NCA Results",
                        tabPanel("Estimates",
                                 h3("Table of NCA estimates"),
                                 h6("The download feature will not work if run through RStudio. Please open in your browser."),
                                 downloadButton("downloadNCA","Download NCA Estimates"),
                                 dataTableOutput("NCAval")
                        ),
                        tabPanel("Summary Stats",
                                 h3("Table of Statistics for the NCA estimates"),
                                 h6("The download feature will not work if run through RStudio. Please open in your browser."),
                                 downloadButton("downloadNCAstat","Download NCA Summary Statistics"),
                                 dataTableOutput("NCAstat"),
                                 br()
                        )
             ),
             tabPanel("About", 
                      #want another commit          
                      h6("The App performs Non-Compartmental Analysis with the ncappc R library.
                         The program is relient on free, open-source software and libraries that are made 
                         available without warranty. No strenuous validation of this App has been conducted. 
                         The code is available on github at www.github.com/fostvedt/shiny_ncappc."),
                      helpText( a("ncappc package documentation",
                                  target="_blank",
                                  href="https://github.com/cacha0227/ncappc/blob/master/vignettes/ncappc-vignette.md")),
                      includeMarkdown("ncappcabout.Rmd"),
                      br()
                      )
)

)          
) # close navbarPanel
#      ) #close UI


