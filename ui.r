library(shiny)
library(ggvis)
shinyUI(fluidPage(
  #h4("Dosing Times"),
  uiOutput("dosefile"),
  fluidRow(
    column(4,
           h4("Model"),
           selectInput('Drug', 'Drug Administration', c("IV-Bolus","Oral 1-Comp"),
                       selected = "Oral 1-Comp", multiple=FALSE, selectize=TRUE),
           selectInput('compartment','Compartments',c("1-Compartment","2-Compartment"),selected = "1-Compartment"),
           selectInput('elimination','Elimination',c("Linear","Michaelis-Menton"),selected="Linear")),
    column(4,
           h4("Sampling Times"),
           selectInput('timescale','Time Scale',c("Days","Hours")),
           selectInput('in6', 'Sample Times', 0:100/2, multiple=TRUE, selectize=TRUE)
           
    ),
    column(4,
           hr(),
           submitButton("Update Plot and Table"),
           uiOutput("dosechange"),
           uiOutput("missd"),
           uiOutput("dosered"),
           uiOutput("reddose")
           #selectInput('Missdose', 'Select Doses to Miss', c(1:28), multiple=TRUE, selectize=TRUE),
           #selectInput('in61', 'Sample Times', 0:100/2, multiple=TRUE, selectize=TRUE)
           #tableOutput('table2')
    )
  )  ,
  
  fluidRow(
    column(2,
           tableOutput('table1')
    ),
    column(1
           #tableOutput('table2')
    ),
    column(9,
           ggvisOutput("plot1")
    )
  )
))