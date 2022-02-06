library(shiny)
library(data.table)

ui=fluidPage(
  fluidRow(
    column(8,tags$img(src="shinyimage3.jpg"),offset = 0.5)
  ),
  
  fluidRow(
    HTML("<br/>"),
    column(9,tableOutput("crosstab"),offset=0.5)
  ),
  
  fluidRow(
    HTML("<br/><br/>"),
    column(4, h5(tags$b("Vin tiré au hasard correspondant aux critères : ")),h5(em(textOutput("thew"))),offset = 0)
  ),
    fluidRow(
    column(1, actionButton(inputId="rand",label="Random",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),offset=0)
  ),
  
  fluidRow(
    HTML("<br/><br/>"),
    column(8,h5(tags$b("Les fiches produit du vin tiré au hasard : ")),offset = 0.5),
    column(8,uiOutput('mytabs'),offset = 0)
  )
  
)

  