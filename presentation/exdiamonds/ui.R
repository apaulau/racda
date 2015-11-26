library(shiny)
library(ggplot2)

dataset <- diamonds

shinyUI(pageWithSidebar(
  headerPanel("Diamonds Explorer"),
  
  sidebarPanel(
    sliderInput(
      'sampleSize', 'Sample Size', min = 1, max = nrow(dataset),
      value = min(1000, nrow(dataset)), step = 500, round = 0
    ),
    
    # conditionalPanel("input.tabset == 'Plot'",)
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset))),
    
    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None = '.', names(dataset)))
  ),
  
#   mainPanel(
#     dataTableOutput('source'),
#     tableOutput('summary'),
#     plotOutput('plot')
#   )

  mainPanel(
    tabsetPanel(id = "tabset",
      tabPanel("Data",
               br(),
               dataTableOutput('source')
      ),
      tabPanel("Summary",
               br(),
               tableOutput('summary')
      ),
      tabPanel("Plot",
               plotOutput('plot')
      )
    )
  )
))


