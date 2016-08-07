library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  h2("Delta and  Suisun GAM results"),
  
  # Sidebar with a slider input for number of observations
  fluidRow(
    
    column(2, 
           
           selectInput(inputId = 'stat',
                       label = h4('Station'),
                       choices = c('C10', 'C3', 'P8', 'D19', 'D26', 'D28', 'D4', 'D6', 'D7'), 
                       selected = 'C10')
           
    ),
    
    column(2, 
           
           selectInput(inputId = 'res', 
                       label = h4('Variable'),
                       choices = c('din', 'nh', 'no23'), 
                       selected = 'din')
           
    ),
    
    column(2, 
           
           selectInput(inputId = 'annuals', 
                       label = h4('Plot type'), 
                       choices = c('annual', 'observed'), 
                       selected = 'observed'
           )
           
    ),
    
    column(2, 
           
           selectInput(inputId = 'scl', 
                       label = h4('Scale type'), 
                       choices = c('natural log', 'linear'), 
                       selected = 'linear'
           )
           
    ),
    
    column(2, 
           
           uiOutput("daterng")
           
    ),
    
    
           
    #),
    
    width = 12
    
  ),
  
  
  # output tabs
  mainPanel(
    
    #plotOutput("floplot", height = "100%"),
    plotOutput("fitplot", height = "100%"),
    plotOutput("nrmplot", height = "100%"),
    plotOutput("nestedPlotFlow",height="100%"),
    plotOutput("nestedPlotNoFlow",height="100%"),
    plotOutput("comparePlot",height="100%"),
    plotOutput("dynagamP",height="100%"),
    h5("Below All on Log Scale Always: RMSE per Station for Chosen Response"),
    plotOutput("spatialPlotRMSEG",height="100%"),
   plotOutput("spatialPlotRMSEW",height="100%"),
    
    width = 9
    
  )
  
))