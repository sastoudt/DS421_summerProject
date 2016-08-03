library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  h2("Per Station GAM Models for Chlorophyll"),
  
  # Sidebar with a slider input for number of observations
  fluidRow(
    
    column(2, 
           
           selectInput(inputId = 'stat',
                       label = h4('Station'),
                       choices = c('C10', 'C3', "D10","D12","D15","D19","D22","D26","D28A","D4",
                                   "D41","D6","D7","D8","MD10","P8"), 
                       selected = 'C10')
           
    ),
    
    # column(2, 
    #        
    #        selectInput(inputId = 'res', 
    #                    label = h4('Variable'),
    #                    choices = c('din', 'nh', 'no23'), 
    #                    selected = 'din')
    #        
    # ),
    
    # column(2, 
    #        
    #        selectInput(inputId = 'annuals', 
    #                    label = h4('Plot type'), 
    #                    choices = c('annual', 'observed'), 
    #                    selected = 'observed'
    #        )
    #        
    # ),
    
    # column(2, 
    #        
    #        selectInput(inputId = 'scl', 
    #                    label = h4('Scale type'), 
    #                    choices = c('natural log', 'linear'), 
    #                    selected = 'linear'
    #        )
    #        
    # ),
    
    column(2, 
           
           uiOutput("daterng")
           
    ),
    
    # column(2, 
    #        
    #        checkboxGroupInput("tau", 
    #                           label = h4("Quantiles"),
    #                           choices = c("0.1" = "0.1", "0.5" = "0.5", "0.9" = "0.9"),
    #                           selected = c('0.1', '0.5', '0.9'), 
    #                           inline = T
    #        )
    #        
    # ),
    
    width = 12
    
  ),
  
  
  # output tabs
  mainPanel(
    
    ## fitted values parsimonious
    ## nested structure parsimonious
    ## fitted values full
    ## nested structure parsimonious
    
    plotOutput("fittedPars", height = "100%"),
    plotOutput("fittedFull", height = "100%"),
    plotOutput("nestedPlotPars", height = "100%"),
    plotOutput("nestedPlotFull",height="100%"),
    
    width = 9
    
  )
  
))