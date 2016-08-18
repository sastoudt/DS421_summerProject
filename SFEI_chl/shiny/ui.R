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
                       choices = c('C10', 'C3', "D10","D12","D19","D22","D26","D28A","D4",
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
    
    column(2, 
           
           uiOutput("ylim12")
           
    ),
    
    column(2, 
           
           uiOutput("ylim34L")
           
    ),
    
    column(2, 
           
           uiOutput("ylim34U")
           
    ),
    
    column(2, 
           
           uiOutput("plot5choice1")
           
    ),
 
    column(2, 
           
           uiOutput("plot5choice2")
           
    ),
    column(2, 
           
           uiOutput("plot6choice1")
           
    ),
    
    column(2, 
           
           uiOutput("plot6choice2")
           
    ),
    
    column(2,

           selectInput(inputId = 'intVar1',
                       label = 'Select a variable in the interaction model',
                       choices = c('doy', 'date_dec', 'interaction'),
                       selected = 'doy')

    ),
    
    column(2,
           
           selectInput(inputId = 'intVar2',
                       label = 'Select a variable in the interaction model',
                       choices = c('doy', 'date_dec', 'interaction'),
                       selected = 'date_dec')
           
    ),

    column(2,
           
           selectInput(inputId = 'spatMod',
                       label = 'Select a spatial model.',
                       choices = c('spatIntercept', 'spatDate_Dec',"spatDOY" ,'spatinteraction'),
                       selected = 'spatIntercept')
           
    ),
    
    # column(2,
    #        
    #        selectInput(inputId = 'spatMod2',
    #                    label = 'Select a spatial model.',
    #                    choices = c('spatIntercept', 'spatDate_Dec',"spatDOY" ,'spatinteraction'),
    #                    selected = 'spatDate_Dec')
    #        
    # ),
    # 
    # column(2,
    #        
    #        selectInput(inputId = 'spatMod3',
    #                    label = 'Select a spatial model.',
    #                    choices = c('spatIntercept', 'spatDate_Dec',"spatDOY" ,'spatinteraction'),
    #                    selected = 'spatDOY')
    #        
    # ),
    # 
    # column(2,
    #        
    #        selectInput(inputId = 'spatMod4',
    #                    label = 'Select a spatial model.',
    #                    choices = c('spatIntercept', 'spatDate_Dec',"spatDOY" ,'spatinteraction'),
    #                    selected = 'spatinteraction')
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
    plotOutput("mapPlot",height="100%"),
    plotOutput("fittedPars", height = "100%"),
    plotOutput("fittedFull", height = "100%"),
    plotOutput("fittedInt",height="100%"),
    plotOutput("fittedSpat",height="100%"),
    plotOutput("fittedChl",height="100%"),
    plotOutput("fittedFlow",height="100%"),
    plotOutput("nestedPlotPars", height = "100%"),
    plotOutput("nestedPlotFull",height="100%"),
    plotOutput("nestedPlotInt",height="100%"),
    plotOutput("nestedPlotSpat",height="100%"),
    #plotOutput("nestedPlotSpat2",height="100%"),
    #plotOutput("nestedPlotSpat3",height="100%"),
    #plotOutput("nestedPlotSpat4",height="100%"),
    plotOutput("nestedPlotChl",height="100%"),
    plotOutput("nestedPlotParsJust2",height="100%"),
    plotOutput("nestedPlotFullJust2",height="100%"),
    plotOutput("nestedPlotIntJust2",height="100%"),
    plotOutput("flowInput",height="100%"),
    width = 9
    
  )
  
))