# packages to use
library(ggplot2)


# raw data
load(file = "~/Desktop/sfei/perStation.Rda")
load(file = "~/Desktop/sfei/perStationParsimoniousModels.Rda")
load(file = "~/Desktop/sfei/perStationFullModels.Rda")

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  dat <- reactive({
    
    stat <- input$stat
  
    
    out<-perStation[[which(names(perStation)==stat)]]
   
    
    return(out)
    
  })
  
  # for initial date range
  output$daterng <- renderUI({
    
    rngs <- range(dat()$Date)
    
    dateRangeInput("dt_rng",
                   label = h4("Date range"), 
                   start = rngs[1], 
                   end = rngs[2],
                   startview = 'year'
    )
    
  })
  
  ## plots
  
  # floplot
  output$fittedPars<- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    stat <- input$stat
    index=which(names(perStation)==stat)
    
    # data
    data<-dat()
    mod<-perStationParsMod[[index]]
    
    toUse=na.omit(data[,c("doy","date_dec","pheo","tn","do_per","Date")])
    toUse=as.data.frame(cbind.data.frame(toUse,mod$fitted.values))
  names(toUse)[7]="fitted.values"
    ggplot(data,aes(x = Date, y = chl))+geom_point()+
      geom_line(data=toUse,aes(x=Date,y =fitted.values ,col="red"),lwd=1)+
      ggtitle(paste(names(perStation)[index], "Fitted Values Parsimonious Model",sep=" "))+
      theme(legend.position='none')+
      scale_x_date(limits = dt_rng)+ylab("chl a (microgram/L)")+xlab("Date")
   
    
  }, height = 250, width = 1200)
  
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    taus <- input$tau
    
    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    
    # create plot
    fitplot(dat(), annuals = annuals, tau = taus, dt_rng = dt_rng, size = 3, alpha = 0.8, min_mo = 11, 
            logspace = logspace) +
      theme_minimal() +
      theme(legend.position = 'none',
            axis.title.x = element_blank()
      )
    
  }, height = 250, width = 1200)
  
  # predictions and flow norms plot
  output$nrmplot <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    taus <- input$tau
    
    # scale argument
    logspace <- TRUE
    if(input$scl == 'linear') logspace <- FALSE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    
    # create plot
    fitplot(dat(), annuals = annuals, predicted = F, tau = taus, dt_rng = dt_rng, size = 3, alpha = 0.8, 
            min_mo = 11, logspace = logspace) + 
      theme_minimal() +
      theme(legend.position = 'none', 
            axis.title.x = element_blank()
      )
    
  }, height = 250, width = 1200)
  
  
})