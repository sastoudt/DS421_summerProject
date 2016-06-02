lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
  )
)

stalk <- list(
  shrt = c('sjr', 'sac', 'sal'),
  lngs = c('San Joaquin', 'Sacramento', 'Salinity')
)

flowPlot_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  if(scale){
    data$res=exp(data$res)
    mod$fitted.values=exp(mod$fitted.values)
    modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]

  # title
  txt <- paste0(data$resdup[1], ' ~ s(time) + s(season) + s(flo)') 

  ggplot(data, aes(x = date, y = res))+geom_point()+
    geom_line(aes(y = mod$fitted.values, color = 'Predicted'),lwd=1)+
    geom_line(aes(y = modNoFlow$fitted.values, color = 'Flow-normalized'), lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
  scale_colour_manual(name = '', 
    labels = c('Predicted', 'Flow-normalized'), 
    values =c('darkblue','orange')
    ) + 
  ggtitle(txt)
}
## need legend




flowPlotNorm_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  if(scale){
    data$res=exp(data$res)
    mod$fitted.values=exp(mod$fitted.values)
    modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))

  }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  data$month=as.numeric(unlist(lapply(data$date,function(x){format(x,"%m")})))
  data$predVal=mod$fitted.values
  data$predValNoFlow=modNoFlow$fitted.values
  months=1:12
  normalGrid=as.data.frame(months)
  
  normalGrid$normVal=rep(NA,nrow(normalGrid))
  normalGrid$normValNoFlow=rep(NA,nrow(normalGrid))
  names(normalGrid)[1]=c("month")
  
  for(i in 1:nrow(normalGrid)){
    
    sub=subset(data,  month==normalGrid[i,1])
    
    normalGrid$normVal[i]=mean(sub$predVal,na.rm=T)
    normalGrid$normValNoFlow[i]=mean(sub$predValNoFlow,na.rm=T)
    #print(i)
    
  }
  
  # title
  txt <- paste0(data$resdup[1], ' ~ s(time) + s(season)') 
  
  data=merge(data,normalGrid,by.x="month",by.y="month")
  data=data[order(data$date),]
  ggplot(data, aes(x = date, y = res))+geom_point()+
    geom_line(aes(y = normVal, color = 'Predicted'),lwd=1)+
    geom_line(aes(y = normValNoFlow, color = 'Flow-normalized'),lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
  scale_colour_manual(name = '', 
    labels = c('Predicted', 'Flow-normalized'), 
    values =c('darkblue','orange')
    ) + 
  ggtitle(txt)
  
}



# packages to use
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)

# raw data
load(file = 'data/modelsNoLag_default.RData')
load(file = 'data/modelsNoLag_NoFlow_default.RData')
load(file="data/dataNice_nolag.RData")
load(file="data/dataPredNice_nolag.RData")

shinyServer(function(input, output) {
  
  ##
  # data
  
  # model
  mod <- reactive({
    
    stat <- input$stat
    res <- input$res
    #scl <- input$scl
    
    out <- which(names(modelsNoLag_default)==paste(stat,res,sep="_"))

    return(out)
    
  })
  
  dat <- reactive({
    
    stat <- input$stat
    res <- input$res
    #scl <- input$scl
    
    out <- which(names(dataNiceNoLag)==paste(stat,res,sep="_"))
    return(out)
    
  })
  
  # for initial date range
  output$daterng <- renderUI({
    
    dat=dataNiceNoLag[[dat()]]
    
    rngs <- range(dat$date)
    
    dateRangeInput("dt_rng",
                   label = h4("Date range"), 
                   start = rngs[1], 
                   end = rngs[2],
                   startview = 'year'
    )
    
  })
  
  ## plots
  
  # floplot
  # output$floplot <- renderPlot({
  # 
  #   # inputs
  #   dt_rng <- input$dt_rng
  #   scl <- input$scl
  #   stat <- input$stat
  # 
  #   # data
  #   flo <- dat()
  #   florng <- attr(flo, 'floobs_rng')
  #   flolab <- attr(flo, 'flolab')
  #   flo <- dplyr::select(flo, date, flo) %>%
  #     mutate(flo = flo * abs(diff(florng)) + florng[1]) %>%
  #     na.omit
  # 
  #   # change scale/labels depending on station and scale
  #   if(stat %in% c('D4', 'D6', 'D7')){
  # 
  #     ylab <- 'ln-salinity'
  # 
  #     if(scl == 'linear'){
  # 
  #       flo$flo <- exp(flo$flo) - 1
  #       ylab <- 'salinity'
  # 
  #     }
  # 
  #   } else {
  # 
  #     ylab <- 'ln-flow'
  # 
  #     if(scl == 'linear'){
  # 
  #       flo$flo <- exp(flo$flo)
  #       ylab <- 'flow'
  # 
  #     }
  # 
  #   }
  # 
  #   ggplot(flo, aes(x = date, y = flo)) +
  #     geom_line() +
  #     scale_y_continuous(ylab) +
  #     theme_minimal() +
  #     theme(axis.title.x = element_blank()) +
  #     scale_x_date(limits = dt_rng)
  # 
  # }, height = 250, width = 1200)
  # 
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    #taus <- input$tau
    
    # scale argument
    logspace <- FALSE
    if(input$scl == 'linear') logspace <- TRUE
    ## if true, need to untransform
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    ### NEED TO DO ####
    
    dat=dataNiceNoLag[[dat()]]
   
    # create plot
    flowPlot_SAS(dat,modelsNoLag_NoFlow_default[[mod()]],modelsNoLag_default[[mod()]],xlim=dt_rng,scale=logspace)
   
  }, height = 250, width = 1200)
  
  # predictions and flow norms plot
  output$nrmplot <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    #taus <- input$tau
    
    # scale argument
    logspace <- FALSE
    if(input$scl == 'linear') logspace <- TRUE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    ### NEED TO DO ####
    
    # create plot
    ## wrtds
    
    dat=dataNiceNoLag[[dat()]]
    flowPlotNorm_SAS(dat,modelsNoLag_NoFlow_default[[mod()]],modelsNoLag_default[[mod()]],xlim=dt_rng,scale=logspace)
    
    
  }, height = 250, width = 1200)
  
  
})