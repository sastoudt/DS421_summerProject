source("getFlowNormalized.R")
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

flowPlot_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F,annual=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  if(scale){
    data$res=exp(data$res)
    mod$fitted.values=exp(mod$fitted.values)
    modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
    
  }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),] ## I think this is what might be causing the weird breaks
  ## removing NAs before they get to be counted in the aggregation
  titleLab=ifelse(scale,data$resdup[1],paste("ln(",data$resdup[1],")",sep=""))
  
  if(annual){
    forAgg3=as.data.frame(cbind.data.frame(data$date,data$res,mod$fitted.values))
    forAgg4=as.data.frame(cbind.data.frame(data$date,data$res,modNoFlow$fitted.values))
    names(forAgg3)=names(forAgg4)=c("date","res","fit")
    data1=annual_agg(forAgg3,min_mo=11)
    data2=annual_agg(forAgg4,min_mo=11)
    data1=data1[order(data1$date),]
    data2=data2[order(data2$date),]

    # title
    txt <- paste0(titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    ggplot(data1, aes(x = date, y = res))+geom_point()+
      geom_line(data=data1,aes(y = fit, color = 'With Flow'),lwd=1)+
      geom_line(data=data2,aes(y = fit, color = 'No Flow'), lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '',
                          labels = c('With Flow', 'No Flow'),
                          values =c('darkblue','orange')
      ) +
      ggtitle(txt)
    
  }else{
  
  
  # title
  txt <- paste0(titleLab, ' ~ s(time) + s(season) + [s(flo)]') 

  ggplot(data, aes(x = date, y = res))+geom_point()+
    geom_line(aes(y = mod$fitted.values, color = 'With Flow'),lwd=1)+
    geom_line(aes(y = modNoFlow$fitted.values, color = 'No Flow'), lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
  scale_colour_manual(name = '', 
    labels = c('With Flow', 'No Flow'), 
    values =c('darkblue','orange')
    ) + 
  ggtitle(txt)
}

}



flowPlotNorm_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F,annual=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  # if(scale){
  #   data$res=exp(data$res)
  #   mod$fitted.values=exp(mod$fitted.values)
  #   modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
  #   ylabel <- gsub('ln-|log-', '', as.character(ylabel))
  #   ylabel <- as.expression(parse(text = ylabel))
  # 
  # }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  data$month=as.numeric(strftime(data$date, '%m'))
  data$year=as.numeric(strftime(data$date, '%Y'))
  monthly<-group_by(data,month)
  monthFlow<-summarise(monthly,meanFlow=mean(flo,na.rm=T))
  monthFlow=as.data.frame(monthFlow)
  names(monthFlow)=c("month","avgFlow")

  mergeData=merge(data,monthFlow,by.x="month",by.y="month")
  
  normVal=getFlowNormalized(mod,data,monthFlow$avgFlow)
  normValNoFlow=getFlowNormalized(modNoFlow,data,monthFlow$avgFlow)
  
  normVal$date=as.Date(paste(normVal$year,normVal$month,"01",sep="-"))
  normValNoFlow$date=as.Date(paste(normValNoFlow$year,normValNoFlow$month,"01",sep="-"))
  normVal=normVal[order(normVal$date),]
  normValNoFlow=normValNoFlow[order(normValNoFlow$date),]
  #print(head(normValNoFlow))
  #print(head(normVal))
  data2=as.data.frame(cbind.data.frame(normVal$date,normVal$res,normValNoFlow$res,normVal$month,normVal$year))
  names(data2)=c("date","normVal","normValNoFlow","month","year")
  
  mergeData2=merge(mergeData,data2,by.x=c("year","month"),by.y=c("year","month"))
  # data$month=as.numeric(unlist(lapply(data$date,function(x){format(x,"%m")})))
  # data$predVal=mod$fitted.values
  # data$predValNoFlow=modNoFlow$fitted.values
  # months=1:12
  # normalGrid=as.data.frame(months)
  # 
  # normalGrid$normVal=rep(NA,nrow(normalGrid))
  # normalGrid$normValNoFlow=rep(NA,nrow(normalGrid))
  # names(normalGrid)[1]=c("month")
  # 
  # for(i in 1:nrow(normalGrid)){
  #   
  #   sub=subset(data,  month==normalGrid[i,1])
  #   
  #   normalGrid$normVal[i]=mean(sub$predVal,na.rm=T)
  #   normalGrid$normValNoFlow[i]=mean(sub$predValNoFlow,na.rm=T)
  #   #print(i)
    
# }
  titleLab=ifelse(scale,data$resdup[1],paste("ln(",data$resdup[1],")",sep=""))
  
  if(annual){
    forAgg=mergeData2[,c("date.x","res","normVal")]
    forAgg2=mergeData2[,c("date.x","res","normValNoFlow")]
    names(forAgg)=names(forAgg2)=c("date","res","norm")
    data1=annual_agg(forAgg,min_mo=11)
    data2=annual_agg(forAgg2,min_mo=11)
    
    if(scale){
      data1$res=exp(data1$res)
      data2$res=exp(data2$res)
      data1$norm=exp(data1$norm)
      data2$norm=exp(data2$norm)
      
      ylabel <- gsub('ln-|log-', '', as.character(ylabel))
      ylabel <- as.expression(parse(text = ylabel))
      
    }
    data1=data1[order(data1$date),]
    data2=data2[order(data2$date),]
    txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    ggplot(data1, aes(x = date, y = res))+geom_point()+
      geom_line(aes(y = norm, color = 'With Flow'),lwd=1)+
      geom_line(data=data2,aes(y = norm, color = 'No Flow'),lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '',
                          labels = c('With Flow', 'No Flow'),
                          values =c('darkblue','orange')
      ) +
      ggtitle(txt)
    
  }

else{
                  
  # title
  txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
  
  #data=merge(data,normalGrid,by.x="month",by.y="month")
  
  #data=data[order(data$date),]
  data=mergeData2[order(mergeData2$date.x),]
  
  if(scale){
    data$res=exp(data$res)
    data$normVal=exp(data$normVal)
    data$normValNoFlow=exp(data$normValNoFlow)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  
  
  ggplot(data, aes(x = date.x, y = res))+geom_point()+
    geom_line(aes(y = normVal, color = 'With Flow'),lwd=1)+
    geom_line(aes(y = normValNoFlow, color = 'No Flow'),lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
  scale_colour_manual(name = '',
    labels = c('With Flow', 'No Flow'),
    values =c('darkblue','orange')
    ) +
  ggtitle(txt)
  
}

}

# packages to use
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)

# raw data
#load(file = 'data/modelsNoLag_default.RData')
#load(file = 'data/modelsNoLag_NoFlow_default.RData')
load(file="data/modelsNoLag_NoFlow_Nested.RData")
load(file="data/modelsNoLag_Nested.RData")
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
    
    out <- which(names(modelsNoLag_Nested)==paste(stat,res,sep="_"))

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
    flowPlot_SAS(dat,modelsNoLag_NoFlow_Nested[[mod()]],modelsNoLag_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
   
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
    flowPlotNorm_SAS(dat,modelsNoLag_NoFlow_Nested[[mod()]],modelsNoLag_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
    
    
  }, height = 250, width = 1200)
  
  
})