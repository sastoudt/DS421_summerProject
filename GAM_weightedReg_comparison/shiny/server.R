source("setup_code/flowNormalized_tryAgain.R")
source("nestedPlot.R")
source("spatialResultsPlot.R")
source("comparePlot.R")
load("data/delt_dat.RData")
#load("data/delt_map.RData")

## need to change xlabel to something more informative
dynagam <- function(mod_in, dat_in, grd = 30, years = NULL, alpha = 1,
                    size = 1, col_vec = NULL, allflo = FALSE, month = c(1:12), scales = NULL, ncol = NULL, 
                    pretty = TRUE, grids = TRUE, use_bw = TRUE, fac_nms = NULL,transform=F){
  ylabel=with(lablk, lngs[shrt == dat_in$resdup[1]])
  # add year, month columns to da111t_in
  dat_in <- mutate(dat_in, 
                   month = as.numeric(strftime(date, '%m')), 
                   year = as.numeric(strftime(date, '%Y'))
  )
  to_plo <- dat_in
  
  # flo values to predict
  flo_vals <- range(to_plo[, 'flo'], na.rm = TRUE)
  flo_vals <- seq(flo_vals[1], flo_vals[2], length = grd)
  
  # get model predictions across range of flow values
  dynadat <- rep(flo_vals, each = nrow(to_plo)) %>% 
    matrix(., nrow = nrow(to_plo), ncol = grd) %>% 
    cbind(to_plo[, c('dec_time', 'doy')], .) %>%
    gather('split', 'flo', -dec_time, -doy) %>% 
    select(-split) %>% 
    data.frame(., res = predict(mod_in, .))
  
  if(transform){
    dynadat$res=exp(dynadat$res)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
  }
  dynadat<-dynadat %>% 
    spread(flo, res) %>% 
    select(-dec_time, -doy)

  
  
  # merge predictions with year, month data, make long format
  to_plo <- select(to_plo, year, month) %>% 
    cbind(., dynadat) %>% 
    gather('flo', 'res', -year, -month) %>% 
    mutate(flo = as.numeric(as.character(flo)))
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
    
  }
  
  # constrain plots to salinity limits for the selected month
  if(!allflo){
    
    #min, max salinity values to plot
    lim_vals <- group_by(data.frame(dat_in), month) %>% 
      summarise(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
    
    # month flo ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]
    
    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    # reduce data
    sel_vec <- with(to_plo, 
                    flo >= Low &
                      flo <= High
    )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- group_by(to_plo, year, month, flo) %>% 
    summarise(
      res = mean(res, na.rm = TRUE)
    )
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # reassign facet names if fac_nms is provided
  if(!is.null(fac_nms)){
    
    if(length(fac_nms) != length(unique(to_plo$month))) stop('fac_nms must have same lengths as months')
    
    to_plo$month <- factor(to_plo$month, labels = fac_nms)
    
  }
  
  
  # make plot
  p <- ggplot(to_plo, aes(x = flo, y = res, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)+
    ggtitle("Changes in Relationship Between Response and Flow Across Time \n (Using Model With Flow)")+
    ylab(ylabel) 
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # colors, uses gradcols from WRTDStidal
  cols <- gradcols(col_vec = col_vec)
  
  # use bw theme
  if(use_bw) p <- p + theme_bw()
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
    theme(      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
  
}



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
    #data1=annual_agg(forAgg3) ## doesn't help
    #data2=annual_agg(forAgg4) ## doesn't help
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
                          labels = c('darkblue'='With Flow', "orange"='No Flow'),
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
    labels = c("darkblue"='With Flow', "orange"='No Flow'), 
    values =c('darkblue','orange')
    ) + 
  ggtitle(txt)
}

}



flowPlotNorm_SAS=function(data,mod,modNoFlow,xlim=range(data$date),scale=F,annual=F){
  
  ## doesn't make sense to flow normalize if we don't put flow in the model
  ## don't really need modNoFlow anymore
  
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  # if(scale){
  #   data$res=exp(data$res)
  #   mod$fitted.values=exp(mod$fitted.values)
  #   modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
  #   ylabel <- gsub('ln-|log-', '', as.character(ylabel))
  #   ylabel <- as.expression(parse(text = ylabel))
  # 
  # }
  # data=data[!is.na(data$res),]
  # data=data[!is.na(data$flo),]
  # data$month=as.numeric(strftime(data$date, '%m'))
  # data$year=as.numeric(strftime(data$date, '%Y'))
  # monthly<-group_by(data,month)
  # monthFlow<-summarise(monthly,meanFlow=mean(flo,na.rm=T))
  # monthFlow=as.data.frame(monthFlow)
  # names(monthFlow)=c("month","avgFlow")
  # 
  # mergeData=merge(data,monthFlow,by.x="month",by.y="month")
  # 
  #normVal=getFlowNormalized(mod,data,monthFlow$avgFlow)
  normVal=flowNormalized(data,mod)
  
  #normValNoFlow=getFlowNormalized(modNoFlow,data,monthFlow$avgFlow)
  
  #normVal$date=as.Date(paste(normVal$year,normVal$month,"01",sep="-"))
 # normValNoFlow$date=as.Date(paste(normValNoFlow$year,normValNoFlow$month,"01",sep="-"))
  normVal=normVal[order(normVal$Date),]
#  normValNoFlow=normValNoFlow[order(normValNoFlow$date),]
  #print(head(normValNoFlow))
  #print(head(normVal))
 # data2=as.data.frame(cbind.data.frame(normVal$date,normVal$res,normValNoFlow$res,normVal$month,normVal$year))
  #names(data2)=c("date","normVal","normValNoFlow","month","year")
  
  #mergeData2=merge(mergeData,data2,by.x=c("year","month"),by.y=c("year","month"))
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
    #forAgg=mergeData2[,c("date.x","res","normVal")]
    #forAgg2=mergeData2[,c("date.x","res","normValNoFlow")]
    test=read.csv("data/test.csv",stringsAsFactors=F)
   # forAgg=data[,c("date","res")]
    #forAgg2=as.data.frame(normVal)
    #forAgg2=cbind.data.frame(normVal[,4],unname(normVal[,3]))
    #forAgg2[,2]=unname(forAgg2[,2])
    #forAgg2=normVal[,c(3,4)]
    #names(forAgg)=names(forAgg2)=c("date","res")
    ## right input for annual_agg
    #data=annual_agg(forAgg,min_mo=11)
    #normVal=annual_agg(forAgg2,min_mo=11)
   
    if(scale){
      #data1$res=exp(data1$res)
      #data2$res=exp(data2$res)
      #data1$norm=exp(data1$norm)
      #data2$norm=exp(data2$norm)
      
      #data$res=exp(data$res)
      #normVal$res=exp(normVal$res)
      ylabel <- gsub('ln-|log-', '', as.character(ylabel))
      ylabel <- as.expression(parse(text = ylabel))
      test$resD=exp(test$resD)
      test$resA=exp(test$resA)
    }
 
    #data1=data1[order(data1$date),]
    #data2=data2[order(data2$date),]
    txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + s(flo)') 
   test$dateD=as.Date(test$dateD)
   test$dateA=as.Date(test$dateA)
    ggplot(data=test,aes_string(x="dateD",y="resD"))+geom_point()+
      geom_line(aes_string(x="dateA",y="resA"))+ xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      ggtitle(txt)
    
    # ggplot(data=data,aes_string(x="date",y="res"))+geom_point()+
    #   geom_line(data=normVal, aes_string(x="date",y="res"))+ xlim(xlim)+
    #   xlab("")+
    #   ylab(ylabel)+
    #   ggtitle(txt)
    
    #plot(data$date,data$res,pch=19)
    #lines(normVal$date,normVal$res)
    
    
    # ggplot(data1, aes(x = date, y = res))+geom_point()+
    #   geom_line(aes(y = norm, color = 'With Flow'),lwd=1)+
    #   geom_line(data=data2,aes(y = norm, color = 'No Flow'),lwd=1)+
    #   xlim(xlim)+
    #   xlab("")+
    #   ylab(ylabel)+
    #   scale_colour_manual(name = '',
    #                       labels = c('With Flow', 'No Flow'),
    #                       values =c('darkblue','orange')
    #   ) +
    #   ggtitle(txt)
    
  }

else{
                  
  # title
  txt <- paste0("Flow Normalized: ",titleLab, ' ~ s(time) + s(season) + s(flo)') 
  
  #data=merge(data,normalGrid,by.x="month",by.y="month")
  
  #data=data[order(data$date),]
 # data=mergeData2[order(mergeData2$date.x),]
  
  if(scale){
    data$res=exp(data$res)
    normVal$meanPred=exp(normVal$meanPred)
    #data$normValNoFlow=exp(data$normValNoFlow)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  
  ggplot(data,aes(x=date,y=res))+geom_point()+
    geom_line(data=  normVal, aes(x=Date,y=meanPred))+ xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
    ggtitle(txt)
  
  
  # ggplot(data, aes(x = date.x, y = res))+geom_point()+
  #   geom_line(aes(y = normVal, color = 'With Flow'),lwd=1)+
  #   geom_line(aes(y = normValNoFlow, color = 'No Flow'),lwd=1)+
  #   xlim(xlim)+
  #   xlab("")+
  #   ylab(ylabel)+
  # scale_colour_manual(name = '',
  #   labels = c('With Flow', 'No Flow'),
  #   values =c('darkblue','orange')
  #   ) +
  # ggtitle(txt)
  
}

}

# packages to use
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

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
#print(out)
    
    return(out)
    
  })
  
  dat <- reactive({
    
    stat <- input$stat
    res <- input$res
    #scl <- input$scl
    
    out <- which(names(dataNiceNoLag)==paste(stat,res,sep="_"))
    #print(out)
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
   
    
    # create plot
    ## wrtds
    
    dat=dataNiceNoLag[[dat()]]
    flowPlotNorm_SAS(dat,modelsNoLag_NoFlow_Nested[[mod()]],modelsNoLag_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotNoFlow <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    #taus <- input$tau
    
    # scale argument
    logspace <- FALSE
    if(input$scl == 'linear') logspace <- TRUE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
   
    
    # create plot
    ## wrtds
    
    dat=dataNiceNoLag[[dat()]]
    nestedPlotNoFlow(dat,modelsNoLag_NoFlow_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
    
    
  }, height = 250, width = 1200)
  
  output$nestedPlotFlow <- renderPlot({
    
    # inputs
    
    dt_rng <- input$dt_rng
    #taus <- input$tau
    
    # scale argument
    logspace <- FALSE
    if(input$scl == 'linear') logspace <- TRUE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'observed') annuals <- FALSE
    
    
    # create plot
    ## wrtds
    
    dat=dataNiceNoLag[[dat()]]
    nestedPlotFlow(dat,modelsNoLag_Nested[[mod()]],xlim=dt_rng,scale=logspace,annual=annuals)
    
    
  }, height = 250, width = 1200)
  
  output$comparePlot <- renderPlot({
    
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
    
    
    dat=dataNiceNoLag[[dat()]]
    
    # create plot
   comparePlot(dat,xlim=dt_rng,scale=logspace,annual=annuals)
    
  }, height = 250, width = 1200)
  
  
  output$dynagamP <- renderPlot({
    
    # inputs
    
    # dt_rng <- input$dt_rng
    # #taus <- input$tau
    # 
    # scale argument
    logspace <- FALSE
    if(input$scl == 'linear') logspace <- TRUE
    # 
    # # aggregation period
    # annuals <- TRUE
    # if(input$annuals == 'observed') annuals <- FALSE
    # 
    # 
    # create plot
    ## wrtds
    
    dat=dataNiceNoLag[[dat()]]

    dynagam(modelsNoLag_Nested[[mod()]],dat,transform=logspace)
    #dynagam(gamtmp, tmp)

    
  }, height = 800, width = 1200
  )
  
  output$spatialPlotRMSEG <- renderPlot({
    replayPlot(spatPlot()$plotG)
  }, height = 800, width = 1200
  )
  
  output$spatialPlotRMSEW <- renderPlot({
    replayPlot(spatPlot()$plotW)
  }, height = 800, width = 1200
  )
  spatPlot <- reactive({
    
    # inputs
    
    # dt_rng <- input$dt_rng
    # #taus <- input$tau
    # 
    # scale argument
    logspace <- FALSE
    #if(input$scl == 'linear') logspace <- TRUE
    # 
    # # aggregation period
    # annuals <- TRUE
    # if(input$annuals == 'observed') annuals <- FALSE
    # 
    # 
    # create plot
    ## wrtds
 
    locationLookUp=as.data.frame(cbind(unique(delt_dat$Site_Code),
                                       unique(delt_dat$Latitude),
                                       unique(delt_dat$Longitude)))
    names(locationLookUp)=c("site","lat","long")
    
    

    dev.control("enable")
    makeSpatialPlotG("getSummaryRMSE",dataNiceNoLag,modelsNoLag_Nested,input$res,locationLookUp)
    spatPlotG=recordPlot()
    dev.off()
    
   makeSpatialPlotW("getSummaryRMSE",dataNiceNoLag,modelsNoLag_Nested,input$res,locationLookUp)
   spatPlotW=recordPlot()
   dev.off()
   return(list(plotG=spatPlotG,plotW=spatPlotW))
   # return(list(plotG=spatPlotG))
  })
    
})