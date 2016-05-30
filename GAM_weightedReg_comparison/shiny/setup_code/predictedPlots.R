
  
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
    ggplot(data, aes(x = date, y = res))+geom_point()+
      geom_line(aes(y = mod$fitted.values),color='darkblue',lwd=1)+
      geom_line(aes(y = modNoFlow$fitted.values),lwd=1,color="orange")+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)#+
    #scale_colour_manual(name = '', 
     #                   values =c('orange'="No Flow",'darkblue'="Flow"))
  }
  ## need legend
  
  mod=modelsNoLag_default[[1]]
  modNoFlow=modelsNoLag_NoFlow_default[[1]]
  tmp <- mods_nolag$data[[1]] %>% 
    mutate(
      dec_time = dec_time(Date)[['dec_time']],
      doy = yday(Date)
    ) %>% 
    rename(
      res = resval, 
      flo = flolag,
      date = Date
    )
  flowPlot_SAS(tmp,mod,modNoFlow,c(as.Date("1990-01-01"),as.Date("2010-01-01")))
  
  
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
    
    data=merge(data,normalGrid,by.x="month",by.y="month")
data=data[order(data$date),]
    ggplot(data, aes(x = date, y = res))+geom_point()+
      geom_line(aes(y = normVal),color='darkblue',lwd=1)+
      geom_line(aes(y = normValNoFlow),lwd=1,color="orange")+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)#+
    #scale_colour_manual(name = '', 
    #                   values =c('orange'="No Flow",'darkblue'="Flow"))
  }
  
  flowPlotNorm_SAS(tmp,mod,modNoFlow)
  