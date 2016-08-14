comparePlot=function(data,xlim=range(data$date),scale=F,annual=F){
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])
  if(scale){
    
    data$res=exp(data$res)
    data$gamPred=exp(data$gamPred)
    data$wrtdsPred=exp(data$wrtdsPred)
    #mod$fitted.values=exp(mod$fitted.values)
    #modNoFlow$fitted.values=exp(modNoFlow$fitted.values)
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
    
  }
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),] ## I think this is what might be causing the weird breaks
  ## removing NAs before they get to be counted in the aggregation
  titleLab=ifelse(scale,data$resdup[1],paste("ln(",data$resdup[1],")",sep=""))
  
  if(annual){
    forAgg3=as.data.frame(cbind.data.frame(data$date,data$res,data$gamPred))
    forAgg4=as.data.frame(cbind.data.frame(data$date,data$res,data$wrtdsPred))
    names(forAgg3)=names(forAgg4)=c("date","res","fit")
    data1=annual_agg(forAgg3,min_mo=11)
    data2=annual_agg(forAgg4,min_mo=11)
    data1=data1[order(data1$date),]
    data2=data2[order(data2$date),]
    
    # title
    txt <- paste0(titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    ggplot(data1, aes(x = date, y = res))+geom_point()+
      geom_line(data=data1,aes(y = fit, color = 'GAM'),lwd=1)+
      geom_line(data=data2,aes(y = fit, color = 'WRTDS'), lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '',
                          labels = c('darkblue'='GAM', "orange"='WRTDS'),
                          values =c('darkblue','orange')
      ) +
      ggtitle(txt)
    
  }else{
    
    
    # title
    txt <- paste0(titleLab, ' ~ s(time) + s(season) + [s(flo)]') 
    
    ggplot(data, aes(x = date, y = res))+geom_point()+
      geom_line(aes(y = gamPred, color = 'GAM'),lwd=1)+
      geom_line(aes(y = wrtdsPred, color = 'WRTDS'), lwd=1)+
      xlim(xlim)+
      xlab("")+
      ylab(ylabel)+
      scale_colour_manual(name = '', 
                          labels = c("darkblue"="GAM", "orange"="WRTDS"), 
                          values =c('darkblue','orange')
      ) + 
      ggtitle(txt)
  }
  # scale_colour_manual(name = '', 
  #                     labels = c('GAM'="darkblue", 'WRTDS'="orange"), 
  #                     values =c('darkblue','orange')
  # )
  
}