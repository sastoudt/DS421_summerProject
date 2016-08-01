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
nestedPlotFlow<-function(data,mod,xlim=range(data$date),scale=F,annual=F){
  terms<-c("ti(flo)","ti(doy)","ti(dec_time)",
           "ti(flo,doy)","ti(flo,dec_time)","ti(doy,dec_time)",
           "ti(doy,dec_time,flo)",'intercept')
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])

  
  
  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  intercept=summary(mod)$p.coeff
  flo=predict(mod,data,exclude=terms[-1])-intercept
  doy=predict(mod,data,exclude=terms[-2])-intercept
  dec_time=predict(mod,data,exclude=terms[-3])-intercept
  flo_doy=predict(mod,data,exclude=terms[-4])-intercept
  flo_dec_time=predict(mod,data,exclude=terms[-5])-intercept
  doy_dec_time=predict(mod,data,exclude=terms[-6])-intercept
  all=predict(mod,data,exclude=terms[-7])-intercept
  intercept=rep(intercept,nrow(data))
  
  test=intercept+flo+doy+dec_time+flo_doy+flo_dec_time+doy_dec_time
  head(test)
  head(mod$fitted.values)
  nestPred=as.data.frame(cbind.data.frame(intercept,flo,doy,dec_time,flo_doy,flo_dec_time,doy_dec_time,all,data$date))
  names(nestPred)[9]="date"
  
  if(scale){
    data$res=exp(data$res)
    nestPred[,1:8]=apply(nestPred[,1:8],2,function(x){exp(x)})
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
    
  }
  
  
  ggplot(data,aes(x = date, y = res))+geom_point()+
    geom_line(data=nestPred,aes(x=date,y = flo, color = 'ti(flo)'),lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y=dec_time, color = 'ti(dec_time)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = flo_doy, color = 'ti(flo,doy)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = flo_dec_time, color = 'ti(flo,dec_time)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = doy_dec_time, color = 'ti(doy,dec_time)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = all, color = 'ti(doy,dec_time,flo)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
    scale_colour_manual(name = '',
                        labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                  "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
                                  "magenta"=terms[7],"grey"=terms[8]),values=c("red","orange",
                                  "dodgerblue","forestgreen","blue","purple","magenta","grey")
    ) +
  ggtitle("Decomposed Model Predictions: With Flow")
  
}


nestedPlotNoFlow<-function(data,mod,xlim=range(data$date),scale=F,annual=F){
  terms<-c("ti(doy)","ti(dec_time)",
           "ti(doy,dec_time)")
  ylabel=with(lablk, lngs[shrt == data$resdup[1]])

  data=data[!is.na(data$res),]
  data=data[!is.na(data$flo),]
  intercept=summary(mod)$p.coeff
  doy=predict(mod,data,exclude=terms[-1])-intercept
  dec_time=predict(mod,data,exclude=terms[-2])-intercept
  
  doy_dec_time=predict(mod,data,exclude=terms[-3])-intercept
  
  intercept=rep(intercept,nrow(data))
  
  nestPred=as.data.frame(cbind.data.frame(intercept,doy,dec_time,doy_dec_time,data$date))
  names(nestPred)[5]="date"
  
  if(scale){
    data$res=exp(data$res)
    nestPred[,1:4]=apply(nestPred[,1:4],2,function(x){exp(x)})
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
    
  }
  
  
  ggplot(data,aes(x = date, y = res))+geom_point()+
    
    geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y=dec_time, color = 'ti(dec_time)'), lwd=1)+
    
    geom_line(data=nestPred,aes(x=date,y = doy_dec_time, color = 'ti(doy,dec_time)'), lwd=1)+
    
    geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
    xlim(xlim)+
    xlab("")+
    ylab(ylabel)+
    scale_colour_manual(name = '',
                        labels =c('red'=terms[1],'orange'=terms[2],"blue"=terms[3],
                                  "forestgreen"=terms[4]),values=c("red","orange",
                                            "blue","forestgreen")
    ) +
    ggtitle("Decomposed Model Predictions: No Flow")
  
}

#### SCRATCH ######
# terms<-c("ti(flo)","ti(doy)","ti(dec_time)",
#          "ti(flo,doy)","ti(flo,dec_time)","ti(doy,dec_time)",
#          "ti(doy,dec_time,flo)")
# load("data/modelsNoLag_Nested.RData")
# intercept=summary(modelsNoLag_Nested[[22]])$p.coeff
# flo=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-1])-intercept
# doy=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-2])-intercept
# dec_time=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-3])-intercept
# flo_doy=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-4])-intercept
# flo_dec_time=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-5])-intercept
# doy_dec_time=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-6])-intercept
# all=predict(modelsNoLag_Nested[[22]],tmp,exclude=terms[-7])-intercept
# test2=predict(modelsNoLag_Nested[[22]],tmp)
# head((flo+doy+dec_time+flo_doy+flo_dec_time+doy_dec_time+all+summary(modelsNoLag_Nested[[22]])$p.coeff)-test2)
# summary(flo+doy+dec_time+flo_doy+flo_dec_time+doy_dec_time+all+summary(modelsNoLag_Nested[[1]])$p.coeff-test2)
# summary(modelsNoLag_Nested[[1]])
# intercept=rep(summary(modelsNoLag_Nested[[1]])$p.coeff,nrow(tmp))
# 
# nestPred=as.data.frame(cbind.data.frame(intercept,flo,doy,dec_time,flo_doy,flo_dec_time,doy_dec_time,all,tmp$date))
# names(nestPred)[9]="date"
# #txt <- paste0(titleLab, ' ~ s(time) + s(season) + [s(flo)]')
# #tmp=tmp[!is.na(tmp$res),]
# #tmp=tmp[!is.na(tmp$flo),]
# nrow(tmp)
# ggplot(tmp,aes(x = date, y = res))+geom_point()+
#   geom_line(data=nestPred,aes(x=date,y = flo, color = 'ti(flo)'),lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y=dec_time, color = 'ti(dec_time)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = flo_doy, color = 'ti(flo,doy)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = flo_dec_time, color = 'ti(flo,dec_time)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = doy_dec_time, color = 'ti(doy,dec_time)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = all, color = 'ti(doy,dec_time,flo)'), lwd=1)+
#   geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
#   #xlim(xlim)+
#   xlab("")+
#   #ylab(ylabel)+
#   scale_colour_manual(name = '',
#                       #labels = terms,
#                       labels =c('red'="ti(flo)",'orange'="ti(doy)","dodgerblue"=ti(dec_time),
#                                 "forestgreen"=terms[4],"blue"=terms[5],"purple"=terms[6],
#                                 "magenta"=terms[7],"grey"=terms[8]),
#                       values=c("red","orange", "dodgerblue","forestgreen","blue","purple","magenta","grey")
#   ) #+
  #ggtitle(txt)
# 
# 
