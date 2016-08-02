## set up modelling framework
## start by per station
## see if GAM approach works constant variance wise
## might need to switch to log transform and see if that is better

setwd("~/Desktop/sfei")

load(file="perStation.Rda")

require(mgcv)
gamDEFAULT<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(sio2,bs="tp"),data=perStation[[1]],family=gaussian(link="log"))
gam.check(gamDEFAULT)

## not allowing enough degrees of freedom, increase
gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(sio2,bs="tp",k=10),data=perStation[[1]],family=gaussian(link="log"))
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(sio2,bs="tp",k=15),data=perStation[[1]],family=gaussian(link="log"))
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=15)+ti(date_dec,bs="tp",k=20)+ti(sio2,bs="tp",k=18),data=perStation[[1]],family=gaussian(link="log"))
gam.check(gamDEFAULT)

plot(gamDEFAULT$fitted.values,gamDEFAULT$residuals)
plot(gamDEFAULT$fitted.values,gamDEFAULT$residuals,ylim=c(-10,20))
## constant variance looks good except not good when it predicts close to zero values

## still not allowing large enough spaces, hopefully this will get better as we add more covariates
## pushing up against highest limit allowed for each k

## qqplot is not great, but it was worse without the transform

## Let's try adding temperature to get a better sense of how extra covariates help/hurt.
toUse=na.omit(perStation[[1]][,c("doy","date_dec","sio2","temp","chl")])
gamDEFAULT<-gam(chl~ti(doy,bs="cc")+ti(date_dec,bs="tp")+ti(sio2,bs="tp")+ti(temp,bs="tp"),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=10)+ti(sio2,bs="tp",k=10)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=15)+ti(sio2,bs="tp",k=15)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(sio2,bs="tp",k=15)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=20)+ti(sio2,bs="tp",k=18)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=30)+ti(sio2,bs="tp",k=18)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

gamDEFAULT<-gam(chl~ti(doy,bs="cc",k=10)+ti(date_dec,bs="tp",k=40)+ti(sio2,bs="tp",k=18)+ti(temp,bs="tp",k=10),family=gaussian(link="log"),data=toUse)
gam.check(gamDEFAULT)

plot(gamDEFAULT$fitted.values,gamDEFAULT$residuals)
plot(gamDEFAULT$fitted.values,gamDEFAULT$residuals,ylim=c(-20,20))



plot(toUse$doy,gamDEFAULT$residuals)
plot(toUse$doy,gamDEFAULT$residuals,ylim=c(-20,20)) ## more in 100-300 spring/summer

plot(toUse$date_dec,gamDEFAULT$residuals) 
plot(toUse$date_dec,gamDEFAULT$residuals,ylim=c(-20,20)) 

plot(toUse$sio2,gamDEFAULT$residuals)
plot(toUse$temp,gamDEFAULT$residuals) 
plot(gamDEFAULT$residuals) 
## zoom in to make sure we aren't missing anything
plot(gamDEFAULT$residuals,ylim=c(-50,50)) ## not too bad

vis.gam(gamDEFAULT)

vis.gam(gamDEFAULT,view=c("doy","date_dec"),plot.type="contour") ## spring/summer not useful
vis.gam(gamDEFAULT,view=c("doy","temp"),plot.type="contour")  ## spring/summer not useful
vis.gam(gamDEFAULT,view=c("doy","sio2"),plot.type="contour")  ## spring/summer not useful
vis.gam(gamDEFAULT,view=c("sio2","temp"),plot.type="contour") ## hm...

###
data=na.omit(perStation[[1]][,c("Date","doy","date_dec","sio2","temp","chl")])

terms<-c("ti(doy)","ti(date_dec)","ti(temp)",
         "ti(sio2)",'intercept')
  
  intercept=summary(gamDEFAULT)$p.coeff
  
  
  all3=gamDEFAULT$fitted.values
  tryThis=predict(gamDEFAULT,data,type="terms")
  tryThis2=apply(tryThis,1,sum)+rep(intercept,length(all3))
  head(exp(tryThis2))
  head(all3) ## these don't match?
  
head(apply(tryThis,1,sum))-head(doy+date_dec+temp+sio2)
 head(tryThis2)-head(intercept+doy+date_dec+temp+sio2)
 tryThisCompare=intercept+doy+date_dec+temp+sio2
 
 
 plot(density(tryThis2-tryThisCompare))
 
 plot(density(exp(tryThis2)-all3)) ## both marginally different from fitted.values
 plot(density(exp(tryThisCompare)-all3)) 
 
 max(abs(exp(tryThis2)-all3))
 max(abs(exp(tryThisCompare)-all3)) ## this one a little bit further off
 
 ## use "terms" instead of exclude
 
  nestPred=as.data.frame(cbind.data.frame(tryThis,intercept,data$Date,tryThis2))
  names(nestPred)=c("doy","date_dec","sio2","temp","intercept","date","all")
  
  
  nestPred$date=as.Date(nestPred$date)

  
  
  require(visreg)
  visreg(gamDEFAULT,scale="response")
  
  require(ggplot2)
  
  ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
    geom_line(data=nestPred,aes(x=date,y = temp, color = 'ti(temp)'),lwd=1)+
    geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = sio2, color = 'ti(sio2)'), lwd=1)
  ## leave out intercept and doy, because makes scale weird
  
  ## means aren't matching up
  ## I thought this might have to do with the bias issue of transforming back and forth
  ## But now I think it is the fact that the model predicts values down to zero
  ## true data has a minimum of 0.85 (detection limit or something?)
  
  summary(nestPred$all) ## these means don't match up
  summary(log(data$chl))
  
  summary(gamDEFAULT$fitted.values)
  summary(data$chl)
 
  ##all
  ggplot(data,aes(x = Date, y = log(chl)))+geom_point()+
    geom_line(data=nestPred,aes(x=date,y = temp, color = 'ti(temp)'),lwd=1)+
  geom_line(data=nestPred,aes(x=date,y = doy, color = 'ti(doy)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y=date_dec, color = 'ti(date_dec)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = sio2, color = 'ti(sio2)'), lwd=1)+
    geom_line(data=nestPred,aes(x=date,y = intercept, color = 'intercept'), lwd=1)+
    scale_colour_manual(name = '',
                        labels =c('red'=terms[1],'orange'=terms[2],"dodgerblue"=terms[3],
                                  "forestgreen"=terms[4],"blue"=terms[5]),values=c("red","orange",
                                                                                   "dodgerblue","forestgreen","blue")
    ) +
    ggtitle(names(perStation)[1])
    
    
  ## fitted value plot
  ggplot(data,aes(x = Date, y = chl))+geom_point()+
    geom_line(data=nestPred,aes(x=date,y = exp(all),col="red"),lwd=1)+
  ggtitle(paste(names(perStation)[1], "Fitted Values",sep=" "))+theme(legend.position='none')
    
   