### investigate P8 annual flow-normalized
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")
load("dataNice_nolag.RData")
grep("P8",names(dataNiceNoLag)) ## 7, 8, 9

load("modelsNoLag_NoFlow_Nested.RData") ##modelsNoLag_NoFlow_Nested
load("modelsNoLag_Nested.RData") ##modelsNoLag_Nested

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

source("/Users/Sara/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/getFlowNormalized.R")

data=dataNiceNoLag[[7]]
mod=modelsNoLag_Nested[[7]]
modNoFlow=modelsNoLag_NoFlow_Nested[[7]]

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
  
  countTest=summarise(monthly,count=n())
  View(countTest)
  yearly<-group_by(data,year)
  countTest2=summarise(yearly,count=n())
  View(countTest2)
  ## not a missingness issue
  
  monthFlow<-summarise(monthly,meanFlow=mean(flo,na.rm=T))
  monthFlow=as.data.frame(monthFlow)
  names(monthFlow)=c("month","avgFlow")
  
  mergeData=merge(data,monthFlow,by.x="month",by.y="month")
  
  normVal=getFlowNormalized(mod,data,monthFlow$avgFlow)
  normValNoFlow=getFlowNormalized(modNoFlow,data,monthFlow$avgFlow)
  
  checkSub=subset(normVal,year>1990 & year<2010)
  checkSub2=subset(normValNoFlow,year>1990 & year<2010)
  
  cbind(checkSub$year,checkSub$res,checkSub2$res) ## Yup, color coded right
  
  
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
  annual=T
  scale=T
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
      #xlim(xlim)+
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

## ok, getFlowNormalized matches
## https://raw.githubusercontent.com/fawda123/sf_trends/master/R/funcs.R
## dynagam
## maybe model is not fitted appropriately?
dat=dataNiceNoLag[[7]]
flowPlotNorm_SAS(dat,modelsNoLag_NoFlow_Nested[[7]],modelsNoLag_Nested[[7]],annual=T)


### check models
gam.check(modelsNoLag_NoFlow_Nested[[7]])
gam.check(modelsNoLag_NoFlow_Nested[[8]])
gam.check(modelsNoLag_NoFlow_Nested[[9]])

attributes(modelsNoLag_NoFlow_Nested[[9]])
plot(modelsNoLag_NoFlow_Nested[[9]]$fitted.values,modelsNoLag_NoFlow_Nested[[9]]$residuals+modelsNoLag_NoFlow_Nested[[9]]$fitted.values)
abline(0,1)

## these look fine

gam.check(modelsNoLag_Nested[[7]])
gam.check(modelsNoLag_Nested[[8]])
gam.check(modelsNoLag_Nested[[9]])

## these also look fine

## doesn't seem to obviously be a model issue
## look at some models that seem to be ok for reference
gam.check(modelsNoLag_Nested[[10]])
gam.check(modelsNoLag_NoFlow_Nested[[10]])


gam.check(modelsNoLag_Nested[[13]])
gam.check(modelsNoLag_NoFlow_Nested[[13]])

## smaller overall residuals, maybe slightly less heavy tails

## I'll try to refit just in case

## i=7,8,9

i=7
setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny")
load("data/mods_nolag.RData")
load("data/dataNice_nolag.RData")



tmp <- mods_nolag$data[[i]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
  ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )
tmp=tmp[!is.na(tmp$res),]
tmp=tmp[!is.na(tmp$flo),]
tmp=tmp[order(tmp$date),]

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)
gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=30)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)
gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=50)+ti(doy,bs="cc",k=30)+ti(dec_time,bs="tp",k=70)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(6,6))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(6,6,6)), data = tmp)
gam.check(gamDEFAULT)
## warning
gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=50)+ti(doy,bs="cc",k=30)+ti(dec_time,bs="tp",k=70)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(6,6))+ti(doy,dec_time,flo,
                                                                 bs=c("cc","tp","tp"),k=c(5,5,5)), data = tmp)
gam.check(gamDEFAULT)
## warning
gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=50)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)
gam.check(gamDEFAULT)
## warning
gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=40)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)
gam.check(gamDEFAULT)
## warning
gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=30)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) ## error

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=25)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) ## error

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(6,6,6))+ti(doy,dec_time,flo,
                                                        bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(6,6,6))+ti(doy,dec_time,flo,
                                                                   bs=c("cc","tp","tp"),k=c(5,5,5)), data = tmp)

gam.check(gamDEFAULT) ## error

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(6,6,6))+ti(doy,dec_time,flo,
                                                                   bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) 

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7))+ti(doy,dec_time,flo,
                                                                   bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) 

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(7,7))+ti(flo,dec_time,bs=c("tp","tp"),k=c(7,7))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7))+ti(doy,dec_time,flo,
                                                                 bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)  ## error

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(7,7))+ti(flo,dec_time,bs=c("tp","tp"),k=c(6,6))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7))+ti(doy,dec_time,flo,
                                                                 bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT)  ## error

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=60)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(7,7))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7))+ti(doy,dec_time,flo,
                                                                 bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) 

gamDEFAULT <- gam(res ~ ti(flo,bs="tp",k=35)+ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=80)+
                    ti(flo,doy,bs=c("tp","cc"),k=c(6,6))+ti(flo,dec_time,bs=c("tp","tp"),k=c(7,7))+
                    ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7))+ti(doy,dec_time,flo,
                                                                 bs=c("cc","tp","tp"),k=c(4,4,4)), data = tmp)

gam.check(gamDEFAULT) ## seems marginally more symmetric but I'll save this and try it


gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=45)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow)
#  gamtmp <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), k = c(5, 8, 5), data = tmp, knots = list(doy = c(1, 366)))

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=55)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow)

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=15)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow)

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=30)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(8,8)), data = tmp)
gam.check(gamDEFAULTnoflow) ## error

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=30)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow) ## error

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow)

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=25)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(7,7)), data = tmp)
gam.check(gamDEFAULTnoflow) ## error

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=80)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(8,8)), data = tmp)
gam.check(gamDEFAULTnoflow)

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=100)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(8,8)), data = tmp)
gam.check(gamDEFAULTnoflow)

gamDEFAULTnoflow <- gam(res ~ ti(doy,bs="cc",k=20)+ti(dec_time,bs="tp",k=100)+
                          ti(doy,dec_time,bs=c("cc","tp"),k=c(9,9)), data = tmp)
gam.check(gamDEFAULTnoflow)  ## this doesn't really seem like an improvement symmetry wise of 
## resids v. linear pred but test it out
modelsNoLag_Nested[[i]]=gamDEFAULT
modelsNoLag_NoFlow_Nested[[i]]=gamDEFAULTnoflow
names(modelsNoLag_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
names(modelsNoLag_NoFlow_Nested)[i]=paste(mods_nolag$Site_Code[[i]],mods_nolag$resvar[[i]],sep="_")
print(i)

####