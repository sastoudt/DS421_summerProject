
forMap=allData[,c("Longitude","Latitude","Station")]
forMap=unique(forMap)

statNam=unlist(lapply(names(mod1$coefficients)[1:15],function(x){y=strsplit(x,"Station)");unlist(y)[2]}))
statNam[1]="C10"

intercept1=as.data.frame(cbind(statNam,mod1$coefficients[1:15]))
row.names(intercept1)<-NULL
names(intercept1)[2]="intercept1"

intercept1=as.data.frame(cbind(statNam,mod1$coefficients[1:15]))
row.names(intercept1)<-NULL
names(intercept1)[2]="intercept1"

intercept1[,2]=as.numeric(as.character(intercept1[,2]))

intercepts=as.data.frame(cbind(statNam,mod1$coefficients[1:15],mod2$coefficients[1:15],mod3$coefficients[1:15],
                               mod4$coefficients[1:15]))

row.names(intercepts)=NULL
names(intercepts)[2:5]=c("int1","int2","int3","int4")
intercepts$int1=as.numeric(as.character(intercepts$int1))
intercepts$int2=as.numeric(as.character(intercepts$int2))
intercepts$int3=as.numeric(as.character(intercepts$int3))
intercepts$int4=as.numeric(as.character(intercepts$int4))

testMerge=merge(forMap,intercepts,by.x="Station",by.y="statNam")

require(gridExtra)

g1<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 Intercepts by Station")+scale_size(guide=F)

g2<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 Intercepts by Station")+scale_size(guide=F)

g3<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 Intercepts by Station")+scale_size(guide=F)

g4<-ggplot(testMerge,aes(x = Longitude, y = Latitude,colour=int4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 Intercepts by Station")+scale_size(guide=F)
  
grid.arrange(g1,g2,g3,g4) ## can't see what is going on

which(testMerge$Latitude<37.8)

testMerge2=testMerge[-1,]

g1<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int1,cex=2))+geom_point()+
  ggtitle("Spatial Model 1 Intercepts by Station")+scale_size(guide=F)

g2<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int2,cex=2))+geom_point()+
  ggtitle("Spatial Model 2 Intercepts by Station")+scale_size(guide=F)

g3<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int3,cex=2))+geom_point()+
  ggtitle("Spatial Model 3 Intercepts by Station")+scale_size(guide=F)

g4<-ggplot(testMerge2,aes(x = Longitude, y = Latitude,colour=int4,cex=2))+geom_point()+
  ggtitle("Spatial Model 4 Intercepts by Station")+scale_size(guide=F)

grid.arrange(g1,g2,g3,g4)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

## deviance explained goes down as you get more complicated
mod1
mod2
mod3
mod4

## REML scores go up which makes sense, adding more complexity to each model

### RMSE per station plot
