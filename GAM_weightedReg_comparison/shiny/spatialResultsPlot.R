require(sp)
load("data/delt_map.RData")
#load("sf_bay.RData")
#plot(sf_bay)
#plot(delt_map,add=T)
load("data/delt_dat.RData")
#head(delt_dat)

#points(delt_dat$Longitude,delt_dat$Latitude,pch=19,col="red")

source("getSummaryRMSE_Dev.R")
source("getFlowNormalizedSummary.R")
source("getSummaryDifference.R")
source("getRegressionResults.R")

#load("data/dataNice_nolag.RData")
#load("data/modelsNoLag_Nested.RData")

## unique preserves order
locationLookUp=as.data.frame(cbind(unique(delt_dat$Site_Code),
                                   unique(delt_dat$Latitude),
                                   unique(delt_dat$Longitude)))
names(locationLookUp)=c("site","lat","long")

makeSpatialPlotG<-function(fn,data,mod,responseVar,locationLookup){
  rbPal<-colorRampPalette(c("red","blue"))
  processing=preProcessSpatialPlot(fn,data,mod,responseVar,locationLookup)
  coordsRef=processing$coordsRef
  ofInterest=processing$ofInterestG
  col=processing$colG
  cuts=processing$cutsG
  #pdf(paste("spatialDistn","_",responseVar,"_",fn,".pdf",sep=""),height=12,width=16)
  par(mfrow=c(3,5),mar=c(rep(2,4))) ## fill by row, extra with frame()
  counter=1
  counter2=1
  while(counter<14){
    if(counter %in% 1:5){
      #plot(delt_map,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))## title is tight on top row
      plot(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))
      
      #points(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2)
      legend(legend=round(cuts[2:5,counter],2),fill=rbPal(4),"bottomleft")
      counter=counter+1
      counter2=counter2+1
    }else if(counter2 %in% c(10,15)){
      frame()
      if(counter2==10){
        legend("center",legend=paste(responseVar,fn,sep="_"))
      }
      counter2=counter2+1
      #counter=counter+1
    }else{ ## need to do \n everywhere, need to make a counter, skip over 10 and 15
      #plot(delt_map,main=paste("\n",row.names(ofInterest)[counter+1],sep=""))
      #points(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2)
      plot(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))
      
      legend(legend=round(cuts[2:5,counter],2),fill=rbPal(4),"bottomleft")
      counter=counter+1
      counter2=counter2+1
    }
  }
  #dev.off()

}

makeSpatialPlotW<-function(fn,data,mod,responseVar,locationLookup){
  rbPal<-colorRampPalette(c("red","blue"))
  processing=preProcessSpatialPlot(fn,data,mod,responseVar,locationLookup)
  #processing=preProcessSpatialPlot("getSummaryRMSE",dataNiceNoLag,rep(1,length(dataNiceNoLag)),"din",locationLookup)
  
  coordsRef=processing$coordsRef
  ofInterest=processing$ofInterestW
  col=processing$colW
  cuts=processing$cutsW
  #pdf(paste("spatialDistn","_",responseVar,"_",fn,".pdf",sep=""),height=12,width=16)
  #pdf("test.pdf",height=10,width=10)
  par(mfrow=c(3,5),mar=c(rep(2,4))) ## fill by row, extra with frame()
  counter=1
  counter2=1
  while(counter<14){
    if(counter %in% 1:5){
     # plot(delt_map,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))## title is tight on top row
    #  points(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2)
      plot(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))
      
      legend(legend=round(cuts[2:5,counter],2),fill=rbPal(4),"bottomleft",cex=.75,bty="n")
      counter=counter+1
      counter2=counter2+1
    }else if(counter2 %in% c(10,15)){
      frame()
      if(counter2==10){
        legend("center",legend=paste(responseVar,fn,sep="_"))
      }
      counter2=counter2+1
      #counter=counter+1
    }else{ ## need to do \n everywhere, need to make a counter, skip over 10 and 15
      #plot(delt_map,main=paste("\n",row.names(ofInterest)[counter+1],sep=""))
      #points(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2)
      plot(coordsRef$lon,coordsRef$lat,col=col[,counter+1],pch=19,cex=2,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))
      
      legend(legend=round(cuts[2:5,counter],2),fill=rbPal(4),"bottomleft",cex=.75,bty="n")
      counter=counter+1
      counter2=counter2+1
    }
  }
  #dev.off()
  
}


preProcessSpatialPlot=function(fn,data,mod,responseVar,locationLookup){
  ## don't really need mod now
  perStation=mapply(fn,data,mod,SIMPLIFY=F)
  rbPal<-colorRampPalette(c("red","blue"))
  station=unlist(lapply(names(perStation),function(x){strsplit(x,"_")[[1]][1]}))
  response=unlist(lapply(names(perStation),function(x){strsplit(x,"_")[[1]][2]}))
  
  ofInterest=do.call(cbind,perStation[which(response==responseVar)[order(station[which(response==responseVar)])]])

  ofInterestG=ofInterest[,seq(1,ncol(ofInterest),by=2)]
  ofInterestW=ofInterest[,seq(2,ncol(ofInterest),by=2)]
  
  cutsG=apply(ofInterestG,1,function(x){quantile(x,c(0,0.25,0.5,0.75,1),na.rm=T)})
  cutsW=apply(ofInterestW,1,function(x){quantile(x,c(0,0.25,0.5,0.75,1),na.rm=T)})
  
  #cuts=quantile(ofInterest,c(0,0.25,0.5,0.75,1))

  
  colG=c()
  colW=c()
  for(i in 1:nrow(ofInterestG)){
    colG=cbind(colG,rbPal(4)[as.numeric(cut(ofInterestG[i,],breaks=cutsG[,i],include.lowest=T))])
    colW=cbind(colW,rbPal(4)[as.numeric(cut(ofInterestW[i,],breaks=unique(cutsW[,i]),include.lowest=T))])
    
  }
  

  ## now need to associate columns with coordinates
  ourStations=as.data.frame(unique(station))
  names(ourStations)="site"
  
  coordsRef=merge(ourStations,locationLookUp)
  coordsRef$lat=as.numeric(as.character(coordsRef$lat))
  coordsRef$lon=as.numeric(as.character(coordsRef$lon))
  
  return(list(ofInterestG=ofInterestG,ofInterestW=ofInterestW,cutsG=cutsG,cutsW=cutsW,colG=colG,colW=colW,coordsRef=coordsRef))
  
}


## makeSpatialPlot("getSummaryRMSE",dataNiceNoLag,modelsNoLag_Nested,"no23",locationLookUp)


#### SCRATCH #####

# rmsePerStation=mapply(getSummaryRMSE,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
# devPerStation=mapply(getSummaryDeviance,dataNiceNoLag,modelsNoLag_Nested,SIMPLIFY=F)
# 
# 
# rbPal<-colorRampPalette(c("red","blue"))
# 
# names(rmsePerStation)[1]
# rmsePerStation[[1]]
# 
# station=unlist(lapply(names(rmsePerStation),function(x){strsplit(x,"_")[[1]][1]}))
# response=unlist(lapply(names(rmsePerStation),function(x){strsplit(x,"_")[[1]][2]}))
# 
# ofInterest=do.call(cbind,rmsePerStation[which(response=="no23")[order(station[which(response=="no23")])]])
# ## cbind these results
# ## quantile over whole matrix
# ## ordered appropriately
# 
# ## check order
# station[which(response=="no23")[order(station[which(response=="no23")])]]
# 
# 
# cuts=quantile(ofInterest,c(0,0.25,0.5,0.75,1))
# 
# col=apply(ofInterest,c(1,2),function(x){rbPal(4)[as.numeric(cut(x,breaks=cuts))]})
# ## makes minimum NA
# col[which(is.na(col))]=rbPal(4)[1]
# 
# ## now need to associate columns with coordinates
# ourStations=as.data.frame(unique(station))
# names(ourStations)="site"
# head(delt_dat)
# 
# ## unique preserves order
# locationLookUp=as.data.frame(cbind(unique(delt_dat$Site_Code),
#                                    unique(delt_dat$Latitude),
#                                    unique(delt_dat$Longitude)))
# names(locationLookUp)=c("site","lat","long")
# locationLookUp
# 
# coordsRef=merge(ourStations,locationLookUp)
# coordsRef$lat=as.numeric(as.character(coordsRef$lat))
# coordsRef$lon=as.numeric(as.character(coordsRef$lon))
# 
# ## Figure 2 type plot Beck_and_Murphy_EMA with RMSE info
# pdf("spatialDistnRMSE.pdf",height=12,width=16)
# par(mfrow=c(3,5),mar=c(rep(2,4))) ## fill by row, extra with frame()
# counter=1
# counter2=1
# while(counter<14){
#   if(counter %in% 1:5){
#     plot(delt_map,main=paste("\n ",row.names(ofInterest)[counter+1],sep=""))## title is tight on top row
#     points(coordsRef$lon,coordsRef$lat,col=col[counter+1,],pch=19,cex=2)
#     legend(legend=round(cuts[2:5],2),fill=rbPal(4),"right")
#     counter=counter+1
#     counter2=counter2+1
#   }else if(counter2 %in% c(10,15)){
#     frame()
#     counter2=counter2+1
#     #counter=counter+1
#   }else{ ## need to do \n everywhere, need to make a counter, skip over 10 and 15
#     plot(delt_map,main=paste("\n",row.names(ofInterest)[counter+1],sep=""))
#     points(coordsRef$lon,coordsRef$lat,col=col[counter+1,],pch=19,cex=2)
#     legend(legend=round(cuts[2:5],2),fill=rbPal(4),"right")
#     counter=counter+1
#     counter2=counter2+1
#   }
#   print(paste(counter,counter2,sep="_"))
# }
# dev.off()

