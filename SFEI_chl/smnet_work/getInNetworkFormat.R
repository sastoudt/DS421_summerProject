
## figure out network

setwd("~/Desktop/DS421_summerProject/GAM_weightedReg_comparison/shiny/data")

load("delt_map.RData")

setwd("~/Desktop/sfei")

allData<-read.csv("allData.csv",stringsAsFactors=F)

pdf("deltMapWLoc.pdf",height=10,width=10)
plot(delt_map)
text(allData$Longitude,allData$Latitude,allData$Station,pch=19)
dev.off()

names(attributes(delt_map))
delt_map

##         min        max
#x -122.17935 -121.27622
#y   37.76008   38.82315
 
range(allData$Longitude) ##-122.3729 -121.2647
range(allData$Latitude) ## 37.67934 38.36771

plot(allData$Longitude,allData$Latitude,pch=19)
## bottom right ## C10
## furthest left ## D41
## just keep these out for now, too hard to tell adjacency by eye 

unique(allData$Station)

row.names=col.names=unique(allData$Station)[-c(1,10)]

## don't think the adjacency matrix has to be symmetric, just put in flow direction

sfeiAdjMatrix<-matrix(0,nrow=13,ncol=13)
sfeiAdjMatrix[1,5]=1 ## C3 --> D22
sfeiAdjMatrix[2,11]=1 ## D10 --> D8
sfeiAdjMatrix[3,2]=1 ## D12 --> D10
sfeiAdjMatrix[4,3]=1 ## D19 --> D12
sfeiAdjMatrix[5,8]=1 ## D22 --> D4
sfeiAdjMatrix[6,4]=1 ## D26 --> D19
sfeiAdjMatrix[7,4]=1 ## D28A --> D19
sfeiAdjMatrix[8,2]=1 ## D4 --> D10
sfeiAdjMatrix[11,9]=1 ## D8 --> D6
sfeiAdjMatrix[11,10]=1 ## D8 --> D7
sfeiAdjMatrix[12,6]=1 ## MD10 --> D28A
sfeiAdjMatrix[13,6]=1 ## P8 --> D28A

row.names(sfeiAdjMatrix)=row.names
colnames(sfeiAdjMatrix)=col.names
View(sfeiAdjMatrix)

library(Matrix)

sfeiAdjMatrix_sp <- Matrix(sfeiAdjMatrix, sparse = TRUE) 

source("~/Desktop/DS421_summerProject/SFEI_chl/toSource.R")

make_weights(sfeiAdjMatrix_sp) ## in right format

bid=c("111011","111","1111","11111","11101","111110","111111","1110","1","10","11","1111101","1111101")


adjacency = sfeiAdjMatrix_sp
#adjacency_to_shreve <- function(adjacency){
  trips        <- triplet(adjacency)$indices
  shreve.order <- inverse.order <- vector("numeric", length = nrow(adjacency))

  #   INVERSE ORDER PROVIDES THE NUMBER OF STREAM SEGMENTS
  #   EACH SEGMENTS LIES UPSTREAM FROM THE SOURCE
  # the root node is the segment that has no downstream neighbours, ie. the outlet
  root.node    <- which(rowSums(adjacency) == 0)
  if(length(root.node) > 1){
    #bid_roots <- nchar(adjacency$rid_bid[root.node, 2]) ## which one has more going into it
    root.node <- root.node[1]
    
   # bid_roots <- nchar(bid)
  #  root.node <- root.node[which(bid_roots == max(bid_roots))]
  }
  # inverse.order is a vector that accumulates from root.node upstream
  # each element indicates the 'height' in the network of each segment
  inverse.order[root.node] <- 1 
  # every element of inverse.order should be non-zero.  
  # A zero might indicate a lake or something disconnected from the network
  # to get the inverse.order, siply count the number of characters in the binaryID vector
  #inverse.order <- nchar(adjacency$rid_bid[, 2])
  
  ## BIDs http://www.fs.fed.us/rm/boise/AWAE/projects/SSN_STARS/downloads/STARS/STARS_tutorial_v204.pdf
  ## pg 41
  
  #row.names
  

  inverse.order<-nchar(bid)
  #[1] 6 3 4 5 5 6 6 4 1 2 2 7 7
  
  for(i in 1:nrow(trips)) inverse.order[trips[i,1]] <- inverse.order[trips[i,2]] + 1
  # sources is a vector indicating the segments that have no upstream neighbours, ie the sources of the stream
  sources <- which(colSums(adjacency) == 0)
  # shreve.order assigns a weight of 1 to these source segments
  shreve.order[sources] <- 1
  # remaining gives the network 'heights' of the non-source segments that remain
  # in reverse order, since shreve.order will accumlate from the highest to the lowest in the network
  remaining <- rev(sort(unique(inverse.order[-sources])))
  # we now ignore the sources and simply populate the remaining shreve.orders
  inverse.order[sources] <- NA
  # need the row locations of 1's in adjacency
  # since spam uses column compression, must transpose to extract these efficiently
  tadjacency <- t(adjacency)
  # populate elements of shreve.order in reverse order of network 'height' (larger number means 'higher')
  for(i in 1:length(remaining)){
    # identify all those segments with network height == remaining[i]
    next.down <- which(inverse.order == remaining[i])
    # cycle through each of next.down in turn and add up their upstream weights
    # this makes sense because every segment with the same network height has weight indep. of the others
    for(j in 1:length(next.down)){
      #shreve.order[next.down[j]] <- sum(shreve.order[tadjacency[next.down[j],]@colindices])
      shreve.order[next.down[j]] <- sum(shreve.order[unname(which(tadjacency[next.down[j],]==1))])
      
    }
  }
  # finally print the shreve.order
  shreve.order
## this can't be right
## https://en.wikipedia.org/wiki/Strahler_number
  
  ## http://pro.arcgis.com/en/pro-app/tool-reference/spatial-analyst/how-stream-order-works.htm
  ## was doing strahler not shreve
  ## still doesn't help the fact that nbin is off

  ### do by hand
  
  # strahler.order<-c(1,2,2,2,1,2,1,1,1,1,1,2,1)
  # shreve.order<-c(1,4,3,3,1,2,1,1,4,4,4,1,1)

#weight       <- adjacency_to_shreve(adjacency = sfeiAdjMatrix_sp)

  shreve.order<-c(1,4,3,3,1,2,1,1,5,1,4,1,1)
  
  bid=c("111011","111","1111","11111","11101","111110","111111","1110","1","10","11","1111101","1111101")
  
  wgt<-c(1,4,3/4,3,1,2/3,1/3,1/4,5,1/5,4/5,1/2,1/2)
  
  wgt=shreve.order
  wgt
  adj=adjacency
    wgt <- try(as.numeric(as.matrix(wgt)))
    if(class(wgt) == "numeric" & (!anyNA(wgt))){
      # pull out the bid from the adjacency object
      #bid  <- adj$rid_bid[,2]
      nbid <- nchar(bid) 
      
      #inverse.order<-c(5,2,3,4,4,5,5,3,1,1,2,6,6)
      #nbid=7-inverse.order
      ## this makes the most sense and gets closest to zero but still doesn't match
      #nbid[c(1,2,5,7,8)]=nbid[c(1,2,5,7,8)]-1
      
      #nbid=c(0,3,3,2,1,1,0,2,5,5,4,0,0)
      #nbid=nbid+1
      min_weight <- min(wgt)
      # the idea here is that for a network weight, the sum of the weights at the i^th 
      # level of the network heirarchy should equal the number of segments in the i-1^th level
      # of the network, minus the number of 'source' segments (dead_ends) that occur at i-1^th level
      # for an additive weight, the idea is similar, the sum of the weights a the i^th level
      # equal the sum of the weights at the i-1^th level, minus the number of dead_ends at i-1
      # since all dead_ends will have order 1 (under Shreve - if the lowest order is different, I'll need to fix this)
      dis_additive     <- 0
      dis_network      <- 0
      nl=c()
      suw=c()
      de=c()
      sdw=c()
      for(i in 2:max(nbid)){
        which_lower     <- which(nbid == i-1)
        which_upper     <- which(nbid == i)
        n_lower         <- length(which_lower)
        sum_up_weight   <- sum(wgt[which_upper])
        sum_dn_weight   <- sum(wgt[which_lower])
        dead_ends       <- sum(colSums(adj)[which_lower] == 0)
        dis_network     <- dis_network + (n_lower - sum_up_weight - dead_ends)^2
        dis_additive    <- dis_additive + (sum_dn_weight - sum_up_weight - dead_ends*min_weight)^2
      nl=c(nl,n_lower)
      suw=c(suw,sum_up_weight)
      de=c(de,dead_ends)
      sdw=c(sdw,sum_dn_weight)
        }
      nl
      suw
      de
      sdw
      
      (nl-suw-de)^2
      (sdw-suw-de*min_weight)^2
      
      if(round(dis_additive, 6) == 0){
        weight.type <- "additive"
        if(!silent) cat("Provided weight passes additivity check... \n")
      } else if(round(dis_network, 6)  == 0){
        weight.type <- "network"
        if(!silent) cat("Provided weight passes network check... \n")
      } else {weight.type <- "unrecognised"}
    } else {
      weight.type <- "unrecognised"
    }
    weight.type
  
    ##shreve.order 
    dis_additive ## 1
    dis_network ## 38
    ## wgts
    dis_additive ## 47.36
    dis_network ## 8
    
    
    ## unrecognized
  ## can't go forward with smnet call without figuring this out :(
 # weight.type  <- check_weight(adjacency, shreve.order)
  
  ## found where the issue is
  ## need to eliminate MD & P level to fix, see if this works
  
#   sfeiAdjMatrix=sfeiAdjMatrix[-c(12,13),]
# sfeiAdjMatrix=sfeiAdjMatrix[,-c(12,13)]
# 
# sfeiAdjMatrix
# sfeiAdjMatrix_sp <- Matrix(sfeiAdjMatrix, sparse = TRUE) 
# shreve.order=c(1,3,2,2,1,1,1,1,3,3,3)
# 
# nbid=c()



####

## ok even though we can't pass the weight check, can still get spatial penalty, can we move forward?
adjacency
wgts=shreve.order
n.segments=nrow(adjacency)
lambda=1

##check_additive_weight
## only gives a warning if not zero
## we have 2.615385 instead of 0
## Additivity of selected weight variable doesn't seem to hold. Proceed with caution.


####spatial_penalty is D matrix
spatial_penalty<-function(adjacency, wgts, lambda, n.segments){
  adj.spam <- make_spam(adjacency)
  pseudo.inds  <- which(colSums.spam(adj.spam) == 1)
  ij.nzero.adj <- triplet(adj.spam)$indices
  in.pseudo    <- ij.nzero.adj[,2] %in% pseudo.inds
  ij.confl     <- ij.nzero.adj[!in.pseudo,]
  n.nzero      <- nrow(ij.confl)
  p.row.ind    <- rep(1:n.nzero, each = 2)
  p.col.ind    <- c(t(ij.confl))
  p.val        <- wgts[rep(ij.confl[,1], each = 2)]*rep(c(-1, 1), n.nzero)
  D2           <- spam(list(i=p.row.ind, j=p.col.ind, p.val), nrow = n.nzero, ncol = n.segments)
  D2           <- t(D2)%*%D2 
  
  if(!is.null(pseudo.inds)){
    ij.pseudo    <- ij.nzero.adj[in.pseudo,]
    n.nzero      <- nrow(ij.pseudo)
    if(is.null(n.nzero)) n.nzero <- 1
    p.row.ind    <- rep(1:n.nzero, each = 2)
    p.col.ind    <- c(t(ij.pseudo))
    if(is.matrix(ij.pseudo)){
      p.val        <- wgts[rep(ij.pseudo[,1], each = 2)]*rep(c(-1, 1), n.nzero)
    }
    if(is.vector(ij.pseudo)){
      p.val        <- wgts[rep(ij.pseudo[1], each = 2)]*rep(c(-1, 1), n.nzero)
    }
    D1           <- spam(list(i=p.row.ind, j=p.col.ind, p.val), nrow = n.nzero, ncol = n.segments)
    D1            <- t(D1)%*%D1
  }  
  return((lambda)*(D2 + D1))  
}

D=spatial_penalty(adjacency,wgts,lambda,n.segments)

weight=shreve.order
#check_additive_weight <- function(adjacency, weight){
  # transpose the adjacency so that the rows are accessible (as cols in the transpose)
  tadjacency <- t(adjacency)
  # create binary numeric vector, i^th element is a check that weights immediately 
  # upstream to i sum exactly to the weight of i
  is.additive <- vector("numeric", length = nrow(adjacency))
  # create a numeric vector that measures the discrepancy between upstream and downstream weights
  discrep <- vector("numeric", length = nrow(adjacency))
  # loop through the rows of the adjacency matrix, performing the sum  check for each
  for(i in 1:nrow(adjacency)){
    dn.weight <- weight[i]
    col.inds  <- tadjacency[i,]
    if(!sum(col.inds) == 0){
      up.weight <- sum(weight[col.inds])
      is.additive[i] <- as.numeric(!dn.weight == up.weight)
      discrep[i] <- sum(dn.weight - up.weight)^2
    } else {
      is.additive[i] <- 0
    }
  }
  # it is possible that the additivity condition holds but is subject to round off and other small errors
  # therefore only print a warning if the rmse reaches a sensible tolerance
  if((mean(is.additive)) > 0 & (mean(discrep) > 10^-5)){
    rmse <- mean(discrep)
    commt <- paste("Additivity of selected weight variable doesn't seem to hold, \nRMSE = ", rmse, 
                   " was observed. \nProceed with caution.", sep = "")
    warning(commt)
  }

