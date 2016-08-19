require(SSN)
require(smnet)

example_network<-createSSN(n=50, obsDesign=binomialDesign(200),predDesign=binomialDesign(50),
                           importToR=T,path = paste(tempdir(),"/example_network",sep = ""),
                           treeFunction = iterativeTreeLayout)

plot(example_network,bty="n",xlab = "x-coord", ylab = "y-coord")

class(example_network)
names(attributes(example_network))

example_network@network.line.coords
## network ID, segmentID, distance upstream

example_network@obspoints

example_network@path ## path to shapefiles etc.
example_network@data
## SL_ID, rid, netID, upDist, shreve, Length, addfunccol


get_binaryIDs<-function(path, net = 1){
  # connect to binaryID.db to import vector of binaryIDs
  driver<-dbDriver("SQLite")
  # open a connection to the binary ID table
  connect<-try(dbConnect(SQLite(), paste(path, "/binaryID.db", sep = "")), silent = TRUE)
  if(class(connect) == "try-error"){
    dbUnloadDriver(driver)
    stop("Unable to open a connection to the binary table - directory could be incorrect")
  }
  # List all database tables - there should be one for each network
  networks<-dbListTables(connect)
  IDTable <- try(dbGetQuery(connect, statement = paste("select * from net", net, sep = "")),
                 silent = TRUE)
  if(class(IDTable) == "try-error"){
    dbDisconnect(connect)
    dbUnloadDriver(driver)
    nnets<-length(networks)
    stop(paste("Error finding the network number specified, number should be between 1 and ", nnets, sep = ""))
  }
  rids<-IDTable[,1]
  IDTable<-IDTable[order(rids),]
  rids.sort<-rids[order(rids)]
  
  ### Must close SQLite Driver
  dbDisconnect(connect)
  dbUnloadDriver(driver)
  IDTable
}

re_map_rid <- function(rid_vector, all_rid){
  # in cases where multiple networks are present, gaps could occur in the rid sequence
  # this function maps the rid vector in a simple way so that it agrees with the adjacency matrix
  mapped_rid <- vector("numeric", length = length(rid_vector))
  rng_rid    <- range(all_rid)
  new_key <- 1:length(unique(all_rid))
  old_rid <- sort(all_rid)
  for(i in 1:length(rid_vector)){
    mapped_rid[i] <- new_key[which(old_rid == rid_vector[i])]
  }
  mapped_rid
}


make_weights<-function(adjacency, type = "unif"){
  adjacency<-as.matrix(adjacency)
  nrowAdj<-nrow(adjacency)
  weights<-vector("numeric", length = nrowAdj)
  if(type == "unif"){
    for(i in 1:nrowAdj){
      inds<-which(adjacency[,i] == 1)
      if(length(inds) == 1) weights[inds] <- 1
      if(length(inds) == 2){
        weights[inds[1]]<-runif(1)
        weights[inds[2]]<-1-weights[inds[1]]
      }
    }
  }
  return(weights)
}


get_adjacency<-function(ssn_directory, netID = 1){
  binaryIDs  <- get_binaryIDs(ssn_directory, net = netID)
  bid        <- binaryIDs[,2]
  rid        <- binaryIDs[,1]
  nch        <- nchar(bid)
  bid.list   <- split(bid, nch)
  rid.list   <- split(rid, nch)
  n.segments <- nrow(binaryIDs)
  xy         <- matrix(ncol = 2, nrow = (n.segments - 1))
  counter    <- 0
  for(j in 1:(length(bid.list)-1)){
    bid.dn  <- bid.list[[j]]
    bid.up  <- bid.list[[j+1]]
    rid.dn  <- rid.list[[j]]
    rid.up  <- rid.list[[j+1]]
    bid.up.sub.vec <- bid.up
    rid.up.sub.vec <- rid.up
    for(i in 1:length(bid.dn)){
      current.dn.bid<-bid.dn[i]
      current.dn.rid<-rid.dn[i]
      inner.count<-1
      number.upstream<-0
      n.bid.up<-length(bid.up.sub.vec)
      crit<-F
      while(!crit){	
        if(n.bid.up>0){
          current.up.bid<-bid.up.sub.vec[inner.count]
          connected<-substr(current.up.bid, 1, nchar(current.dn.bid)) == current.dn.bid
          if(connected){
            counter<-counter+1
            number.upstream<-number.upstream+1
            xy[counter,]<-c(current.dn.rid, rid.up.sub.vec[inner.count])
            rid.up.sub.vec<-rid.up.sub.vec[-inner.count]
            bid.up.sub.vec<-bid.up.sub.vec[-inner.count]
          }
          if(!connected) inner.count<-inner.count+1
          crit<-(number.upstream == 2)|((number.upstream + inner.count - 1) == n.bid.up)
        }
        if(n.bid.up==0) crit<-T
      }
    }		
  }	
  #xy<-xy[-which(is.na(xy), arr.ind = T)[,1],]
  xy[,1]  <- re_map_rid(rid_vector = xy[,1], all_rid = rid)
  xy[,2]  <- re_map_rid(rid_vector = xy[,2], all_rid = rid)
  add.one <- min(xy) == 0
  list(adjacency = spam(list(j = (xy[,1]+add.one), i = (xy[,2]+add.one), rep(1, nrow(xy))), 
                        nrow = n.segments, ncol=n.segments), rid_bid = cbind(rid, bid))
}	


adjacency=get_adjacency(example_network@path,1)

make_weights(adjacency[[1]])
class(adjacency)

adjacency <- get_adjacency(
  paste(tempdir(), "/example_network", sep = ""),
  net = 1
)

display(adjacency[[1]])
class(adjacency[[1]])
example_network<- createSSN(
  n = 50,
  obsDesign = binomialDesign(200),
  predDesign = binomialDesign(50),
  importToR = TRUE,
  path = paste(tempdir(),"/example_network",sep = ""),
  treeFunction = iterativeTreeLayout
)

observed_data <- getSSNdata.frame(example_network, "Obs")
prediction_data <- getSSNdata.frame(example_network, "preds")
## associate continuous covariates with the observation locations
# data generated from a normal distribution
obs <- rnorm(200)
observed_data[,"X"] <- obs
observed_data[,"X2"] <- obs^2
## associate continuous covariates with the prediction locations
# data generated from a normal distribution
pred <- rnorm(50)
prediction_data[,"X"] <- pred
prediction_data[,"X2"] <- pred^2
createDistMat(example_network, "preds", o.write=TRUE, amongpred = TRUE)
sims <- SimulateOnSSN(
  ssn.object = example_network,
  ObsSimDF = observed_data,
  PredSimDF = prediction_data,
  PredID = "preds",
  formula = ~ 1 + X,
  coefficients = c(1, 10),
  CorModels = c("Exponential.tailup"),
  use.nugget = TRUE,
  CorParms = c(10, 5, 0.1),
  addfunccol = "addfunccol")$ssn.object
## extract the observed and predicted data frames, now with simulated values
sim1DFpred <- getSSNdata.frame(sims, "preds")
sim1preds <- sim1DFpred[,"Sim_Values"]
sim1DFpred[,"Sim_Values"] <- NA
sims <- putSSNdata.frame(sim1DFpred, sims, "preds")

show_weights(sims, adjacency)

unifWeights=make_weights(adjacency[[1]])
unifWeights
wgts=unifWeights

make_spam<-function(M){
  if(class(M) == "matrix") as.spam(M)
  if(class(M) == "spam") as.spam(M)
  else as.spam(as.spam.dgCMatrix(as(M, "dgCMatrix")))
}

n.segments=50
adj.spam <- make_spam(adjacency[[1]])
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

require(Matrix)

View(as.data.frame(as.matrix(D2)))
