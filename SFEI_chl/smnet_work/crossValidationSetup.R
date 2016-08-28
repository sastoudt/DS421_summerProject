require(mgcv)
setwd("~/Desktop/sfei")
allData<-read.csv("allData.csv",stringsAsFactors=F)

allData<-allData[-which(allData$Station=="C10"),]
allData<-allData[-which(allData$Station=="D41"),]

allData<-allData[-which(is.na(allData$chl)),]

statID=unique(allData$Station)
statID

nrow(allData)

set.seed(101)
## first just do overall random and see if we get a good distribution of years/stations

.25*nrow(allData)

nTesting=1999
nTraining=nrow(allData)-nTesting
perFold=nTraining/5

forTest=sample(1:nrow(allData),nTesting)

table(allData[forTest,"Station"])
# C3  D10  D12  D19  D22  D26 D28A   D4   D6   D7   D8 MD10   P8 
#153  164  156  107  158  145  144  193  134  156  170  152  167 

## decent

table(year(allData[forTest,"Date"]))
## decent
# 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 
# 60   74  110   89   71   47   67   67   39   57   71   44   58   59   57   51   55   65   40   41 
# 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2015 
# 50   39   31   32   46   36   42   37   36   42   39   46   37   35   28   49   49   50   36   17 

forTraining=setdiff(1:nrow(allData),forTest)

fold1<-sample(forTraining,perFold)
fold2<-sample(setdiff(forTraining,fold1),perFold)
fold3<-sample(setdiff(forTraining,c(fold1,fold2)),perFold)
fold4<-sample(setdiff(forTraining,c(fold1,fold2,fold3)),perFold)
fold5<-sample(setdiff(forTraining,c(fold1,fold2,fold3,fold4)),perFold)

setwd("~/Desktop/sfei/crossValid_smnet")

write.csv(allData[forTest,],"testing.csv",row.names=F)
write.csv(allData[forTraining,],"training.csv",row.names=F)
write.csv(allData[fold1,],"fold1.csv",row.names=F)
write.csv(allData[fold2,],"fold2.csv",row.names=F)
write.csv(allData[fold3,],"fold3.csv",row.names=F)
write.csv(allData[fold4,],"fold4.csv",row.names=F)
write.csv(allData[fold5,],"fold5.csv",row.names=F)

