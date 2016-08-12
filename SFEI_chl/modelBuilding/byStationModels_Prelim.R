setwd("~/Desktop/sfei")
load("perStation.Rda")

## go through each station first do a check of the chosen variables for the model

data=perStation[[41]]
 
## all missing
## 1: bod, cl all missing
## 2: cl, no3,no2, doc,toc all missing
## doc, toc
## bod, cl, doc, toc
## doc, toc, no3, no2
##  doc, toc, no3, no2
# cl, doc, toc, no3, no2
# bod, cl, no3,no2, doc,toc all missing
# cl, no3, no2, doc, toc

#bod, cl, doc, toc,no3,no2
# cl, doc, toc, no3, no2 D2 only has 38 entries
# bod, cl, doc, toc, no3, no2
#bod, cl, doc, toc, no3, no2
#bod, cl, doc, toc, no3, no2
#cl, doc, toc, no3, no2
#cl, doc, toc, no3, no2
#bod, doc, toc, no3, no2
#bod, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2

# cl, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2
## mostly missing
## one entry, mostly missing
## mostly missing
## one entry, mostly missing
#bod,cl, doc, toc, no3, no2
#cl, doc, toc, no3, no2

#bod,cl, doc, toc, no3, no2
# mostly missing
# mostly missing
# mostly missing
# mostly missing
# mostly missing
#bod,cl, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2
#bod,cl, doc, toc, no3, no2
#cl, doc, toc, no3

#bod,cl, doc, toc, no3, no2
View(data)

pairs(data[,c(10,5,6,9)]) 

pairs(data[,c(10,11,15)]) ## tkn

pairs(data[,c(10,18:20)]) ## ton

pairs(data[,c(10,21:25)]) ## do, pheo, tp

pairs(data[,c(10,26:30)])

pairs(data[,c(10,31:37)])

## tn v. ton (ton too sparse)
## do v. do_per (add one to parsimonious model)
## po4 v. tp  (go with tp for context reasons since neither are particular predictive anyway, just interested)
## add sal to full (interesting)
## check nh4 for missingness

sfei<-read.csv("sfeiPlusDates.csv")

sum(is.na(sfei$nh4))/nrow(sfei)

nh4Missing<-c()
for(i in 1:length(perStation)){
  nh4Missing<-c(nh4Missing,sum(is.na(perStation[[i]]$nh4))/nrow(perStation[[i]]))
}
hist(nh4Missing)
length(which(nh4Missing>0.2)) ## 15
summary(nh4Missing)
length(which(nh4Missing>0.31)) ##15
## might be problematic for some stations, but since only in "full" model, just use for those that have
## <20% missing

plot(sfei$po4,sfei$chl)
plot(sfei$tp,sfei$chl)

par(mfrow=c(2,2))
plot(perStation[[10]]$po4,perStation[[10]]$chl)
plot(perStation[[10]]$tp,perStation[[10]]$chl)
plot(perStation[[38]]$po4,perStation[[38]]$chl)
plot(perStation[[38]]$tp,perStation[[38]]$chl)

plot(perStation[[6]]$po4,perStation[[6]]$chl)
plot(perStation[[6]]$tp,perStation[[6]]$chl)
plot(perStation[[16]]$po4,perStation[[16]]$chl)
plot(perStation[[16]]$tp,perStation[[16]]$chl)

test=na.omit(perStation[[10]][,c("po4","chl")])
cor(test$po4,test$chl) ## -0.3260965

test=na.omit(perStation[[10]][,c("tp","chl")])
cor(test$tp,test$chl) ##-0.06399103
 
test=na.omit(perStation[[38]][,c("po4","chl")])
cor(test$po4,test$chl) ##-0.06693277

test=na.omit(perStation[[38]][,c("tp","chl")])
cor(test$tp,test$chl) ## 0.4040624

test=na.omit(perStation[[6]][,c("po4","chl")])
cor(test$po4,test$chl) ##-0.182546

test=na.omit(perStation[[6]][,c("tp","chl")])
cor(test$tp,test$chl) ## 0.1203387

test=na.omit(perStation[[16]][,c("po4","chl")])
cor(test$po4,test$chl) ##-0.2398092

test=na.omit(perStation[[16]][,c("tp","chl")])
cor(test$tp,test$chl) ##  0.03125155

###
plot(sfei$do,sfei$chl)
plot(sfei$do_per,sfei$chl)

par(mfrow=c(2,2))
plot(perStation[[10]]$do,perStation[[10]]$chl)
plot(perStation[[10]]$do_per,perStation[[10]]$chl)
plot(perStation[[38]]$do,perStation[[38]]$chl)
plot(perStation[[38]]$do_per,perStation[[38]]$chl)

plot(perStation[[6]]$do,perStation[[6]]$chl)
plot(perStation[[6]]$do_per,perStation[[6]]$chl)
plot(perStation[[16]]$do,perStation[[16]]$chl)
plot(perStation[[16]]$do_per,perStation[[16]]$chl)

test=na.omit(perStation[[10]][,c("do","chl")])
cor(test$do,test$chl) ## 0.1369511

test=na.omit(perStation[[10]][,c("do_per","chl")])
cor(test$do_per,test$chl) ##0.6288873

test=na.omit(perStation[[38]][,c("do","chl")])
cor(test$do,test$chl) ##0.2520524

test=na.omit(perStation[[38]][,c("do_per","chl")])
cor(test$do_per,test$chl) ##  0.3901313

test=na.omit(perStation[[6]][,c("do","chl")])
cor(test$do,test$chl) ## 0.04331098

test=na.omit(perStation[[6]][,c("do_per","chl")])
cor(test$do_per,test$chl) ##0.4732619

test=na.omit(perStation[[16]][,c("do","chl")])
cor(test$do,test$chl) ## 0.1592326

test=na.omit(perStation[[16]][,c("do_per","chl")])
cor(test$do_per,test$chl) ##  0.5434788

## definitely add do_per to "parsimonious" model


### "parsimonious model"

### look promising
## doy 
## date_dec
## pheo
## tn (monthly structure) 
## do_per (check context of the "per")

### Add these for "full model"

### of interest
## sio2
## tp
## tss
## nh4 (when data available)




