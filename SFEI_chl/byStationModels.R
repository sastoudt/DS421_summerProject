### "parsimonious model"

### look promising
## doy 
## date_dec
## pheo
## tn (monthly structure) 

### Add these for "full model"

### of interest
## sio2
## po4
## tss
## nh4

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

## tn v. ton (sparse)
## do v. do_per (add one)
## p04 v. tp 
## add sal to full (interesting)
## check nh4 for missingness
